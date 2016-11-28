package carisma.ui.vision.io.implementations.db.mongodb.restapi;

import static carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBConstants.*;
import static carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBResponseMessage.Operation.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.Scanner;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import carisma.core.io.content.Content;
import carisma.core.io.content.ContentFactory;
import carisma.core.io.content.ContentFactory.ContentFormats;
import carisma.core.io.content.JSON;
import carisma.core.io.implementations.db.DataBaseIO;
import carisma.core.io.implementations.db.ResponseMessage;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBConfiguration.WriteAction;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBConfiguration.ReadAction;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBConfiguration.DeleteAction;

public class MongoDBRestAPI implements DataBaseIO {
	
	private static final ContentFormats F_JSON = ContentFormats.F_JSON;
	
	private HttpClient httpClient;
	Content contentObject;
	private ResponseMessage response;
	
	private RestAPI api;
	
	
	public MongoDBRestAPI(final String user, final String secret, final String url) {
		if (user == null || user.trim() == "") {
			throw new RuntimeException("The value of the parameter \"user\" is null or empty.");
		}
		if (secret == null || secret.trim() == "") {
			throw new RuntimeException("The value of the parameter \"secret\" is null or empty.");
		}
		if (url == null || url.trim() == "") {
			throw new RuntimeException("The value of the parameter \"url\" is null or empty.");
		}
		
		HttpClientBuilder create = HttpClientBuilder.create();
		this.httpClient =  create.build();
		
		this.api  = new RestAPI(user, secret, url);
	}
	
	/*
	 * Interface methods for reading and writing
	 * 
	 * (non-Javadoc)
	 * @see carisma.core.io.implementations.db.DataBaseIO#write(carisma.core.io.configuration.Configuration, carisma.core.io.content.Content)
	 */
	
	@Override
	public final boolean write(final Destination config, final Content content) {
		if (config instanceof MongoDBDestination) {
			MongoDBDestination mongoConf = (MongoDBDestination) config;
			
			String body;
			String contentAsString = content.asString();
			if (content.getFormat().compareTo(JSON.ID) == 0) {
				body = contentAsString;
			} else {
				JSON json = ContentFactory.convertToJson(content);
				body = json.asString();
			}
			
			
			String collectionID = mongoConf.getCollectionID();
			String documentID = mongoConf.getDocumentID();
			String fieldID = mongoConf.getFieldID();
			
			boolean hasCollectionID = collectionID != null && collectionID.trim().length() != 0;
			if (!hasCollectionID) {
				throw new RuntimeException("No CollectionID is given");
			}
			boolean hasDocumentID = documentID != null && documentID.trim().length() != 0;
			boolean hasFieldID = fieldID != null && fieldID.trim().length() != 0;
			if (!hasDocumentID && !hasFieldID) {
				throw new RuntimeException("No DocumentID and no FieldID is given");
			}
			
			this.response = null;
			if (hasCollectionID) {
				if (hasDocumentID) {
					if (hasFieldID) {
						this.response = this.api.getField(collectionID, documentID, fieldID);
					} else {
						this.response = this.api.getDocument(collectionID, documentID);
					}
				}
			}
			
			if (this.response != null && this.response.getStatus() == 200) {
				if (hasDocumentID) {
					if (hasFieldID) {
						String documentBody = "{\"" + fieldID + "\":'" + body + "'}";
						this.response = this.api.putField(collectionID, documentID, fieldID, documentBody);
					} else {
						this.api.deleteDocument(collectionID, documentID);
						this.response = this.api.postDocument(collectionID, documentID, body);
					}
				}
			} else {
				if (hasDocumentID) {
					if (hasFieldID) {
						this.response = this.api.getDocument(collectionID, documentID);
						if (this.response.getStatus() == 404) {
							return false;
						}
						String documentBody = "{\"" + fieldID + "\":'" + body + "'}";
						this.response = this.api.postField(collectionID, documentID, fieldID, documentBody);
					} else {
						this.response = this.api.postDocument(collectionID, documentID, body);
					}
				}
			}
			
			return this.response.getStatus() == 201 || this.response.getStatus() == 409;
			
		}
		return false;
	}

	@Override
	public final Content read(final Destination config) {
		if (config instanceof MongoDBDestination) {
			MongoDBDestination mongoConf = (MongoDBDestination) config;
			
			String collectionID = mongoConf.getCollectionID();
			String documentID = mongoConf.getDocumentID();
			String fieldID = mongoConf.getFieldID();
			
			boolean hasCollectionID = collectionID != null && collectionID.trim().length() != 0;
			if (!hasCollectionID) {
				throw new RuntimeException("No CollectionID is given");
			}
			boolean hasDocumentID = documentID != null && documentID.trim().length() != 0;
			boolean hasFieldID = fieldID != null && fieldID.trim().length() != 0;
			if (!hasDocumentID && !hasFieldID) {
				throw new RuntimeException("No DocumentID and no FieldID is given");
			}
			
			this.response = null;
			if (hasCollectionID) {
				if (hasDocumentID) {
					if (hasFieldID) {
						this.response = this.api.getField(mongoConf.getCollectionID(), mongoConf.getDocumentID(), mongoConf.getFieldID());
					} else {
						this.response = this.api.getDocument(mongoConf.getCollectionID(), mongoConf.getDocumentID());
					}
				}
			}
			if (this.response.getStatus() == 200) {
				return this.contentObject;
			}
			
		}
		return null;
	}
	
	@Override
	public final ResponseMessage getResponseMessage() {
		return this.response;
	}

	/*
	 * Helper methods
	 */

	HttpResponse httpExecute(final HttpRequestBase httpRequest) {
		HttpResponse httpResponse = null;
		
		try {
			httpResponse = this.httpClient.execute(httpRequest);
		} catch (ClientProtocolException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (IllegalStateException e) {
			e.printStackTrace();
		}
		return httpResponse;
	}
	
	/**
	 * Creates a Content object from the given HttpResponse.
	 * 
	 * @param httpResponse The Httpresponse
	 * @return a Content object
	 */
	static Content createcontentFromHttpResponse(final HttpResponse httpResponse) {
		HttpEntity entity = httpResponse.getEntity();
		InputStream inputStream;
		
		try {
			inputStream = entity.getContent();
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
		
		try( 
			InputStreamReader reader = new InputStreamReader(inputStream);
			Scanner scanner = new Scanner(reader);
		) {
			String string = scanner.useDelimiter("\\A").next();
			return ContentFactory.createContent(string, F_JSON);
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	
	public static class MongoDBDestination implements Destination {

		private final String COLLECTION;
		private final String DOCUMENT;
		private final String FIELD;

		public MongoDBDestination(String carismaCollection, String carismaDocument, String carismaField) {
			this.COLLECTION = carismaCollection;
			this.DOCUMENT = carismaDocument;
			this.FIELD = carismaField;
		}

		public final String getFieldID() {
			return this.FIELD;
		}

		public final String getDocumentID() {
			return this.DOCUMENT;
		}

		public final String getCollectionID() {
			return this.COLLECTION;
		}
		
	}
	
	/**
	 * A wrapper for the swagger RestAPI of the VisiOn DB.
	 * @author speldszus
	 *
	 */
	private class RestAPI {
		/*
		 * Private implementation of RestAPI
		 */

		/**
		 * A configuration with the basic server data.
		 */
		private MongoDBConfiguration config;

		public RestAPI(String user, String secret, String url) {
			this.config = new MongoDBConfiguration(user, secret, url);
		}

		ResponseMessage deleteDocument(final String collectionID, final String documentID) {
			URI url = this.config.buildUrl(new DeleteAction(collectionID, documentID, null));
			HttpDelete	request = new HttpDelete(url);
						request.addHeader(KEYWORD_AUTHORIZATION, this.config.buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);
				
			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, delete);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		ResponseMessage getDocument(final String collectionID, final String documentID) {
			URI url = this.config.buildUrl(new ReadAction(collectionID, documentID, null));
			HttpGet 	request = new HttpGet(url);
						request.addHeader(KEYWORD_AUTHORIZATION, this.config.buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);
			
			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				if (statusCode == 200) {
					MongoDBRestAPI.this.contentObject = createcontentFromHttpResponse(httpResponse);
				}
				
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, get);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		ResponseMessage postDocument(final String collectionID, final String documentID, final String documentBody) {
			URI url = this.config.buildUrl(new WriteAction(collectionID, documentID, null));
			HttpPost	request = new HttpPost(url);
						request.addHeader(KEYWORD_AUTHORIZATION, this.config.buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);
			
			StringEntity params;
			try {
				params = new StringEntity(documentBody);
				params.setContentType(APPLICATION_JSON);
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				throw new RuntimeException("Body encoding is not supported");
			}
					
			request.setEntity(params);
			
			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, post);
			}
			throw new RuntimeException("HttpRespone is null");	
		}
		
		@SuppressWarnings("unused")
		private ResponseMessage putDocument(final String collectionID, final String documentID, final String documentBody) {
			URI url = this.config.buildUrl(new WriteAction(collectionID, documentID, null));
			HttpPut	request = new HttpPut(url);
						request.addHeader(KEYWORD_AUTHORIZATION, this.config.buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);
						
			StringEntity params;
			try {
				params = new StringEntity(documentBody);
				params.setContentType(APPLICATION_JSON);
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				throw new RuntimeException("Body encoding is not supported");
			}
								
			request.setEntity(params);
						
			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, put);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		@SuppressWarnings("unused")
		private ResponseMessage deleteField(final String collectionID, final String documentID, final String fieldID) {
			URI url = this.config.buildUrl(new DeleteAction (collectionID, documentID, fieldID));
			HttpDelete	request = new HttpDelete(url);
						request.addHeader(KEYWORD_AUTHORIZATION, this.config.buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);

			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, delete);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		ResponseMessage getField(final String collectionID, final String documentID, final String fieldID) {
			URI url = this.config.buildUrl(new ReadAction(collectionID, documentID, fieldID));
			HttpGet 	request = new HttpGet(url);
						request.addHeader(KEYWORD_AUTHORIZATION, this.config.buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);

			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				if (statusCode == 200) {
					Content responseContent = createcontentFromHttpResponse(httpResponse);
					JSON json = ContentFactory.convertToJson(responseContent);
					Object field = json.get(fieldID);
					if (field instanceof String) {
						String string = (String) field;
						
						MongoDBRestAPI.this.contentObject = ContentFactory.createContent(string);
					}
				}
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, get);
			}
			throw new RuntimeException("HttpRespone is null");
		}

		ResponseMessage postField(final String collectionID, final String documentID, final String fieldID, final String documentBody) {
			HttpPost	request = new HttpPost(this.config.buildUrl(new WriteAction(collectionID, documentID, fieldID)));
						request.addHeader(KEYWORD_AUTHORIZATION, this.config.buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);
						
			StringEntity params;
			try {
				params = new StringEntity(documentBody);
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				throw new RuntimeException("Body encoding is not supported");
			}
						
			request.setEntity(params);
			
			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, post);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		ResponseMessage putField(final String collectionID, final String documentID, final String fieldID, final String documentBody) {
			URI url = this.config.buildUrl(new WriteAction(collectionID, documentID, fieldID));
			HttpPut	request = new HttpPut(url);
					request.addHeader(KEYWORD_AUTHORIZATION, this.config.buildAuth());
					request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
					request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);
					
			StringEntity params;
			try {
				params = new StringEntity(documentBody);
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				throw new RuntimeException("Body encoding is not supported");
			}
					
			request.setEntity(params);
			
			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, put);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		@SuppressWarnings("unused")
		private ResponseMessage getDocuments(final String collectionID) {
			URI url = this.config.buildUrl(new ReadAction(collectionID, null, null));
			HttpGet 	request = new HttpGet(url);
			request.addHeader(KEYWORD_AUTHORIZATION, this.config.buildAuth());
			request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);

			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
			MongoDBRestAPI.this.contentObject = createcontentFromHttpResponse(httpResponse);
				
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, get);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		@SuppressWarnings("unused")
		private ResponseMessage getForAllDocuments(final String collectionID, final String fieldID) {		
			URI url = this.config.buildUrl(new ReadAction(collectionID, null, fieldID));
			HttpGet 	request = new HttpGet(url);
						request.addHeader(KEYWORD_AUTHORIZATION, this.config.buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						
			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				MongoDBRestAPI.this.contentObject = createcontentFromHttpResponse(httpResponse);
				
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, get);
			}
			throw new RuntimeException("HttpRespone is null");
		}
	}
}
