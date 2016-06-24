package carisma.core.io.implementations.db.mongodb.restapi;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.Base64;
import java.util.Scanner;

import org.apache.commons.lang3.StringEscapeUtils;
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
import carisma.core.io.configuration.Configuration;
import carisma.core.io.content.Content;
import carisma.core.io.content.ContentFactory;
import carisma.core.io.content.ContentFactory.ContentFormats;
import carisma.core.io.content.JSON;
import carisma.core.io.content.PLAIN;
import carisma.core.io.implementations.db.DataBaseIO;
import carisma.core.io.implementations.db.ResponseMessage;

import static carisma.core.io.implementations.db.mongodb.restapi.MongoDBResponseMessage.Operation.*;
import static carisma.core.io.implementations.db.mongodb.restapi.MongoDBConstants.*;

public class MongoDBRestAPI implements DataBaseIO {
	
	private static final ContentFormats F_JSON = ContentFormats.F_JSON;
	private final String SECRET;
	private final String USER;
	
	private HttpClient httpClient = HttpClientBuilder.create().build();
	private Content contentObject;
	private ResponseMessage response;
	
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
		USER = user;
		SECRET = secret;
	}
	
	/*
	 * Interface methods for reading and writing
	 * 
	 * (non-Javadoc)
	 * @see carisma.core.io.implementations.db.DataBaseIO#write(carisma.core.io.configuration.Configuration, carisma.core.io.content.Content)
	 */
	
	@Override
	public final boolean write(final Configuration config, final Content content) {
		if (config instanceof MongoDBDynamicConfiguration) {
			MongoDBDynamicConfiguration mongoConf = (MongoDBDynamicConfiguration) config;
			RestAPI api = new RestAPI(mongoConf);
			
			String body;
			String contentAsString = content.asString();
			if (content.getFormat().compareTo(JSON.ID) == 0) {
				body = contentAsString;
			} else if (content.getFormat().compareTo(PLAIN.ID) == 0) {
				body = StringEscapeUtils.escapeJson(contentAsString);
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
			
			response = null;
			if (hasCollectionID) {
				if (hasDocumentID) {
					if (hasFieldID) {
						response = api.getField(collectionID, documentID, fieldID);
					} else {
						response = api.getDocument(collectionID, documentID);
					}
				}
			}
			
			if (response != null && response.getStatus() == 200) {
				if (hasDocumentID) {
					if (hasFieldID) {
						String documentBody = "{\"" + fieldID + "\":'" + body + "'}";
						response = api.putField(collectionID, documentID, fieldID, documentBody);
					} else {
						api.deleteDocument(collectionID, documentID);
						response = api.postDocument(collectionID, documentID, body);
					}
				}
			} else {
				if (hasDocumentID) {
					if (hasFieldID) {
						response = api.getDocument(collectionID, documentID);
						if (response.getStatus() == 404) {
							return false;
						}
						String documentBody = "{\"" + fieldID + "\":'" + body + "'}";
						response = api.postField(collectionID, documentID, fieldID, documentBody);
					} else {
						response = api.postDocument(collectionID, documentID, body);
					}
				}
			}
			
			return response.getStatus() == 201 || response.getStatus() == 409;
			
		}
		return false;
	}

	@Override
	public final Content read(final Configuration config) {
		if (config instanceof MongoDBDynamicConfiguration) {
			MongoDBDynamicConfiguration mongoConf = (MongoDBDynamicConfiguration) config;
			RestAPI api = new RestAPI(mongoConf);
			
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
			
			response = null;
			if (hasCollectionID) {
				if (hasDocumentID) {
					if (hasFieldID) {
						response = api.getField(mongoConf.getCollectionID(), mongoConf.getDocumentID(), mongoConf.getFieldID());
					} else {
						response = api.getDocument(mongoConf.getCollectionID(), mongoConf.getDocumentID());
					}
				}
			}
			if (response.getStatus() == 200) {
				return contentObject;
			}
			
		}
		return null;
	}
	
	@Override
	public final ResponseMessage getResponseMessage() {
		return response;
	}

	/*
	 * Helper methods
	 */

	private String buildAuth() {
		StringBuilder userCredentials = new StringBuilder(USER);
		userCredentials.append(":");
		userCredentials.append(SECRET);
		
		String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userCredentials.toString().getBytes()));
		return basicAuth;
	}	

	private HttpResponse httpExecute(final HttpRequestBase httpRequest) {
		HttpResponse httpResponse = null;
		
		try {
			httpResponse = httpClient.execute(httpRequest);
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
	private Content createcontentFromHttpResponse(final HttpResponse httpResponse) {
		Scanner scanner = null;
		try {
			HttpEntity entity = httpResponse.getEntity();
			InputStream inputStream = entity.getContent();
			InputStreamReader reader = new InputStreamReader(inputStream);
			scanner = new Scanner(reader);
			String string = scanner.useDelimiter("\\A").next();
			return ContentFactory.createContent(string, F_JSON);
		} catch (IllegalStateException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (scanner != null) {
				scanner.close();
			}
		}
		return null;
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
		private MongoDBStaticConfiguration config;

		/**
		 * Creates a RestAPI instance for accessing a MongoDB
		 * at the server given in the configuration.
		 * 
		 * @param configuration The server configuration
		 */
		RestAPI(final MongoDBStaticConfiguration configuration) {
			this.config = configuration;
		}
		
		private ResponseMessage deleteDocument(final String collectionID, final String documentID) {
			URI url = config.buildUrl(collectionID, documentID, null);
			HttpDelete	request = new HttpDelete(url);
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);
				
			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, delete);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		private ResponseMessage getDocument(final String collectionID, final String documentID) {
			URI url = config.buildUrl(collectionID, documentID, null);
			HttpGet 	request = new HttpGet(url);
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);
			
			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				if (statusCode == 200) {
					contentObject = createcontentFromHttpResponse(httpResponse);
				}
				
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, get);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		private ResponseMessage postDocument(final String collectionID, final String documentID, final String documentBody) {
			URI url = config.buildUrl(collectionID, documentID, null);
			HttpPost	request = new HttpPost(url);
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
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
			URI url = config.buildUrl(collectionID, documentID, null);
			HttpPut	request = new HttpPut(url);
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
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
			URI url = config.buildUrl(collectionID, documentID, fieldID);
			HttpDelete	request = new HttpDelete(url);
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);

			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, delete);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		private ResponseMessage getField(final String collectionID, final String documentID, final String fieldID) {
			URI url = config.buildUrl(collectionID, documentID, fieldID);
			HttpGet 	request = new HttpGet(url);
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
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
						
						contentObject = ContentFactory.createContent(string);
					}
				}
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, get);
			}
			throw new RuntimeException("HttpRespone is null");
		}

		private ResponseMessage postField(final String collectionID, final String documentID, final String fieldID, final String documentBody) {
			HttpPost	request = new HttpPost(config.buildUrl(collectionID, documentID, fieldID));
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
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
		
		private ResponseMessage putField(final String collectionID, final String documentID, final String fieldID, final String documentBody) {
			URI url = config.buildUrl(collectionID, documentID, fieldID);
			HttpPut	request = new HttpPut(url);
					request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
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
			URI url = config.buildUrl(collectionID, null, null);
			HttpGet 	request = new HttpGet(url);
			request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
			request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);

			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
			contentObject = createcontentFromHttpResponse(httpResponse);
				
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, get);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		@SuppressWarnings("unused")
		private ResponseMessage getForAllDocuments(final String collectionID, final String fieldID) {		
			URI url = config.buildUrl(collectionID, null, fieldID);
			HttpGet 	request = new HttpGet(url);
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						
			HttpResponse httpResponse = httpExecute(request);
			
			if (httpResponse != null) {
				contentObject = createcontentFromHttpResponse(httpResponse);
				
				int statusCode = httpResponse.getStatusLine().getStatusCode();
				return MongoDBResponseMessage.createFromHttpStatus(statusCode, get);
			}
			throw new RuntimeException("HttpRespone is null");
		}
	}
}
