package carisma.core.io.implementations.db.mongodb.restapi;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.Base64;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
import carisma.core.io.content.BASE64;
import carisma.core.io.content.Content;
import carisma.core.io.content.Content.ContentException;
import carisma.core.io.content.JSON;
import carisma.core.io.content.PLAIN;
import carisma.core.io.content.XML_DOM;
import carisma.core.io.implementations.db.DataBaseIO;
import carisma.core.io.implementations.db.ResponseMessage;

import static carisma.core.io.implementations.db.mongodb.restapi.MongoDBResponseMessage.Operation.*;
import static carisma.core.io.implementations.db.mongodb.restapi.MongoDBConstants.*;

public class MongoDBRestAPI implements DataBaseIO {
	
	private final String SECRET;
	private final String USER;
	private final String URL;
	
	private HttpClient httpClient = HttpClientBuilder.create().build();
	private Content content;
	private ResponseMessage response;
	
	public MongoDBRestAPI(String user, String secret, String url) {
		if(user==null || user.trim()==""){
			throw new RuntimeException("The value of the parameter \"user\" is null or empty.");
		}
		if(secret==null || secret.trim()==""){
			throw new RuntimeException("The value of the parameter \"secret\" is null or empty.");
		}
		if(url==null || url.trim()==""){
			throw new RuntimeException("The value of the parameter \"url\" is null or empty.");
		}
		USER = user;
		SECRET = secret;
		URL = url;
	}
	
	/*
	 * Interface methods for reading and writing
	 * 
	 * (non-Javadoc)
	 * @see carisma.core.io.implementations.db.DataBaseIO#write(carisma.core.io.configuration.Configuration, carisma.core.io.content.Content)
	 */
	
	@Override
	public boolean write(Configuration config, Content content) {
		if(config instanceof MongoDBDynamicConfiguration){
			MongoDBDynamicConfiguration mongoConf = (MongoDBDynamicConfiguration) config;
			RestAPI api = new RestAPI(mongoConf);
			
			String contentFormat = content.getFormat();
			String contentAsString;
			if(contentFormat.compareTo(JSON.ID)==0){
				contentAsString = content.asString();
			}
			else if(contentFormat.compareTo(XML_DOM.ID)==0){
				contentAsString = new JSON((XML_DOM) content).asString();
			}
			else if (contentFormat.compareTo(PLAIN.ID)==0){
				contentAsString = StringEscapeUtils.escapeJson(content.asString());
			}
			else{
				throw new RuntimeException("Unknown content format: "+contentFormat);
			}
			
			String collectionID = mongoConf.getCollectionID();
			String documentID = mongoConf.getDocumentID();
			String fieldID = mongoConf.getFieldID();
			
			boolean hasCollectionID = collectionID!=null && collectionID.trim().length()!=0;
			if(!hasCollectionID){
				throw new RuntimeException("No CollectionID is given");
			}
			boolean hasDocumentID = documentID!=null && documentID.trim().length()!=0;
			boolean hasFieldID = fieldID!=null && fieldID.trim().length()!=0;
			if(!hasDocumentID && !hasFieldID){
				throw new RuntimeException("No DocumentID and no FieldID is given");
			}
			
			response = null;
			if(hasCollectionID){
				if(hasDocumentID){
					if(hasFieldID){
						response = api.getField(collectionID, documentID, fieldID);
					}
					else{
						response = api.getDocument(collectionID, documentID);
					}
				}
			}
			
			if(response!=null && response.getStatus() == 200){
				if(hasDocumentID){
					if(hasFieldID){
						response = api.putField(collectionID, documentID, fieldID, "{\""+fieldID+"\":'"+contentAsString+"'}");
					}
					else{
						api.deleteDocument(collectionID, documentID);
						response = api.postDocument(collectionID, documentID, contentAsString);
					}
				}
			}
			else {
				if(hasDocumentID){
					if(hasFieldID){
						response = api.getDocument(collectionID, documentID);
						if(response.getStatus() == 404){
							response = api.postDocument(collectionID, documentID, "");
						}
						response = api.postField(collectionID, documentID, fieldID, "{\""+fieldID+"\":'"+contentAsString+"'}");
					}
					else{
						response = api.postDocument(collectionID, documentID, contentAsString);
					}
				}
			}
			
			return response.getStatus() == 201 || response.getStatus() == 409;
			
		}
		return false;
	}

	@Override
	public Content read(Configuration config) {
		if(config instanceof MongoDBDynamicConfiguration){
			MongoDBDynamicConfiguration mongoConf = (MongoDBDynamicConfiguration) config;
			RestAPI api = new RestAPI(mongoConf);
			
			String collectionID = mongoConf.getCollectionID();
			String documentID = mongoConf.getDocumentID();
			String fieldID = mongoConf.getFieldID();
			
			boolean hasCollectionID = collectionID!=null && collectionID.trim().length()!=0;
			if(!hasCollectionID){
				throw new RuntimeException("No CollectionID is given");
			}
			boolean hasDocumentID = documentID!=null && documentID.trim().length()!=0;
			boolean hasFieldID = fieldID!=null && fieldID.trim().length()!=0;
			if(!hasDocumentID && !hasFieldID){
				throw new RuntimeException("No DocumentID and no FieldID is given");
			}
			
			response = null;
			if(hasCollectionID){
				if(hasDocumentID){
					if(hasFieldID){
						response = api.getField(mongoConf.getCollectionID(), mongoConf.getDocumentID(), mongoConf.getFieldID());
					}
					else{
						response = api.getDocument(mongoConf.getCollectionID(), mongoConf.getDocumentID());
					}
				}
			}
			if(response.getStatus() == 200){
				return content;
			}
			
		}
		return null;
	}
	
	@Override
	public ResponseMessage getResponseMessage() {
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

	private HttpResponse httpExecute(HttpRequestBase request) {
		HttpResponse response = null;
		
		try {
			response = httpClient.execute(request);
		} catch (ClientProtocolException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (IllegalStateException e) {
			e.printStackTrace();
		}
		return response;
	}
	
	private Content createcontentFromHttpResponse(HttpResponse response){
		Scanner scanner = null;
		try{
			HttpEntity entity = response.getEntity();
			InputStream content = entity.getContent();
			InputStreamReader reader = new InputStreamReader(content);
			scanner = new Scanner(reader);
			String inputStreamString = scanner.useDelimiter("\\A").next();
			return new JSON(inputStreamString);
		} catch (IllegalStateException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		finally {
			if(scanner!=null){
				scanner.close();
			}
		}
		return null;
	}
	
	private class RestAPI {
		
		private MongoDBStaticConfiguration configuration;

		/*
		 * Private implementation of RestAPI
		 */

		public RestAPI(MongoDBStaticConfiguration configuration) {
			this.configuration = configuration;
		}
		
		private ResponseMessage deleteDocument(String collectionID, String documentID){
			HttpDelete	request = new HttpDelete(configuration.buildUrl(collectionID, documentID, null));
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);
				
			HttpResponse response = httpExecute(request);
			
			if(response != null){
				return MongoDBResponseMessage.createFromHttpStatus(response.getStatusLine().getStatusCode(), delete);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		private ResponseMessage getDocument(String collectionID, String documentID){
			HttpGet 	request = new HttpGet(configuration.buildUrl(collectionID, documentID, null));
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);
			
			HttpResponse response = httpExecute(request);
			
			if(response != null){
				if(response.getStatusLine().getStatusCode()==200){
					content = createcontentFromHttpResponse(response);
				}
				
				return MongoDBResponseMessage.createFromHttpStatus(response.getStatusLine().getStatusCode(), get);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		private ResponseMessage postDocument(String collectionID, String documentID, String documentBody){
			HttpPost	request = new HttpPost(configuration.buildUrl(collectionID, documentID, null));
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
			
			HttpResponse response = httpExecute(request);
			
			if(response != null){
				return MongoDBResponseMessage.createFromHttpStatus(response.getStatusLine().getStatusCode(), post);
			}
			throw new RuntimeException("HttpRespone is null");	
		}
		
		@SuppressWarnings("unused")
		private ResponseMessage putDocument(String collectionID, String documentID, String documentBody){
			HttpPut	request = new HttpPut(configuration.buildUrl(collectionID, documentID, null));
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
						
			HttpResponse response = httpExecute(request);
			
			if(response != null){
				return MongoDBResponseMessage.createFromHttpStatus(response.getStatusLine().getStatusCode(), put);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		private ResponseMessage deleteField(String collectionID, String documentID, String fieldID){
			HttpDelete	request = new HttpDelete(configuration.buildUrl(collectionID, documentID, fieldID));
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);

			HttpResponse response = httpExecute(request);
			
			if(response != null){
				return MongoDBResponseMessage.createFromHttpStatus(response.getStatusLine().getStatusCode(), delete);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		private ResponseMessage getField(String collectionID, String documentID, String fieldID){
			HttpGet 	request = new HttpGet(configuration.buildUrl(collectionID, documentID, fieldID));
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						request.addHeader(KEYWORD_CONTENT_TYPE, APPLICATION_JSON);

			HttpResponse response = httpExecute(request);
			
			if(response != null){
				if(response.getStatusLine().getStatusCode() == 200){
					Object field = ((JSON)createcontentFromHttpResponse(response)).get(fieldID);
					if(field instanceof String){
						String string = (String) field;
						
						//is base64 encoded?
						Pattern pattern = Pattern.compile("^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)$");
						Matcher matcher = pattern.matcher(string);
						if(matcher.find()){
							content = new BASE64(string.getBytes());
						}
						else if(string.startsWith("{")){
							content = new JSON(string);
						}
						else if(string.startsWith("<?xml")){
							try {
								content = new XML_DOM(string);
							} catch (ContentException e) {
								e.printStackTrace();
								content = new PLAIN(string);
							}
						}
						else{
							content = new PLAIN(string);
						}
					}
				}
				return MongoDBResponseMessage.createFromHttpStatus(response.getStatusLine().getStatusCode(), get);
			}
			throw new RuntimeException("HttpRespone is null");
		}

		private ResponseMessage postField(String collectionID, String documentID, String fieldID, String documentBody){
			HttpPost	request = new HttpPost(configuration.buildUrl(collectionID, documentID, fieldID));
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
			
			HttpResponse response = httpExecute(request);
			
			if(response != null){
				return MongoDBResponseMessage.createFromHttpStatus(response.getStatusLine().getStatusCode(), post);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		@SuppressWarnings("unused")
		private ResponseMessage putField(String collectionID, String documentID, String fieldID, String documentBody){
			HttpPut	request = new HttpPut(configuration.buildUrl(collectionID, documentID, fieldID));
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
			
			HttpResponse response = httpExecute(request);
			
			if(response != null){
				return MongoDBResponseMessage.createFromHttpStatus(response.getStatusLine().getStatusCode(), put);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		@SuppressWarnings("unused")
		private ResponseMessage getDocuments(String collectionID){
			HttpGet 	request = new HttpGet(configuration.buildUrl(collectionID, null, null));
			request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
			request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);

			HttpResponse response = httpExecute(request);
			
			if(response != null){
			content = createcontentFromHttpResponse(response);
				
				return MongoDBResponseMessage.createFromHttpStatus(response.getStatusLine().getStatusCode(), get);
			}
			throw new RuntimeException("HttpRespone is null");
		}
		
		@SuppressWarnings("unused")
		private ResponseMessage getForAllDocuments(String collectionID, String fieldID){		
			HttpGet 	request = new HttpGet(configuration.buildUrl(collectionID, null, fieldID));
						request.addHeader(KEYWORD_AUTHORIZATION, buildAuth());
						request.addHeader(KEYWORD_ACCEPT, APPLICATION_JSON);
						
			HttpResponse response = httpExecute(request);
			
			if(response != null){
				content = createcontentFromHttpResponse(response);;
				
				return MongoDBResponseMessage.createFromHttpStatus(response.getStatusLine().getStatusCode(), get);
			}
			throw new RuntimeException("HttpRespone is null");
		}
	}
}
