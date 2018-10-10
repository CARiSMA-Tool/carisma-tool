package carisma.ui.vision.io.implementations.db.mongodb.restapi;

import java.net.URI;
import java.net.URISyntaxException;

import javax.xml.bind.DatatypeConverter;

import org.apache.http.client.utils.URIBuilder;

import carisma.core.io.configuration.Configuration;

public class MongoDBConfiguration implements Configuration {

	private final String USER;
	private final String SECRET;
	private final String URL;
	private final int PORT;
	private final String PATH = "/VppDatabaseRestApi/database/";
	
	public MongoDBConfiguration(String user, String secret, String url, int port) {
		this.USER = user;
		this.SECRET = secret;
		this.URL = url;
		this.PORT = port;
	}

	@Override
	public URI buildUrl(Action action){
		if (action instanceof MongoDBAction) {
			MongoDBAction mongo = (MongoDBAction) action;
			
			String collectionID = mongo.getCollectionID();
			String documentID = mongo.getDocumentID();
			String fieldID = mongo.getFieldID();
			
			String path = this.PATH+collectionID;
			
			boolean hasDocumentID = documentID!=null && documentID.trim().length()!=0;
			boolean hasFieldID = fieldID!=null && fieldID.trim().length()!=0;
			
			if(hasDocumentID & hasFieldID){
				path += action.getAction()+documentID+"/"+fieldID;
			}
			else if(hasDocumentID && !hasFieldID){
				path += action.getAction()+documentID;
			}
			else if(!hasDocumentID && hasFieldID){
				path += action.getAction()+fieldID;
				
			}
			else{
				path += action.getAction();
			}
			URIBuilder builder = new URIBuilder()
				    .setScheme("http")
				    .setHost(this.URL)
				    .setPort(this.PORT)
				    .setPath(path);
			
			try {
				return builder.build();
			} catch (URISyntaxException e) {
				e.printStackTrace();
			}
			return null;
		}
		throw new RuntimeException();
	}

	String buildAuth() {
		StringBuilder userCredentials = new StringBuilder(this.USER);
		userCredentials.append(":");
		userCredentials.append(this.SECRET);
		
		String basicAuth = "Basic " + new String(DatatypeConverter.printBase64Binary(userCredentials.toString().getBytes()));
		return basicAuth;
	}	
	
	public static abstract class MongoDBAction implements Action {
		public final String COLLECTION, DOCUMENT, FIELD;
		
		public MongoDBAction(String collection, String document, String field) {
			this.COLLECTION = collection;
			this.DOCUMENT = document;
			this.FIELD = field;
		}

		public String getCollectionID() {
			return this.COLLECTION;
		}

		public String getDocumentID() {
			return this.DOCUMENT;
		}

		public String getFieldID() {
			return this.FIELD;
		}
	}
	
	public static class WriteAction extends MongoDBAction {

		public WriteAction(String collection, String document, String field) {
			super(collection, document, field);
		}

		@Override
		public String getAction() {
			return "/document/";
		}
		
	}
	
	public static class ReadAction extends MongoDBAction {

		public ReadAction(String collection, String document, String field) {
			super(collection, document, field);
		}

		@Override
		public String getAction() {
			if(this.COLLECTION!=null && this.DOCUMENT==null && this.FIELD!=null){
				return "/documents/";
			}
			return "/document/";
		}
		
	}
	
	public static class DeleteAction extends MongoDBAction {

		public DeleteAction(String collection, String document, String field) {
			super(collection, document, field);
		}

		@Override
		public String getAction() {
			return "/document/";
		}
		
	}
	
	public static class LockAction extends MongoDBAction {

		public LockAction(String collection, String document, String field) {
			super(collection, document, field);
		}

		@Override
		public String getAction() {
			return "/lock/";
		}
		
	}
	
	public static class UnlockAction extends MongoDBAction {

		public UnlockAction(String collection, String document, String field) {
			super(collection, document, field);
		}

		@Override
		public String getAction() {
			return "/unlock/";
		}
		
	}
}