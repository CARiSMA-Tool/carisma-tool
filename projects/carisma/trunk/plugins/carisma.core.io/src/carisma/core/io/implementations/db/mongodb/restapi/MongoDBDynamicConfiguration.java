package carisma.core.io.implementations.db.mongodb.restapi;

import java.net.URI;
import carisma.core.io.configuration.Configuration;

public class MongoDBDynamicConfiguration extends MongoDBStaticConfiguration implements Configuration{
	
	//Dynamic part
	private final String fieldID;
	private final String documentID;
	private final String collectionID;
	
	public MongoDBDynamicConfiguration(String url, String collectionID, String documentID, String fieldID) {
		super.URL = url;
		this.collectionID = collectionID;
		this.documentID = documentID;
		this.fieldID = fieldID;
	}

	public MongoDBDynamicConfiguration(String collectionID, String documentID, String fieldID) {
		this.collectionID = collectionID;
		this.documentID = documentID;
		this.fieldID = fieldID;
	}

	public URI buildUrl() {
		return super.buildUrl(collectionID, documentID, fieldID);
	}
	
	public String getFieldID() {
		return fieldID;
	}

	public String getDocumentID() {
		return documentID;
	}

	public String getCollectionID() {
		return collectionID;
	}
}