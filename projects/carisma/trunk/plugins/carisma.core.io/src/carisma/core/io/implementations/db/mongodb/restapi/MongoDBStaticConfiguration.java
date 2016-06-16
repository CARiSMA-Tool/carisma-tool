package carisma.core.io.implementations.db.mongodb.restapi;

import java.net.URI;
import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;

import carisma.core.io.configuration.Configuration;

public class MongoDBStaticConfiguration implements Configuration{

	protected String URL = "212.34.151.216";
	
	protected final int PORT = 8080;
	
	private final String DOCUMENT = "/document/";
	private final String DOCUMENTS = "/documents/";
	private final String PATH = "/VppDatabaseRestApi/database/";
	
	public MongoDBStaticConfiguration() {
		super();
	}

	public String getPath() {
		return PATH;
	}

	public String getUrl() {
		return URL;
	}

	public int getPort() {
		return PORT;
	}
	
	public URI buildUrl(String collectionID, String documentID, String fieldID){
		String path = PATH+collectionID;
		
		boolean hasDocumentID = documentID!=null && documentID.trim().length()!=0;
		boolean hasFieldID = fieldID!=null && fieldID.trim().length()!=0;
		
		if(hasDocumentID & hasFieldID){
			path += DOCUMENT+documentID+"/"+fieldID;
		}
		else if(hasDocumentID && !hasFieldID){
			path += DOCUMENT+documentID;
		}
		else if(!hasDocumentID && hasFieldID){
			path += DOCUMENTS+fieldID;
			
		}
		else{
			path += DOCUMENTS;
		}
		URIBuilder builder = new URIBuilder()
			    .setScheme("http")
			    .setHost(URL)
			    .setPort(PORT)
			    .setPath(path);
		
		try {
			return builder.build();
		} catch (URISyntaxException e) {
			e.printStackTrace();
		}
		return null;
	}

}