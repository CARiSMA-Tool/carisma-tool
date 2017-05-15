package carisma.ui.vision.test.visiondb;

import org.junit.Test;

import carisma.core.io.content.ContentFactory;
import carisma.core.io.content.ContentFactory.ContentFormats;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI.MongoDBDestination;

public class VisionDBTest {

	private final static String dbaddress = "212.34.151.216";
	private final static String dbport = "9898";
	private final static String dbuser = "vppapp";
	private final static String dbpassword = "vppapptomcatpassword";
	private final static String dbApiPath = "VppDatabaseRestApi/database";
	
	private static final String carismaCollection = "testCollection";
	private static final String carismaDocument = "carismaTest";
	private static final String carismaField = "carismaTest";
	
	@Test
	public void writeField(){
		MongoDBRestAPI db = new MongoDBRestAPI(dbuser, dbpassword, dbaddress);
		MongoDBDestination carismaConfiguration = new MongoDBDestination(carismaCollection, carismaDocument, carismaField);
		db.write(carismaConfiguration, ContentFactory.createContent("Test", ContentFormats.F_PLAIN));		
	}
}
