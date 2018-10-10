package carisma.processanalysis.loader.misc.tests;

//import static org.junit.Assert.*;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Test;

import carisma.processanalysis.loader.misc.CSVImporter;
import carisma.processanalysis.textmodel.ProcessDescription;


public class CSVImporterTest {

	@Test
	public final void test() {
		String filename = "resources/rf_activities.txt";
		
		//import activites from csv file
		CSVImporter importer = new CSVImporter(filename, ',');
		ProcessDescription model = importer.doImport();
		
		File f = new File(filename);
		BufferedReader br = null;
		
		try {
			br = new BufferedReader(new FileReader(f));
		} catch (FileNotFoundException e) {
			fail(e.getMessage());
		}
		
		String line = null;
		int lineCount = 0;
		
		try {
			while( ( line = br.readLine() ) != null ) {
				if( !line.startsWith("#") );
					++lineCount;
			}
		} catch( IOException ioe ) {
			fail( ioe.getMessage() );
		}
		
		//Die Anzahl an Entities muss der Anzahl an Zeilen in der Datei entsprechen 
		assertTrue(model.getEntities().size() == lineCount);
	}

}
