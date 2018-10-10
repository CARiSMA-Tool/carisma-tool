package carisma.processanalysis.loader.misc.tests;

import org.junit.Test;

import carisma.processanalysis.loader.misc.TextFileImporter;
import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;


public class TextFileImporterTest {

	@Test
	public final void test() {
		TextFileImporter importer = new TextFileImporter("resources/rf_activities.txt");
		ProcessDescription model = importer.doImport();
		
		for(ProcessEntity curEntity : model.getEntities()){
			System.out.println(curEntity.getTexts());
		}
	}

}
