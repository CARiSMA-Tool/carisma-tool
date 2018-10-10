package carisma.xutils.regulatory.extractor;

import java.io.File;

/**
 * Encapsulating class for the extraction constants. 
 * @author dwarzecha
 *
 */
public class ExtractorConstants {
	
	public static final String DEFAULT_RESOURCE_FOLDER = "resources" + File.separator;
	
	public static final String DEFAULT_ONTOLOGY_OUTPUT_FOLDER = "output" + File.separator + "ontologies" + File.separator;
	
	/**
	 * Never initialized.
	 */
	private ExtractorConstants() {
		
	}
}
