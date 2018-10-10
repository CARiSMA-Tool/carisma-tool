package carisma.processanalysis.loader.misc;

import carisma.processanalysis.textmodel.ProcessDescription;

/**
 * An interface that describes classes to import process models into the internal datamodel.
 * @author thumberg
 *
 */
public interface Importer {
	/**
	 * Performs the import. Any necessary parameters have to be stored in the instances before.
	 * @return The imported process description. Null if the import failed.
	 */
	ProcessDescription doImport() throws ImportException;
}
