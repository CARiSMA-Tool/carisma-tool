package carisma.processanalysis.loader.misc;

/**
 * An interface for classes that import models from files.
 * @author thumberg
 *
 */
public abstract class FileImporter implements Importer {
	/**
	 * The filename that is used for import.
	 */
	private String filename;

	/**
	 * Get the filename that is used for import.
	 * @return The filename.
	 */
	public final String getFilename() {
		return filename;
	}

	/**
	 * Get the filename that is used for import.
	 * @param filename The filename.
	 */
	public final void setFilename(final String filename) {
		this.filename = filename;
	}

	/**
	 * Constructor.
	 * @param filename The file that contains the model to be imported.
	 */
	public FileImporter(final String filename) {
		this.filename = filename;
	}
	
	/**
	 * 
	 */
//	public abstract DataModel doImport();
}
