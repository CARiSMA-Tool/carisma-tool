package carisma.xutils.regulatory.importer.juris;

public class ImportException extends Exception {

	/**
	 * Whatever.
	 */
	private static final long serialVersionUID = 1L;

	public ImportException() {
		super();
	}
	
	public ImportException(final String message) {
		super(message);
	}
	
	public ImportException(Throwable cause) {
		super(cause);
	}
	
	public ImportException(final String message, Throwable cause) {
		super(message, cause);
	}
}
