package carisma.core.logging;

public interface ExternalLogPrinter {

	/**
	 * Prints a log message.
	 * @param level
	 * @param source
	 * @param message
	 */
	public boolean print(StackTraceElement ste, LogLevel level, String message);
}
