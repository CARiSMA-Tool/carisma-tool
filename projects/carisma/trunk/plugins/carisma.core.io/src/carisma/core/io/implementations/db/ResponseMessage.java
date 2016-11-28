package carisma.core.io.implementations.db;

/**
 * An interface for basic response messages.
 * @author speldszus
 *
 */
public interface ResponseMessage {

	/**
	 * Returns an unique status code.
	 * @return a status code
	 */
	int getStatus();
	
	/**
	 * Returns a human readable description of the response status.
	 * @return human readable description
	 */
	@Override
	String toString();

}