package carisma.core.checks;


/**
 * Interface for analysis checks with IDs of the extension point.
 * @author speldszus
 *
 */
public interface CarismaCheckWithID extends CarismaCheck {
	
	/**
	 * Returns the ID under which the check is registered at the extension point "carisma.carismacheck"
	 * @return ID
	 */
	String getCheckID();
	
	/**
	 * Returns the name of the check
	 * @return name
	 */
	String getName();
	
}
