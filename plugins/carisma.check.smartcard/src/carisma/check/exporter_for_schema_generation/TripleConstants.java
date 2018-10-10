package carisma.check.exporter_for_schema_generation;

/** Class containing Strings used for the output of the schema.
 * @author bberghoff
 */
public final class TripleConstants {
    
    /** Hiding the default constructor.
     */
    private TripleConstants() {
    }

    /**
     * Constant String describing a Element is element of a set.
     */
	public static final String IS_ELEMENT = "IN";
	/**
     * Constant String describing a Element is not element of a set.
	 */
	public static final String IS_NOT_ELEMENT = "NOT.IN";
	/**
     * Constant String describing operator 'and'.
     */ 
	public static final String AND = "&&";
    /**
     * Constant String describing operator 'or'.
     */ 
	public static final String OR = "||";
    /**
     * Constant String describing the command to change a status.
     */ 
	public static final String SET_STATUS = "SET_STATUS";
	/**
	 * Constant String for the used seperator.
	 */
	public static final String SEPERATOR = ",";
}
