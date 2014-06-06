package carisma.evolution;

public enum ConstraintType {
	AND,
	NOT,
	REQ;
	
	public static boolean contains(final String value) {
		for (ConstraintType type : values()) {
			if (type.toString().equalsIgnoreCase(value)) {
				return true;
			}
		}
		return false;
	}
}
