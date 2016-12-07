package carisma.check.sequencediagramcrypto.knowledge;

public class ValueStringKnowledge implements Knowledge {
	
	final private String	value;
	
	public ValueStringKnowledge(String value) {
		this.value = value;
	}
	
	@Override
	public String toKnowsString() {
		return "knows(" + this.toString() + ")";
	}
	
	@Override
	public String toString() {
		return value;
	}
	
	// Auto-Generated
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}
	
	// Auto-Generated
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof ValueStringKnowledge)) {
			return false;
		}
		ValueStringKnowledge other = (ValueStringKnowledge) obj;
		if (value == null) {
			if (other.value != null) {
				return false;
			}
		}
		else if (!value.equals(other.value)) {
			return false;
		}
		return true;
	}

	@Override
	public Knowledge simplify() {
		return this;
	}
	
}
