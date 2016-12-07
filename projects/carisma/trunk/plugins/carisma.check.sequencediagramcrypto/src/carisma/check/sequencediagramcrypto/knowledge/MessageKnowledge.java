package carisma.check.sequencediagramcrypto.knowledge;

public class MessageKnowledge implements Knowledge {
	
	final private String	messageName;
	final private int		argumentNumber;
	
	public MessageKnowledge(String messageName, int argumentNumber) {
		this.messageName = messageName;
		this.argumentNumber = argumentNumber;
	}
	
	@Override
	public String toKnowsString() {
		return "knows(" + this.toString() + ")";
	}
	
	@Override
	public String toString() {
		return this.messageName + "_" + argumentNumber;
	}
	
	// Auto-Generated
	@Override
	public int hashCode() {
		return this.toString().hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (obj instanceof ValueStringKnowledge) {
			return this.toString().equals(obj.toString());
		}
		if (!(obj instanceof MessageKnowledge)) {
			return false;
		}
		MessageKnowledge other = (MessageKnowledge) obj;
		if (argumentNumber != other.argumentNumber) {
			return false;
		}
		if (messageName == null) {
			if (other.messageName != null) {
				return false;
			}
		}
		else if (!messageName.equals(other.messageName)) {
			return false;
		}
		return true;
	}

	@Override
	public Knowledge simplify() {
		return this;
	}
	
}
