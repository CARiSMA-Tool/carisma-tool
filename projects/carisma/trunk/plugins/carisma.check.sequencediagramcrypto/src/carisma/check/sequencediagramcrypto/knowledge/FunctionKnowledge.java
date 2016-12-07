package carisma.check.sequencediagramcrypto.knowledge;

public class FunctionKnowledge implements Knowledge {
	
	private Knowledge	e1;
	private String		msgName;
	
	public FunctionKnowledge(String msgName, Knowledge e1) {
		this.msgName = msgName;
		this.e1 = e1;
	}
	
	public Knowledge first() {
		return this.e1;
	}
	
	public String getMsgName() {
		return this.msgName;
	}
	
	@Override
	public String toKnowsString() {
		return "Knows(" + this.toString() + ")";
	}
	
	@Override
	public String toString() {
		StringBuilder strBuilder = new StringBuilder();
		{
			strBuilder.append(this.getMsgName());
			strBuilder.append('(');
			strBuilder.append(this.e1.toString());
			strBuilder.append(')');
		}
		return strBuilder.toString();
	}
	
	// Auto-Generated
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((e1 == null) ? 0 : e1.hashCode());
		result = prime * result + ((msgName == null) ? 0 : msgName.hashCode());
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
		if (!(obj instanceof FunctionKnowledge)) {
			return false;
		}
		FunctionKnowledge other = (FunctionKnowledge) obj;
		if (e1 == null) {
			if (other.e1 != null) {
				return false;
			}
		}
		else if (!e1.equals(other.e1)) {
			return false;
		}
		if (msgName == null) {
			if (other.msgName != null) {
				return false;
			}
		}
		else if (!msgName.equals(other.msgName)) {
			return false;
		}
		return true;
	}
	
	@Override
	public Knowledge simplify() {
		FunctionKnowledge clone;
		try {
			clone = (FunctionKnowledge) this.clone();
			{
				clone.msgName = this.msgName;
				clone.e1 = this.e1.simplify();
			}
		}
		catch (CloneNotSupportedException e) {
			throw new RuntimeException(e);
		}
		return clone;
	}
	
}
