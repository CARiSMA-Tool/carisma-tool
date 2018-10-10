package carisma.check.sequencediagramcrypto.knowledge;

public class BiFunctionKnowledge implements Knowledge {
	
	private Knowledge	e1;
	private Knowledge	e2;
	private String		msgName;
	
	public BiFunctionKnowledge(String msgName, Knowledge e1, Knowledge e2) {
		this.msgName = msgName;
		this.e1 = e1;
		this.e2 = e2;
	}
	
	public String getMsgName() {
		return this.msgName;
	}
	
	public Knowledge first() {
		return this.e1;
	}
	
	public Knowledge second() {
		return this.e2;
	}
	
	@Override
	public String toKnowsString() {
		return "Knows(" + this.toString() + ")";
	}
	
	@Override
	public String toString() {
		// Buffer-Size is msgName.length + 3 '(' ',' ')' + aproximation of e1 and e2 (both could be around 8 characters)
		StringBuilder strBuilder = new StringBuilder(this.msgName.length() + 3 + 2 * 8);
		{
			strBuilder.append(this.getMsgName());
			strBuilder.append('(');
			strBuilder.append(this.e1.toString());
			strBuilder.append(',');
			strBuilder.append(this.e2.toString());
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
		result = prime * result + ((e2 == null) ? 0 : e2.hashCode());
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
		if (!(obj instanceof BiFunctionKnowledge)) {
			return false;
		}
		BiFunctionKnowledge other = (BiFunctionKnowledge) obj;
		if (e1 == null) {
			if (other.e1 != null) {
				return false;
			}
		}
		else if (!e1.equals(other.e1)) {
			return false;
		}
		if (e2 == null) {
			if (other.e2 != null) {
				return false;
			}
		}
		else if (!e2.equals(other.e2)) {
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
		BiFunctionKnowledge clone;
		try {
			clone = (BiFunctionKnowledge) this.clone();
			{
				clone.msgName = this.msgName;
				clone.e1 = this.e1.simplify();
				clone.e2 = this.e2.simplify();
			}
		}
		catch (CloneNotSupportedException e) {
			throw new RuntimeException(e);
		}
		return clone;
	}
	
}
