package carisma.check.sequencediagramcrypto.fol.term;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;
import carisma.check.sequencediagramcrypto.knowledge.ValueStringKnowledge;

public class StringNotEqual implements FOLTerm {
	
	public final String	valueLeft;
	public final String	valueRight;
	
	public StringNotEqual(String valueLeft, String valueRight) {
		this.valueLeft = valueLeft;
		this.valueRight = valueRight;
	}
	
	@Override
	public String toString() {
		return String.format("(%s != %s)", this.valueLeft, this.valueRight);
	}
	
	@Override
	public boolean evaluate(KnowledgeBase kBase) {
		// a = a , is always true!
		if (valueLeft.equals(valueRight)) {
			return false;
		}
		return !kBase.isSame(new ValueStringKnowledge(this.valueLeft), new ValueStringKnowledge(
				this.valueRight));
	}
	
	@Override
	public FOLTerm simplify() {
		return this;
	}
	
}
