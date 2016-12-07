package carisma.check.sequencediagramcrypto.fol.term;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;
import carisma.check.sequencediagramcrypto.knowledge.Knowledge;

public class KnowledgeEqual implements FOLTerm {
	
	public final Knowledge	valueLeft;
	public final Knowledge	valueRight;
	
	public KnowledgeEqual(Knowledge valueLeft, Knowledge valueRight) {
		this.valueLeft = valueLeft;
		this.valueRight = valueRight;
	}
	
	@Override
	public String toString() {
		return String.format("(%s == %s)", this.valueLeft.toString(), this.valueRight.toString());
	}
	
	@Override
	public boolean evaluate(KnowledgeBase kBase) {
		// a = a , is always true!
		if (valueLeft.equals(valueRight)) {
			return true;
		}
		return kBase.isSame(this.valueLeft, this.valueRight);
	}
	
	@Override
	public FOLTerm simplify() {
		return this;
	}
	
}
