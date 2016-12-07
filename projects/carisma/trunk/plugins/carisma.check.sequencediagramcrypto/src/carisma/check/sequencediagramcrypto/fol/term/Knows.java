package carisma.check.sequencediagramcrypto.fol.term;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;
import carisma.check.sequencediagramcrypto.knowledge.Knowledge;

public class Knows implements FOLTerm {
	
	public final Knowledge	value;
	
	public Knows(Knowledge value) {
		this.value = value;
	}
	
	@Override
	public boolean evaluate(KnowledgeBase kBase) {
		return kBase.knows(value);
	}
	
	@Override
	public String toString() {
		return this.value.toKnowsString();
	}
	
	@Override
	public FOLTerm simplify() {
		return new Knows(this.value.simplify());
	}
	
}
