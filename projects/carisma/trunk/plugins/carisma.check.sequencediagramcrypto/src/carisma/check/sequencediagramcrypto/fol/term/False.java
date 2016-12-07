package carisma.check.sequencediagramcrypto.fol.term;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;

public class False implements FOLTerm {
	
	@Override
	public boolean evaluate(KnowledgeBase kBase) {
		return false;
	}
	
	@Override
	public String toString() {
		return "FALSE";
	}
	
	@Override
	public FOLTerm simplify() {
		return this;
	}
	
}
