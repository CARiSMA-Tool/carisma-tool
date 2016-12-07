package carisma.check.sequencediagramcrypto.fol.term;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;

public class True implements FOLTerm {
	
	@Override
	public boolean evaluate(KnowledgeBase kBase) {
		return true;
	}
	
	@Override
	public String toString() {
		return "TRUE";
	}
	
	@Override
	public FOLTerm simplify() {
		return this;
	}
	
}
