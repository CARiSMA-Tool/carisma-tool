package carisma.check.sequencediagramcrypto.fol;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;

public interface FOLTerm extends Cloneable {
	
	boolean evaluate(KnowledgeBase knows);
	
	public FOLTerm simplify();
	
}
