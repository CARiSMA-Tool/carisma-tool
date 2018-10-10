package carisma.check.sequencediagramcrypto.message;

import carisma.check.sequencediagramcrypto.fol.FOLTerm;
import carisma.check.sequencediagramcrypto.knowledge.Knowledge;

public interface Argument {
	public Knowledge toKnowledge();
	
	public FOLTerm toFOLTerm();
	
	public String value();
}
