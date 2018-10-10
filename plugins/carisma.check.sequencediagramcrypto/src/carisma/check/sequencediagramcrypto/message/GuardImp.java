package carisma.check.sequencediagramcrypto.message;

import carisma.check.sequencediagramcrypto.fol.FOLTerm;
import carisma.check.sequencediagramcrypto.fol.term.True;
import carisma.check.sequencediagramcrypto.parser.GuardParser;

public class GuardImp implements Guard {
	
	final private String	guardString;
	final private FOLTerm	guardFOL;
	
	public GuardImp() {
		this.guardFOL = new True();
		this.guardString = this.guardFOL.toString();
	}
	
	public GuardImp(String guardString) {
		this.guardString = guardString;
		this.guardFOL = GuardParser.Parse(this.guardString);
	}
	
	@Override
	public FOLTerm toFOLTerm() {
		return this.guardFOL;
	}
	
}
