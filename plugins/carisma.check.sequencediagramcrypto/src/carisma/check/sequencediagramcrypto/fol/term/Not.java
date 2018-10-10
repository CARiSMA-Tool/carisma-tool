package carisma.check.sequencediagramcrypto.fol.term;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;

public class Not implements FOLTerm {
	
	public final FOLTerm	term;
	
	public Not(FOLTerm term) {
		this.term = term;
	}
	
	@Override
	public boolean evaluate(KnowledgeBase kBase) {
		return !this.term.evaluate(kBase);
	}
	
	@Override
	public String toString() {
		return String.format("! %s", this.term.toString());
	}
	
	@Override
	public FOLTerm simplify() {
		// Not(Not(t)) == t
		if (this.term instanceof Not) {
			return ((Not) this.term).term.simplify();
		}
		
		// Not(StringEqual(X,Y)) == StringNotEqual(X,Y)
		if (this.term instanceof StringEqual) {
			StringEqual strEql = (StringEqual) this.term;
			return new StringNotEqual(strEql.valueLeft, strEql.valueRight);
		}
		
		return new Not(this.term.simplify());
	}
	
}
