package carisma.check.sequencediagramcrypto.fol.term;

import carisma.check.sequencediagramcrypto.fol.FOLTerm;

public abstract class AbstractBiJunction implements FOLTerm {
	
	public final FOLTerm	termLeft;
	public final FOLTerm	termRight;
	public final String		operator;
	
	public AbstractBiJunction(FOLTerm termLeft, FOLTerm termRight, String operator) {
		this.termLeft = termLeft;
		this.termRight = termRight;
		this.operator = operator;
	}
	
	@Override
	public String toString() {
		String left = this.termLeft != null ? this.termLeft.toString() : "NULL";
		String right = this.termRight != null ? this.termRight.toString() : "NULL";
		return String.format("(%s %s %s)", left, this.operator, right);
	}
	
	@Override
	abstract public FOLTerm simplify();
	
}
