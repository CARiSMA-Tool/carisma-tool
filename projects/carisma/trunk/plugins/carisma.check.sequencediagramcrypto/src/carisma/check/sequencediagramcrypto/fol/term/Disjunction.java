package carisma.check.sequencediagramcrypto.fol.term;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;

public class Disjunction extends AbstractBiJunction {
	
	public Disjunction(FOLTerm termLeft, FOLTerm termRight) {
		super(termLeft, termRight, "|");
	}
	
	@Override
	public boolean evaluate(KnowledgeBase kBase) {
		return this.termLeft.evaluate(kBase) || this.termRight.evaluate(kBase);
	}
	
	@Override
	public String toString() {
		String left = this.termLeft != null ? this.termLeft.toString() : "NULL";
		String right = this.termRight != null ? this.termRight.toString() : "NULL";
		if (this.termLeft instanceof Disjunction) {
			// Remove ( and )
			left = left.substring(1, left.length() - 1);
		}
		if (this.termRight instanceof Disjunction) {
			// Remove ( and )
			right = right.substring(1, right.length() - 1);
		}
		return String.format("(%s %s %s)", left, this.operator, right);
	}
	
	@Override
	public FOLTerm simplify() {
		
		FOLTerm leftSimplify = this.termLeft.simplify();
		FOLTerm rightSimplify = this.termRight.simplify();
		
		//True || X == True
		if (leftSimplify instanceof True) {
			return leftSimplify;
		}
		//False || X == X
		if (leftSimplify instanceof False) {
			return rightSimplify;
		}
		//X || True == True
		if (rightSimplify instanceof True) {
			return rightSimplify;
		}
		//X || False == X
		if (rightSimplify  instanceof False) {
			return leftSimplify;
		}
		
		//DeMorgan Not(X) || Not(Y) = Not(Y && X)
		if(rightSimplify instanceof Not && leftSimplify instanceof Not){
			FOLTerm innerLeftTerm = ((Not)leftSimplify).term;
			FOLTerm innerRightTerm = ((Not)rightSimplify).term;
			return new Not(new Conjunction(innerLeftTerm, innerRightTerm));
		}
		
		return new Disjunction(leftSimplify, rightSimplify);
	}
	
}
