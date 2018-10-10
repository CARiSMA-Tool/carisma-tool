package carisma.check.sequencediagramcrypto.fol.term;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;

public class Conjunction extends AbstractBiJunction {
	
	public Conjunction(FOLTerm termLeft, FOLTerm termRight) {
		super(termLeft, termRight, "&");
	}
	
	@Override
	public boolean evaluate(KnowledgeBase kBase) {
		return this.termLeft.evaluate(kBase) && this.termRight.evaluate(kBase);
	}
	
	@Override
	public String toString() {
		String left = this.termLeft != null ? this.termLeft.toString() : "NULL";
		String right = this.termRight != null ? this.termRight.toString() : "NULL";
		if (this.termLeft instanceof Conjunction) {
			// Remove ( and )
			left = left.substring(1, left.length() - 1);
		}
		if (this.termRight instanceof Conjunction) {
			// Remove ( and )
			right = right.substring(1, right.length() - 1);
		}
		return String.format("(%s %s %s)", left, this.operator, right);
	}
	
	@Override
	public FOLTerm simplify() {
		
		FOLTerm leftSimplify = this.termLeft.simplify();
		FOLTerm rightSimplify = this.termRight.simplify();
		
		//True && X == X
		if (leftSimplify instanceof True) {
			return rightSimplify;
		}
		//False && X == False
		if (leftSimplify  instanceof False) {
			return leftSimplify;
		}
		//X && True == X
		if (rightSimplify  instanceof True) {
			return leftSimplify;
		}
		//X && False == False
		if (rightSimplify  instanceof False) {
			return rightSimplify;
		}
		
		//DeMorgan Not(X) && Not(Y) = Not(Y || X)
		if(rightSimplify instanceof Not && leftSimplify instanceof Not){
			FOLTerm innerLeftTerm = ((Not)leftSimplify).term.simplify();
			FOLTerm innerRightTerm = ((Not)rightSimplify).term.simplify();
			return new Not(new Disjunction(innerLeftTerm, innerRightTerm));
		}
		
		return new Conjunction(leftSimplify, rightSimplify);
	}
	
}
