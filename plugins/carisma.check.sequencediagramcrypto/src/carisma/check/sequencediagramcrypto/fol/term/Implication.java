package carisma.check.sequencediagramcrypto.fol.term;

import carisma.check.sequencediagramcrypto.data.KnowledgeBase;
import carisma.check.sequencediagramcrypto.fol.FOLTerm;

public class Implication extends AbstractBiJunction {
	
	public Implication(FOLTerm termLeft, FOLTerm termRight) {
		super(termLeft, termRight, "=>");
	}
	
	@Override
	public boolean evaluate(KnowledgeBase kBase) {
		return !this.termLeft.evaluate(kBase) || this.termRight.evaluate(kBase);
	}
	
	@Override
	public FOLTerm simplify() {
		FOLTerm leftSimplify = this.termLeft.simplify();
		FOLTerm rightSimplify = this.termRight.simplify();
		
		if (leftSimplify instanceof True) {
			return rightSimplify;
		}
		if (leftSimplify instanceof False) {
			return new True();
		}
		if (rightSimplify instanceof True) {
			return rightSimplify;
		}
		if (rightSimplify instanceof False) {
			return new Not(leftSimplify);
		}
		
		// Not(X) => Y == Not(Not(X)) || Y == X || Y
		if(leftSimplify instanceof Not){
			return new Disjunction(((Not)leftSimplify).term, rightSimplify);
		}
		
		return new Implication(leftSimplify, rightSimplify);
		
	}
	
}
