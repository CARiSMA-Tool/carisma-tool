package carisma.check.sequencediagramcrypto.knowledge;

public class InvKnowledge extends FunctionKnowledge {
	
	public static final String	MSG_NAME	= "inv";
	
	public InvKnowledge(Knowledge e1) {
		super(InvKnowledge.MSG_NAME, e1);
	}

	@Override
	public Knowledge simplify() {
		if(this.first() instanceof InvKnowledge){
			InvKnowledge e1 = (InvKnowledge)this.first();
			return e1.first().simplify();
		}
		return super.simplify();
	}
	
}
