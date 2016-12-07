package carisma.check.sequencediagramcrypto.knowledge;

public class SymencKnowledge extends BiFunctionKnowledge {
	
	public static final String	MSG_NAME	= "symenc";
	
	public SymencKnowledge(Knowledge e1, Knowledge e2) {
		super(SymencKnowledge.MSG_NAME, e1, e2);
	}
	
}
