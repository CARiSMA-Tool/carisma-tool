package carisma.check.sequencediagramcrypto.knowledge;

public class ConcKnowledge extends BiFunctionKnowledge {
	
	public static final String	MSG_NAME	= "conc";
	
	public ConcKnowledge(Knowledge e1, Knowledge e2) {
		super(ConcKnowledge.MSG_NAME, e1, e2);
	}
	
}
