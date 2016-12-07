package carisma.check.sequencediagramcrypto.knowledge;

public class EncKnowledge extends BiFunctionKnowledge {
	
	public static final String	MSG_NAME	= "enc";
	
	public EncKnowledge(Knowledge e1, Knowledge e2) {
		super(EncKnowledge.MSG_NAME, e1, e2);
	}
	
}
