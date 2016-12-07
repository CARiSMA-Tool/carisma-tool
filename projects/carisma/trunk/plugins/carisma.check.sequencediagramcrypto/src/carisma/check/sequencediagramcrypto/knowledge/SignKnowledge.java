package carisma.check.sequencediagramcrypto.knowledge;

public class SignKnowledge extends BiFunctionKnowledge {
	
	public static final String	MSG_NAME	= "sign";
	
	public SignKnowledge(Knowledge e1, Knowledge e2) {
		super(SignKnowledge.MSG_NAME, e1, e2);
	}
	
}
