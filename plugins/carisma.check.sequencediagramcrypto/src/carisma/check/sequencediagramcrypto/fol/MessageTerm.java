package carisma.check.sequencediagramcrypto.fol;

public class MessageTerm {
	
	final private FOLTerm	receivedTerm;
	final private FOLTerm	guardTerm;
	final private FOLTerm	sendTerm;
	
	public MessageTerm(FOLTerm receivedTerm, FOLTerm guardTerm, FOLTerm messageTerm) {
		this.receivedTerm = receivedTerm;
		this.guardTerm = guardTerm;
		this.sendTerm = messageTerm;
	}
	
	public FOLTerm receivedTerm() {
		return this.receivedTerm;
	}
	
	public FOLTerm guardTerm() {
		return this.guardTerm;
	}
	
	public FOLTerm sendTerm() {
		return this.sendTerm;
	}
	
	@Override
	public String toString() {
		return "MessageTerm [receivedTerm=" + receivedTerm + ", guardTerm=" + guardTerm
				+ ", sendTerm=" + sendTerm + "]";
	}
	
}
