package carisma.check.sequencediagramcrypto.fol;

import java.util.List;
import java.util.ListIterator;

import carisma.check.sequencediagramcrypto.fol.term.Conjunction;
import carisma.check.sequencediagramcrypto.fol.term.Implication;
import carisma.check.sequencediagramcrypto.fol.term.Knows;
import carisma.check.sequencediagramcrypto.fol.term.True;
import carisma.check.sequencediagramcrypto.knowledge.Knowledge;
import carisma.check.sequencediagramcrypto.knowledge.MessageKnowledge;
import carisma.check.sequencediagramcrypto.message.Argument;
import carisma.check.sequencediagramcrypto.message.Message;
import carisma.check.sequencediagramcrypto.message.Messages;

public final class TermGenerator {
	
	public static MessageTerm MessageFOLTerm(Messages recievedInBetweenMessages, Message message) {
		
		// Generate ReceivedTerm
		FOLTerm receivedTerm = new True();
		{
			for (Message receivedMessage : recievedInBetweenMessages) {
				String messageName = receivedMessage.name();
				for (int i = 0; i < receivedMessage.arguments().size(); i++) {
					Knowledge argKnowledge = new MessageKnowledge(messageName, i);
					receivedTerm = new Conjunction(receivedTerm, new Knows(argKnowledge));
				}
			}
		}
		receivedTerm = receivedTerm.simplify();
		
		// Generate GuardTerm
		FOLTerm guardTerm = message.guard().toFOLTerm().simplify();
		
		// Generate SendTerm
		FOLTerm sendTerm = new True();
		{
			for (Argument arg : message.arguments()) {
				sendTerm = new Conjunction(sendTerm, arg.toFOLTerm());
			}
		}
		sendTerm = sendTerm.simplify();
		
		return new MessageTerm(receivedTerm, guardTerm, sendTerm);
	}
	
	public static FOLTerm AsymmetricPeerTerm(List<MessageTerm> messageTerms) {
		FOLTerm asymPeerTerm = new True();
		{
			for (MessageTerm messageTerm : messageTerms) {
				FOLTerm leftSide = new Conjunction(messageTerm.receivedTerm(),
						messageTerm.guardTerm());
				FOLTerm rightSide = messageTerm.sendTerm();
				asymPeerTerm = new Conjunction(asymPeerTerm, new Implication(leftSide, rightSide));
			}
		}
		asymPeerTerm = asymPeerTerm.simplify();
		return asymPeerTerm;
	}
	
	public static FOLTerm SymmetricPeerTerm(List<MessageTerm> messageTerms) {
		FOLTerm symPeerTerm = new True();
		{
			// How it should work:
			// A & B => C
			// D & E => F
			// G & H => I
			// Transforms to:
			// (A & B) => C & (D & E => F & (G & H => I)
			ListIterator<MessageTerm> iter = messageTerms.listIterator(messageTerms.size());
			while (iter.hasPrevious()) {
				MessageTerm messageTerm = iter.previous();
				FOLTerm leftSide = new Conjunction(messageTerm.receivedTerm(),
						messageTerm.guardTerm());
				FOLTerm rightSide = new Conjunction(messageTerm.sendTerm(), symPeerTerm);
				symPeerTerm = new Implication(leftSide, rightSide);
			}
		}
		symPeerTerm = symPeerTerm.simplify();
		return symPeerTerm;
	}
	
}
