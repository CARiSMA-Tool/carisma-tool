package carisma.check.sequencediagramcrypto.message;

import java.util.SortedSet;
import java.util.TreeSet;

import carisma.check.sequencediagramcrypto.filter.Filter;
import carisma.check.sequencediagramcrypto.replacewithjava8.Optional;

public final class Messages extends TreeSet<Message> implements SortedSet<Message> {
	
	/**
	 * 
	 */
	private static final long	serialVersionUID	= -8095422151043138372L;

	public Optional<Message> messageBefor(Message message) {
		Optional<Message> beforeMessage = Optional.empty();
		for (Message innerMessage : this) {
			if (innerMessage.isBefore(message)) {
				beforeMessage = Optional.of(innerMessage);
			}
		}
		return beforeMessage;
		
	}
	
	public boolean anyAsymmetric() {
		for (Message message : this) {
			if (!message.symmetric()) {
				return true;
			}
		}
		return false;
	}
	
	public Messages filter(Filter<Message> filter) {
		Messages filteredMessages = new Messages();
		for (Message message : this) {
			if (filter.accept(message)) {
				filteredMessages.add(message);
			}
		}
		return filteredMessages;
	}
	
}
