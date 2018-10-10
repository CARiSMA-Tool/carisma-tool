package carisma.check.sequencediagramcrypto.filter.message;

import carisma.check.sequencediagramcrypto.filter.Filter;
import carisma.check.sequencediagramcrypto.message.Message;

public class MessageBefore implements Filter<Message> {
	
	final private Message	message;
	
	public MessageBefore(final Message message) {
		this.message = message;
	}
	
	@Override
	public boolean accept(Message o) {
		return this.message.isBefore(o);
	}
	
}
