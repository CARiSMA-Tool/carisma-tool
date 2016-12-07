package carisma.check.sequencediagramcrypto.filter.message;

import carisma.check.sequencediagramcrypto.filter.Filter;
import carisma.check.sequencediagramcrypto.message.Message;
import carisma.check.sequencediagramcrypto.message.Peer;

public class SelectSource implements Filter<Message> {
	
	final private Filter<Peer>	filter;
	
	public SelectSource(Filter<Peer> filter) {
		this.filter = filter;
	}
	
	@Override
	public boolean accept(Message o) {
		return this.filter.accept(o.source());
	}
	
}
