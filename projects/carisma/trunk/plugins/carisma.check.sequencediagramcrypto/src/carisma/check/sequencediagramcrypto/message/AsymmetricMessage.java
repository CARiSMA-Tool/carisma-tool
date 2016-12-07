package carisma.check.sequencediagramcrypto.message;

import java.util.List;

public class AsymmetricMessage extends AbstractMessage {
	
	public AsymmetricMessage(String name, List<Argument> arguments, Peer source, Peer target,
			int msgNo, Guard guard) {
		super(name, arguments, source, target, msgNo, guard, false);
	}
	
	@Override
	public int compareTo(Message o) {
		return this.messageNumber() - o.messageNumber();
	}
	
}
