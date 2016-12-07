package carisma.check.sequencediagramcrypto.message;

import java.util.List;

public interface Message extends Comparable<Message> {
	
	public String name();
	
	public List<Argument> arguments();
	
	public boolean isAfter(Message message);
	
	public boolean isBefore(Message message);
	
	public Peer source();
	
	public Peer target();
	
	public boolean symmetric();
	
	public int messageNumber();
	
	public Guard guard();
	
}
