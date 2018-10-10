package carisma.core.io.implementations.db;

import carisma.core.io.content.Content;

public interface DataBaseIO {

	boolean write(Destination config, Content content);
	Content read(Destination config);
	ResponseMessage getResponseMessage();
	
	public interface Destination{
		
	}
}
