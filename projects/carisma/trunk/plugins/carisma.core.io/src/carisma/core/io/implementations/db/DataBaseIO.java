package carisma.core.io.implementations.db;

import carisma.core.io.configuration.Configuration;
import carisma.core.io.content.Content;

public interface DataBaseIO {

	boolean write(Configuration config, Content content);
	Content read(Configuration config);
	ResponseMessage getResponseMessage();
}
