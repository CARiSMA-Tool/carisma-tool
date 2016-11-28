package carisma.core.io.configuration;

import java.net.URI;

public interface Configuration {
	
	URI buildUrl(Action action);
	
	public interface Action {
		String getAction();
	}
}
