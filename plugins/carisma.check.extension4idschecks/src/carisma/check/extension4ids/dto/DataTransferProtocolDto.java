package carisma.check.extension4ids.dto;

import java.util.List;
import java.util.Set;

import org.eclipse.uml2.uml.Interaction;
import org.eclipse.uml2.uml.Lifeline;
import org.eclipse.uml2.uml.Message;

public class DataTransferProtocolDto {
	private Lifeline provider;
	private Lifeline consumer;
	
	public DataTransferProtocolDto(Lifeline provider, Lifeline consumer) {
		this.consumer = consumer;
		this.provider = provider;
	}
	
	public Lifeline getProvider() {
		return provider;
	}
	public void setProvider(Lifeline provider) {
		this.provider = provider;
	}
	public Lifeline getConsumer() {
		return consumer;
	}
	public void setConsumer(Lifeline consumer) {
		this.consumer = consumer;
	}
	
}
