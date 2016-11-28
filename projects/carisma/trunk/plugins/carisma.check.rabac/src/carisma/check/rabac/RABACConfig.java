package carisma.check.rabac;

import java.util.HashMap;
import java.util.HashSet;

import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "rabac")
public class RABACConfig {
	private HashMap<String, SetWrapper> sessions = new HashMap<String, SetWrapper>();
	private HashSet<Attribute> attributes = new HashSet<Attribute>();

	public HashMap<String, SetWrapper> getSessions() {
		return this.sessions;
	}

	// without a setter marshalling fails
	public void setSessions(HashMap<String, SetWrapper> sessions) {
		this.sessions = sessions;
	}

	public HashSet<Attribute> getAttributes() {
		return this.attributes;
	}

	public void setAttributes(HashSet<Attribute> attributes) {
		this.attributes = attributes;
	}

}