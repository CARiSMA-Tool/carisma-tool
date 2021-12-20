package carisma.check.rabac;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "rabac")
@XmlAccessorType(XmlAccessType.FIELD)
public class RABACConfig {
	private Map<String, SetWrapper> sessions = new HashMap<>();
	private Set<Attribute> attributes = new HashSet<>();

	public Map<String, SetWrapper> getSessions() {
		return this.sessions;
	}

	// without a setter marshalling fails
	public void setSessions(final Map<String, SetWrapper> sessions) {
		this.sessions = sessions;
	}

	public Set<Attribute> getAttributes() {
		return this.attributes;
	}

	public void setAttributes(final Set<Attribute> attributes) {
		this.attributes = attributes;
	}

}