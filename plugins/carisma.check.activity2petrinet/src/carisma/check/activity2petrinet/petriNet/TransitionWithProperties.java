package carisma.check.activity2petrinet.petriNet;

import java.util.HashMap;

/**
 * Meine Klasse!
 * Benutzung auf eigene Gefahr.
 * @author thumberg
 *
 */

public class TransitionWithProperties extends Transition {
	private HashMap<String, Object> properties = new HashMap<>();

	public HashMap<String, Object> getProperties() {
		return this.properties;
	}
	
	public void setProperty(String key, Object value){
		this.properties.put(key, value);
	}
	
	public Object getProperty(String key){
		return this.properties.get(key);
	}

	public boolean getBooleanProperty(String key){
		return ((Boolean)this.properties.get(key)).booleanValue();
	}

	public boolean hasBooleanProperty(String key){
		if(this.properties.get(key) != null)
			return this.getBooleanProperty(key);
		return false;
	}

	public TransitionWithProperties(String id, Graphics graphics, String name) {
		super(id, graphics, name);
		// TODO Auto-generated constructor stub
	}

	public TransitionWithProperties(String type, String id, Graphics graphics,
			String name) {
		super(type, id, graphics, name);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
