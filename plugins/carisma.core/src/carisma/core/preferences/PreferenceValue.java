package carisma.core.preferences;

public class PreferenceValue {

	private String name;
	
	private Object value;

	public PreferenceValue(String name, Object value) {
		super();
		this.name = name;
		this.value = value;
	}
	
	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Object getValue() {
		return this.value;
	}

	public void setValue(Object value) {
		this.value = value;
	}

	
}
