package carisma.check.rabac;

import java.util.HashMap;

public class Attribute {
	private String name;
	private String type;
	private HashMap<String, String> values = new HashMap<>();

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getType() {
		return this.type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public HashMap<String, String> getValues() {
		return this.values;
	}

	public void setValues(HashMap<String, String> values) {
		this.values = values;
	}

}