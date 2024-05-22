package carisma.check.policycreation;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.json.JSONObject;

public class ExtendedJSONObject extends JSONObject {
	//no set (in case of parent removel with multiple occurences of one)
	List<ExtendedJSONObject> parents =new LinkedList<ExtendedJSONObject>();
	
	public JSONObject put(String key, ExtendedJSONObject object) {
		super.put(key, object);
		object.addParent(this);
		return this;
	}
	
	public void addParent(ExtendedJSONObject parent) {
		this.parents.add(parent);
	}
	
	
}
