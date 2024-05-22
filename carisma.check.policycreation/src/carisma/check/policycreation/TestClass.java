package carisma.check.policycreation;

import java.util.HashMap;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;

public class TestClass {

	public static void main(String[] args) {
		Map<String,JSONObject> jmap = new HashMap();
		JSONObject jso = new JSONObject();
		JSONObject jso2 = new JSONObject();
		jmap.put("jso2",jso2);
		jso.append("first", jso2);
		jso2.append("first",3);
		jmap.get("jso2").append("third", "test2");
		JSONObject jso3 = new JSONObject();
		jso2.append("second", jso3);
		jso3.append("first", "test");
		jso3.append("second", "test2");
		jso.append("duplicate", jso3);
		JSONArray topObj = new JSONArray();
		topObj.put(jso);
		//topObj.
		
		System.out.println(topObj.toString(2));
		
		
	}

}
