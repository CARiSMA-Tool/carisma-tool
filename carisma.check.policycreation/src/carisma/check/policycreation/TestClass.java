package carisma.check.policycreation;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
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
		JSONConvTest test = new JSONConvTest("");
		String[] allowedAttributes = {"stringAttr"};
		System.out.println(new JSONObject(test, allowedAttributes));
		System.out.println();
		System.out.println(TestClass.class.getSimpleName());
		
		
	}
	public static class JSONConvTest {
		public String stringAttr = "Hi";
		String stringAttrEmpty;
		
		List<List<JSONConvTest>> testClasses;
		List<List<JSONConvTest>> testClassesEmpty;
		
		public JSONConvTest(String s) {
			JSONConvTest testclass = new JSONConvTest();
			testClasses= new LinkedList<List<JSONConvTest>>();
			List<JSONConvTest> firstLevel = new LinkedList<JSONConvTest>();
			testClasses.add(firstLevel);
			firstLevel.add(testclass);
		}
		public JSONConvTest() {
		}
		public String getStringAttr() {
			return stringAttr;
		}
		public void setStringAttr(String stringAttr) {
			this.stringAttr = stringAttr;
		}
		public String getStringAttrEmpty() {
			return stringAttrEmpty;
		}
		public void setStringAttrEmpty(String stringAttrEmpty) {
			this.stringAttrEmpty = stringAttrEmpty;
		}
		public List<List<JSONConvTest>> getTestClasses() {
			return testClasses;
		}
		public void setTestClasses(List<List<JSONConvTest>> testClasses) {
			this.testClasses = testClasses;
		}
		public List<List<JSONConvTest>> getTestClassesEmpty() {
			return testClassesEmpty;
		}
		public void setTestClassesEmpty(List<List<JSONConvTest>> testClassesEmpty) {
			this.testClassesEmpty = testClassesEmpty;
		}		
	}
	public static class ExtensionTestClass extends JSONConvTest {
		public String substring = "subst";
	}

}
