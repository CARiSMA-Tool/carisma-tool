package carisma.check.policycreation;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;

public class TestClass {

	public static void main(String[] args) {
		
//		ODRLCommonVocabularyPackage odrlPackage = ODRLCommonVocabularyPackage.eINSTANCE;
//		System.out.println(odrlPackage.getAsset_Uid().getName());
//		System.out.println(odrlPackage.getPermission().getName());

//		try {
//			throw new NoSuchFieldException("bye");
//		} catch (NoSuchFieldException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (SecurityException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
		
		
		
//		Class<? extends Policy> klass = Offer.class;
//		for (Constructor c : klass.getConstructors()) {
//			System.out.println(c);
//			
//		}
//		System.out.println(klass.getConstructors());
		
		
		
		
//		Map<String, Object> topLevelMap = new HashMap<>();
//		topLevelMap.put("object1", Map.ofEntries(
//				Map.entry("Test", "hi"),
//				Map.entry("Array", new int[]{3,5,6}),
//				Map.entry("List",Arrays.asList(new String[] {"String1","String2"})),
//				Map.entry("FurtherNesting", Map.ofEntries(Map.entry("down", "downvalue")
//						))
//				));
//				
//		JSONObject jo = new JSONObject(topLevelMap);
//		System.out.println(jo.toString(4));
		
//		switch(topLevelMap.get("hi").getClass().getSimpleName()) {
//		
//		}
		
		
		
//		JSONObject jo = new JSONObject();
//		JSONObject jo2 = new JSONObject();
//		jo.put("HI",jo2);
//		jo2.put("@id","hi3");
//		
//		System.out.println(jo.toString(4));
		
		Map<String,JSONObject> jmap = new HashMap();
		List<String> jList1 = new LinkedList<>();
		jList1.add("hi");
		jList1.add("bye");
		List<String> jList2 = new LinkedList<>();
		jList2.add("bye");
		jList2.add("hi");
		JSONArray jArray0 = new JSONArray();
		jArray0.put("hi");
		jArray0.put("bye");
		JSONArray jArray1 = new JSONArray(jList1);
		JSONArray jArray2 = new JSONArray(jList2);
		System.out.println(jArray0.toString());
		System.out.println(jArray1.toString());
		System.out.println(jArray2.toString());
		System.out.println("0 equals 1?: " + jArray0.similar(jArray1));
		System.out.println("0 equals 2?: " + jArray0.similar(jArray2));
		System.out.println("1 equals 2?: " + jArray1.similar(jArray2));
		System.out.println("0 equals 1?(unordered): " + compareJsonLdEquality(jArray0, jArray1));
		System.out.println("0 equals 2?(unordered): " + compareJsonLdEquality(jArray0, jArray2));
		System.out.println("1 equals 2?(unordered): " + compareJsonLdEquality(jArray1, jArray2));
		System.out.println("0 equals 1?(ordered): " + compareJsonLdEqualityOrderedList(jArray0, jArray1));
		System.out.println("0 equals 2?(ordered): " + compareJsonLdEqualityOrderedList(jArray0, jArray2));
		System.out.println("1 equals 2?(ordered): " + compareJsonLdEqualityOrderedList(jArray1, jArray2));
		
		JSONObject jso1 = new JSONObject();
		JSONObject jso2 = new JSONObject();
		jso1.put("bye","hi");
		jso2.put("bye", "hi");
		jso2.put("hi", "bye");
		jso2.put("ai22222", "bye2");

//		System.out.println(jso1.toString(4));
//		System.out.println();
//		System.out.println(jso2.toString(4));
		
		
		/*
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
		System.out.println(topObj.toString(2));
		//topObj.
		 */
		 
		
		
//		JSONConvTest test = new JSONConvTest("");
//		String[] allowedAttributes = {"stringAttr"};
//		System.out.println(new JSONObject(test).toString(4));
//		System.out.println();
//		System.out.println(TestClass.class.getSimpleName());
		
		
		/*
		List<String> slist = new LinkedList<String>();
		List<List<String>> llist =  new LinkedList<List<String>>();
		slist.add("Hi");
		llist.add(slist);
		System.out.println(new JSONObject(llist));
		*/
		
		
		
//		JSONConvTest o1 = new JSONConvTest();
//		JSONConvTest o2 = new ExtensionTestClass();
//		System.out.println("1");
//		testMethod(o1);
//		System.out.println("2");
//		testMethod(o2);
		
//		Integer i = 4;
//		Map<Object,Object> newMap = new HashMap<Object, Object>();
//		newMap.put("hi", i);
//		JSONObject nj = new JSONObject(newMap);
//		System.out.println(nj.toString(4));
		
	}
	public static class JSONConvTest {
		public String stringAttr = "Hi";
		String stringAttrEmpty;
		
		List<List<JSONConvTest>> testClasses;
		List<List<JSONConvTest>> testClassesEmpty;
		List<JSONConvTest> list = new LinkedList<TestClass.JSONConvTest>();
		List<JSONConvTest> listEmpty;
		
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
		public List<JSONConvTest> getList() {
			return list;
		}
		public void setList(List<JSONConvTest> list) {
			this.list = list;
		}
		
	}
	public static class ExtensionTestClass extends JSONConvTest {
		public String substring = "subst";
	}
	
	public static <T>  void testMethod(T input) {
		if (input instanceof ExtensionTestClass) {
			System.out.println("Is Extension");
		}
		
	}
	
	public static boolean compareJsonLdEquality(Object o1, Object o2) {
		if (o1 == null) {
			return o2 == null;
		}
		if (o1.equals(o2)) {
			return true;
		}
		if (o1.getClass().equals(JSONObject.class)&&o1.getClass().equals(JSONObject.class)) {
			JSONObject jo1 = (JSONObject) o1;
			JSONObject jo2 = (JSONObject) o2;
			if(jo1.keySet().equals(jo2.keySet())) {
				for (String  key : jo1.keySet()) {
					if(key.equals("@list") && compareJsonLdEqualityOrderedList(jo1.get(key),jo2.get(key))==false) {
						return false;
					}
					else if (compareJsonLdEquality(jo1.get(key), jo2.get(key))==false) {
						return false;
					}
				}
			}
			return true;
		}
		else if (o1.getClass().equals(JSONArray.class)&&o2.getClass().equals(JSONArray.class)) {
			JSONArray ja1 = (JSONArray) o1;
			JSONArray ja2 = new JSONArray((JSONArray) o2);
			if (ja1.length()==ja2.length()) {
				for (Object ao1 : ja1) {
					int removeIndex = -1;
					for (int i = 0; i<ja2.length();i++) {
						if (compareJsonLdEquality(ao1, ja2.get(i))) {
							removeIndex = i;
							break;
						}
					}
					if (removeIndex != -1) {
						ja2.remove(removeIndex);
						
					}
				}
			}
			return ja2.length()==0;
		}
		return false;
		
	}
	public static boolean compareJsonLdEqualityOrderedList(Object o1, Object o2) {
		if (o1.getClass().equals(JSONArray.class)&&o2.getClass().equals(JSONArray.class)) {
			JSONArray ja1 = (JSONArray) o1;
			JSONArray ja2 = new JSONArray((JSONArray) o2);
			if (ja1.length()==ja2.length()) {
					for (int i = 0; i<ja1.length();i++) {
						if (!compareJsonLdEquality(ja1.get(i), ja2.get(i))) {
							return false;
						}
					}
			}
			return true;
		}
		return false;
	}

}
