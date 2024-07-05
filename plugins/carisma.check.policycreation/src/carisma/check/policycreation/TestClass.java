package carisma.check.policycreation;

import java.lang.reflect.Constructor;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.ui.internal.handlers.WizardHandler.New;
import org.json.JSONArray;
import org.json.JSONObject;

import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Offer;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Policy;

public class TestClass {

	public static void main(String[] args) {
		
//		ODRLCommonVocabularyPackage odrlPackage = ODRLCommonVocabularyPackage.eINSTANCE;
//		System.out.println(odrlPackage.getAsset_Uid().getName());
//		System.out.println(odrlPackage.getPermission().getName());

		try {
			throw new NoSuchFieldException("bye");
		} catch (NoSuchFieldException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
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

}
