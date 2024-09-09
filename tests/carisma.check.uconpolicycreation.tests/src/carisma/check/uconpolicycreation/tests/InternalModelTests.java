package carisma.check.uconpolicycreation.tests;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collections;

import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Model;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Ignore;
import org.junit.Test;

import carisma.check.uconpolicycreation.Check;
import carisma.core.logging.Logger;

public class InternalModelTests {

	private String filePath = "resources" + File.separator + "models";
	

	private ResourceSet rs = new ResourceSetImpl();
	
	private org.eclipse.emf.ecore.resource.Resource modelres = null;

	private Model model = null;
	

	public final void loadModel(final String testmodelname) {
		File testmodelfile = new File(testmodelname);
		assertTrue(testmodelfile.exists());
		try (FileInputStream in = new FileInputStream(testmodelfile)){
			this.modelres = this.rs.createResource(org.eclipse.emf.common.util.URI.createFileURI(testmodelname));
			this.modelres.load(in, Collections.EMPTY_MAP);
		} catch (IOException e) {
			Logger.log(carisma.core.logging.LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
		assertNotNull(this.modelres);
		this.model = (Model) this.modelres.getContents().get(0);
		assertNotNull(this.model);
	}
	
	public final void loadModelWithFilePath(final String testmodelname) {
		loadModel(this.filePath+File.separator+testmodelname);
	}
	
	public final String readResourceFile(String path) {
		try(BufferedReader br = new BufferedReader(new FileReader(path))) {
		    StringBuilder sb = new StringBuilder();
		    String line = br.readLine();

		    while (line != null) {
		        sb.append(line);
		        sb.append(System.lineSeparator());
		        line = br.readLine();
		    }
		    return sb.toString();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	/**
	 * Helper function to check for equality of two Objects in a JSON-LD-structure.
	 * Checks unordered equality of members in {@link JSONObject}s and in {@linkJSONArray}s.
	 * For other inputs simple equality is tested.
	 * 
	 * @param o1 the first object
	 * @param o2 the second object
	 * @return true if the objects are considered equal, otherwise false
	 */
	public static boolean compareJsonLdEquality(Object o1, Object o2) {
		if (o1 == null) {
			return o2 == null;
		}
		if (o1.equals(o2)) {
			return true;
		}
		if (o1.getClass().equals(JSONObject.class)&&o2.getClass().equals(JSONObject.class)) {
			JSONObject jo1 = (JSONObject) o1;
			JSONObject jo2 = (JSONObject) o2;
			if(!jo1.keySet().equals(jo2.keySet())) {
				return false;//Differing
			}
			for (String  key : jo1.keySet()) {
				if(key.equals("@list") && compareJsonLdEqualityOrderedList(jo1.get(key),jo2.get(key))==false) {
					System.out.println("Ordered equality failed: " + o1.toString());
					return false;
				}
				else if (compareJsonLdEquality(jo1.get(key), jo2.get(key))==false) {
					System.out.println("Normal equality failed: " + o1.toString() + "    " + o2.toString());
					return false;
				}
			}
			return true;//no value-mismatch found
			
		}
		else if (o1.getClass().equals(JSONArray.class)&&o2.getClass().equals(JSONArray.class)) {
			JSONArray ja1 = (JSONArray) o1;
			JSONArray ja2 = new JSONArray((JSONArray) o2);
			if (ja1.length()!=ja2.length()) {
				System.out.println("Unequal length: " + o1.toString());
				return false;
			}	
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
			System.out.println("Uneven amount At the end: " + o1.toString());
			return ja2.length()==0;//if all entries are shared all are removed from the 2nd array (same length checked before)
		}
		System.out.println("At the end: " + o1.toString());
		System.out.println("O1: " + o1.getClass() + "  O2: " + o2.getClass());
		System.out.println("O1: " + o1.toString() + "   O2: " + o2.toString());
		return false;//both Elements are not equal() and, both are not JSONObjects or both are not JSONArrays
		
	}
	
	/**
	 * Helper function to check for equality of two Objects in a JSON-LD-structure.
	 * Checks ordered equality of members in {@linkJSONArray}s.
	 * 
	 * @param o1 the first object
	 * @param o2 the second object
	 * @return true if the objects are considered equal JSONArrays, otherwise false
	 */
	public static boolean compareJsonLdEqualityOrderedList(Object o1, Object o2) {
		if (o1.getClass().equals(JSONArray.class)&&o2.getClass().equals(JSONArray.class)) {
			JSONArray ja1 = (JSONArray) o1;
			JSONArray ja2 = new JSONArray((JSONArray) o2);
			if (ja1.length()==ja2.length()) {
					for (int i = 0; i<ja1.length();i++) {
						if (!compareJsonLdEquality(ja1.get(i), ja2.get(i))) {
							System.out.println(o1.toString());
							return false;
						}
					}
			}
			return true;
		}
		System.out.println(o1.toString());
		return false;
	}
	
	
	
	
	@Test
	public final void modelCoverageTestFilled() {
		String modelName = "uconpolicycreation-valid-filled.uml";
		assertNull(this.modelres);
		loadModel(this.filePath + File.separator + modelName);
		Check policyCheck = new Check();
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(policyCheck.perform(null, analysisHost));
		System.out.println(new JSONObject(policyCheck.getPolicyString()).toString(4));
		this.modelres.unload();
	}
	
	@Test
	public final void w3OdrlModelExample1Test() {
		String modelName = "w3OdrlModelExamples/W3OdrlModelExample1.uml";
		assertNull(this.modelres);
		loadModel(this.filePath + File.separator + modelName);
		Check policyCheck = new Check();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(policyCheck.perform(null, analysisHost));
		this.modelres.unload();
		
		String content = readResourceFile(this.filePath + File.separator +"w3OdrlModelExamples/W3OdrlModelExample1.txt");
		JSONObject createdJson = new JSONObject(policyCheck.getPolicyString());
		System.out.println("Program-created:\n" + createdJson.toString(4));
		System.out.println("Text-file-created:\n" + new JSONObject(content).toString(4));
		assertTrue(compareJsonLdEquality(createdJson, new JSONObject(content)));
		
	}
	
	@Test
	public final void w3OdrlModelExample3Test() {
		String modelName = "w3OdrlModelExamples/W3OdrlModelExample3.uml";
		assertNull(this.modelres);
		loadModel(this.filePath + File.separator + modelName);
		Check policyCheck = new Check();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(policyCheck.perform(null, analysisHost));
		this.modelres.unload();
		
		String content = readResourceFile(this.filePath + File.separator +"w3OdrlModelExamples/W3OdrlModelExample3.txt");
		JSONObject createdJson = new JSONObject(policyCheck.getPolicyString());
		System.out.println("Program-created:\n" + createdJson.toString(4));
		System.out.println("Text-file-created:\n" + new JSONObject(content).toString(4));
		assertTrue(compareJsonLdEquality(createdJson, new JSONObject(content)));
		
	}
	
	@Test
	public final void w3OdrlModelExample13Test() {
		String modelName = "w3OdrlModelExamples/W3OdrlModelExample13.uml";
		assertNull(this.modelres);
		loadModel(this.filePath + File.separator + modelName);
		Check policyCheck = new Check();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(policyCheck.perform(null, analysisHost));
		this.modelres.unload();
		
		String content = readResourceFile(this.filePath + File.separator +"w3OdrlModelExamples/W3OdrlModelExample13.txt");
		JSONObject createdJson = new JSONObject(policyCheck.getPolicyString());
		System.out.println("Program-created:\n" + createdJson.toString(4));
		System.out.println("Text-file-created:\n" + new JSONObject(content).toString(4));
		assertTrue(compareJsonLdEquality(createdJson, new JSONObject(content)));
		
	}
	
	@Test
	public final void w3OdrlModelExample14Test() {
		String modelName = "w3OdrlModelExamples/W3OdrlModelExample14.uml";
		assertNull(this.modelres);
		loadModel(this.filePath + File.separator + modelName);
		Check policyCheck = new Check();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(policyCheck.perform(null, analysisHost));
		this.modelres.unload();
		
		String content = readResourceFile(this.filePath + File.separator +"w3OdrlModelExamples/W3OdrlModelExample14.txt");
		JSONObject createdJson = new JSONObject(policyCheck.getPolicyString());
		System.out.println("Program-created:\n" + createdJson.toString(4));
		System.out.println("Text-file-created:\n" + new JSONObject(content).toString(4));
		assertTrue(compareJsonLdEquality(createdJson, new JSONObject(content)));
		
	}
	
	@Test
	@Ignore("Not yet implemented: RightOperand-Classes")
	public final void w3OdrlModelExample22Test() {
		String modelName = "w3OdrlModelExamples/W3OdrlModelExample22.uml";
		assertNull(this.modelres);
		loadModel(this.filePath + File.separator + modelName);
		Check policyCheck = new Check();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(policyCheck.perform(null, analysisHost));
		this.modelres.unload();
		
		String content = readResourceFile(this.filePath + File.separator +"w3OdrlModelExamples/W3OdrlModelExample22.txt");
		JSONObject createdJson = new JSONObject(policyCheck.getPolicyString());
		createdJson.put("@context", "http://www.w3.org/ns/odrl.jsonld");
		System.out.println("Program-created:\n" + createdJson.toString(4));
		System.out.println("Text-file-created:\n" + new JSONObject(content).toString(4));
		assertTrue(compareJsonLdEquality(createdJson, new JSONObject(content)));
		
	}
}
