package carisma.check.uconpolicycreation.tests;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.util.Collections;

import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Model;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;

import carisma.check.policycreation.Check;
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
		if (o1.getClass().equals(JSONObject.class)&&o1.getClass().equals(JSONObject.class)) {
			JSONObject jo1 = (JSONObject) o1;
			JSONObject jo2 = (JSONObject) o2;
			if(!jo1.keySet().equals(jo2.keySet())) {
				return false;//Differening
			}
			for (String  key : jo1.keySet()) {
				if(key.equals("@list") && compareJsonLdEqualityOrderedList(jo1.get(key),jo2.get(key))==false) {
					return false;
				}
				else if (compareJsonLdEquality(jo1.get(key), jo2.get(key))==false) {
					return false;
				}
			}
			return true;//no value-mismatch found
			
		}
		else if (o1.getClass().equals(JSONArray.class)&&o2.getClass().equals(JSONArray.class)) {
			JSONArray ja1 = (JSONArray) o1;
			JSONArray ja2 = new JSONArray((JSONArray) o2);
			if (ja1.length()!=ja2.length()) {
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
			return ja2.length()==0;//if all entries are shared all are removed from the 2nd array (same length checked before)
		}
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
							return false;
						}
					}
			}
			return true;
		}
		return false;
	}
	
	@Test
	public final void modelTest() {
		String modelName = "uconpolicycreation-valid-filled.uml";
		assertNull(this.modelres);
		loadModel(this.filePath + File.separator + modelName);
		Check policyCheck = new Check();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(policyCheck.perform(null, analysisHost));
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
		JSONObject externalJson = new JSONObject(content);
		createdJson.put("@context", "http://www.w3.org/ns/odrl.jsonld");
		System.out.println("Program-created:\n" + createdJson.toString(4));
		System.out.println("Text-file-created:\n" + externalJson.toString(4));
		assertTrue(compareJsonLdEquality(createdJson, new JSONObject(content)));
		
	}
	
	@Test
	public void emptyTest() {
		System.out.println("In empty test");
		assertTrue(true);
		assertFalse(true);
	}
}
