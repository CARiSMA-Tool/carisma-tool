package carisma.check.oclcheck;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.lang.reflect.Method;

import org.junit.Test;

import carisma.ocl.library.OclLibrary;

public class LoadOclLibraryTest {
	/**
	 * The path to the model directory. 
	 */
	private String filepath = "resources/models/";
	
	/**
	 * Test will load a dummy OCL Library and will compare the entries.
	 */
	@Test 
	public final void loadOclLibraryTest() {
		String testmodelfilename = "testLibrary.col";
		File testmodelfile = new File(this.filepath + File.separator + testmodelfilename);
		assertTrue(testmodelfile.exists());
		
		OclLibrary oclLibrary = null;
		
		MultiOclChecker oclChecker = new MultiOclChecker();
		String methodName = "getOclLibrary";
		Method[] methods  = oclChecker.getClass().getDeclaredMethods();
		int methodIndex = -1;
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methods[i].setAccessible(true);
				methodIndex = i;

				try {
					oclLibrary = (OclLibrary) methods[i].invoke(oclChecker, testmodelfile);
					
					assertNotNull(oclLibrary);
					assertTrue(oclLibrary.getName().equals("Library"));
					assertTrue(oclLibrary.getOclExpressions().get(0).getContext().equals("context"));
					assertTrue(oclLibrary.getOclExpressions().get(0).getDescription().equals("description"));
					assertTrue(oclLibrary.getOclExpressions().get(0).getName().equals("name"));
					assertTrue(oclLibrary.getOclExpressions().get(0).getQuery().equals("query"));
				} catch (Exception e) {
					fail("Error during invoke");
				}
				break;
			}
		}
		
		assertNotSame(Integer.valueOf(-1), Integer.valueOf(methodIndex));
	}
}
