package carisma.check.staticcheck.nodownflow;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Package;
import org.junit.Test;

import carisma.check.staticcheck.nodownflow.NoDownFlow;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.UML2ModelLoader;


/**
 * JUnit test-case for NoDownFlow.
 * @author Klaus Rudack
 *
 */
public class NoDownFlowTest {

	/**
	 * path to the model-folder.
	 */
	private String filepath = "resources/models/no_down_flow";
	
	/**
	 * UML2ModelLoader.
	 */
	private UML2ModelLoader ml = null;
	
	/**
	 * the model-resource.
	 */
	private Resource modelres = null;
	
	/**
	 * loads a model, represented by the filename.
	 * @param testmodelname - name of the model-file
	 */
	public final void loadModel(final String testmodelname) {
		File testmodelfile = new File(filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		if (ml == null) {
			ml = new UML2ModelLoader();
		}
		try {
			modelres = ml.load(testmodelfile);
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
	}
	
	/**
	 * tests if the class throws a NullPointerException if the model is null.
	 */
	@Test (expected = IllegalArgumentException.class)
	public final void testNull() {
		NoDownFlow ndf = new NoDownFlow();
		ndf.startCheck(null, null);
	}
	
	/**
	 * tests if the class returns true if no profile is applied.
	 */
	@Test
	public final void testNoProfile() {
		loadModel("testNoDownFlowNoProfile.uml");
		assertNotNull(modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertTrue(ndf.startCheck((Package) modelres.getContents().get(0), null));
		modelres.unload();
	}
	
	/**
	 * tests if the class returns true if no stereotype is applied.
	 */
	@Test
	public final void testNoStereotype() {
		loadModel("testNoDownFlowNoStereotype.uml");
		assertNotNull(modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertTrue(ndf.startCheck((Package) modelres.getContents().get(0), null));
		modelres.unload();
	}
	
	/**
	 * test with the example model of the umlsec book.
	 */
	@Test
	public final void test() {
		loadModel("testNoDownFlow.uml");
		assertNotNull(modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertFalse(ndf.startCheck((Package) modelres.getContents().get(0), null));
		modelres.unload();
	}
	
	/**
	 * test with a correct example.
	 */
	@Test
	public final void testCorrect() {
		loadModel("testNoDownFlowCorrect.uml");
		assertNotNull(modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertTrue(ndf.startCheck((Package) modelres.getContents().get(0), null));
		modelres.unload();
	}
	
	/**
	 * test with an example where the algorithm should say correct, but isn't.
	 */
	@Test
	public final void test4Algo() {
		loadModel("testNoDownFlowFalseAlgoExample.uml");
		assertNotNull(modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertTrue(ndf.startCheck((Package) modelres.getContents().get(0), null));
		modelres.unload();
	}
	 
	/**
	 * model example of the book.
	 * should fail
	 */
	@Test
	public final void testBookExample() {
		loadModel("testNoDownFlowBookExample.uml");
		assertNotNull(modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertFalse(ndf.startCheck((Package) modelres.getContents().get(0), null));
		modelres.unload();
	}
	
	/**
	 * model for a contains() check.
	 * should succeed
	 */
	@Test
	public final void testContains() {
		loadModel("testNoDownFlowContains.uml");
		assertNotNull(modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertTrue(ndf.startCheck((Package) modelres.getContents().get(0), null));
		modelres.unload();
	}
}
