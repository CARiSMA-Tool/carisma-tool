package carisma.check.staticcheck.nodownflow;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.Collections;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Package;
import org.junit.After;
import org.junit.Test;

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
	private ResourceSet rs = new ResourceSetImpl();
	
	/**
	 * the model-resource.
	 */
	private Resource modelres = null;
	
	/**
	 * loads a model, represented by the filename.
	 * @param testmodelname - name of the model-file
	 */
	public final void loadModel(final String testmodelname) throws IOException {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
	}
	
	/**
	 * tests if the class throws a NullPointerException if the model is null.
	 */
	@SuppressWarnings("javabugs:S6416")
	@Test (expected = IllegalArgumentException.class)
	public final void testNull() {
		NoDownFlow ndf = new NoDownFlow();
		ndf.startCheck(null, null);
	}
	
	/**
	 * tests if the class returns true if no profile is applied.
	 * @throws IOException 
	 */
	@Test
	public final void testNoProfile() throws IOException {
		loadModel("testNoDownFlowNoProfile.uml");
		assertNotNull(this.modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertTrue(ndf.startCheck((Package) this.modelres.getContents().get(0), null));
		this.modelres.unload();
	}
	
	/**
	 * tests if the class returns true if no stereotype is applied.
	 * @throws IOException 
	 */
	@Test
	public final void testNoStereotype() throws IOException {
		loadModel("testNoDownFlowNoStereotype.uml");
		assertNotNull(this.modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertTrue(ndf.startCheck((Package) this.modelres.getContents().get(0), null));
		this.modelres.unload();
	}
	
	/**
	 * test with the example model of the umlsec book.
	 * @throws IOException 
	 */
	@Test
	public final void test() throws IOException {
		loadModel("testNoDownFlow.uml");
		assertNotNull(this.modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertFalse(ndf.startCheck((Package) this.modelres.getContents().get(0), null));
		this.modelres.unload();
	}
	
	/**
	 * test with a correct example.
	 * @throws IOException 
	 */
	@Test
	public final void testCorrect() throws IOException {
		loadModel("testNoDownFlowCorrect.uml");
		assertNotNull(this.modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertTrue(ndf.startCheck((Package) this.modelres.getContents().get(0), null));
		this.modelres.unload();
	}
	
	/**
	 * test with an example where the algorithm should say correct, but isn't.
	 * @throws IOException 
	 */
	@Test
	public final void test4Algo() throws IOException {
		loadModel("testNoDownFlowFalseAlgoExample.uml");
		assertNotNull(this.modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertTrue(ndf.startCheck((Package) this.modelres.getContents().get(0), null));
		this.modelres.unload();
	}
	 
	/**
	 * model example of the book.
	 * should fail
	 * @throws IOException 
	 */
	@Test
	public final void testBookExample() throws IOException {
		loadModel("testNoDownFlowBookExample.uml");
		assertNotNull(this.modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertFalse(ndf.startCheck((Package) this.modelres.getContents().get(0), null));
		this.modelres.unload();
	}
	
	/**
	 * model for a contains() check.
	 * should succeed
	 * @throws IOException 
	 */
	@Test
	public final void testContains() throws IOException {
		loadModel("testNoDownFlowContains.uml");
		assertNotNull(this.modelres);
		NoDownFlow ndf = new NoDownFlow();
		assertTrue(ndf.startCheck((Package) this.modelres.getContents().get(0), null));
		this.modelres.unload();
	}
	
	@After
	public void unloadModel(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}
}
