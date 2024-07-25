package carisma.check.uconpolicycreation.tests;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collections;

import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Model;
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
	
	@Test
	public final void modelTest() {
		String modelName = "profregtest.uml";
		assertNull(this.modelres);
		loadModel(this.filePath + File.separator + modelName);
		Check policyCheck = new Check();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(policyCheck.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	@Test
	public void emptyTest() {
		System.out.println("In empty test");
		assertTrue(true);
		assertFalse(true);
	}
}
