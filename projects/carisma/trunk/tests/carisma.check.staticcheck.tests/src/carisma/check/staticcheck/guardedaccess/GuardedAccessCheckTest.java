package carisma.check.staticcheck.guardedaccess;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.junit.Test;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.UserAbortedAnalysisException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;




/**
 * JUnit test-file for the FairExchange plugin.
 * @author Klaus Rudack
 *
 */
public class GuardedAccessCheckTest {
	
	/** Path to the testmodel folder. */
	private String filepath = "resources" + File.separator + "models" + File.separator + "guarded_access";
	
	/** UML2ModelLoader. */
	private UML2ModelLoader ml = null;
	
	/** The modelresource. */
	private Resource modelres = null;
	
	/** The model. */
	private Model model = null;

	/**
	 * MokHost for a Test that generates output and also returns the loaded model-resource.
	 * @author CARiSMA
	 *
	 */
	private class TestHost implements AnalysisHost {

		@Override
		public void addResultMessage(final AnalysisResultMessage detail) {
			Logger.log(LogLevel.INFO, detail.getText());
		}

		@Override
		public void appendToReport(final String text) {
			Logger.log(LogLevel.INFO, text);			
		}

		@Override
		public void appendLineToReport(final String text) {
			Logger.log(LogLevel.INFO, text);			
		}

		@Override
		public Resource getAnalyzedModel() {
			return GuardedAccessCheckTest.this.modelres;
		}

		@Override
		public String getCurrentModelFilename() {
			return GuardedAccessCheckTest.this.modelres.getURI().toFileString();
		}

		@Override
		public void putToRegister(final String registerName, final Object data)
				throws RegisterInUseException {
			// TODO Auto-generated method stub
			
		}

		@Override
		public boolean isRegisterInUse(final String registerName) {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public Object getFromRegister(final String registerName)
				throws RegisterNotInUseException {
			return null;
		}

		@Override
		public Object removeFromRegister(final String registerName)
				throws RegisterNotInUseException {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public void displayError(final String message) {
			// TODO Auto-generated method stub
			Logger.log(LogLevel.INFO, message);
		}

		@Override
		public File getFileToBeWritten(final File file)
				throws UserAbortedAnalysisException {
			// TODO Auto-generated method stub
			return file;
		}
	}
	
	/**
	 * Method to load a model from an UML file.
	 * @param testmodelname - the name of the UML file
	 */
	public final void loadModel(final String testmodelname) {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		if (this.ml == null) {
			this.ml = new UML2ModelLoader();
		}
		try {
			this.modelres = this.ml.load(testmodelfile);
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
		assertNotNull(this.modelres);
		this.model = (Model) this.modelres.getContents().get(0);
		assertNotNull(this.model);
	}
	
	
	/**
	 * This test tests if the check will return false if the given model is null.
	 */
	@Test
	public final void testNullModel() {
		assertNull(this.modelres);
		GuardedAccessCheck check = new GuardedAccessCheck();
		TestHost analysisHost = new TestHost();
		assertFalse(check.perform(null, analysisHost));
	}
	
	/**
	 * This test tests if the check will return true if the given model has no Stereotype applied.
	 */
	@Test
	public final void testNoStereotype() {
		assertNull(this.modelres);
		loadModel("testGuardedAccessNoStereotype.uml");
		GuardedAccessCheck check = new GuardedAccessCheck();
		StereotypeApplication guardedAccessApp = UMLsecUtil.getStereotypeApplication(this.model, UMLsec.GUARDED_ACCESS);
		assertNull(guardedAccessApp);
		TestHost analysisHost = new TestHost();
		assertTrue(check.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	/**
	 * Tests if the test returns false if the model is incorrect.
	 * In this example, the element referenced by the guard value 'obj' is stereotyped guarded in StateMachine 'JavaSecArch".
	 * The method referenced by the effect belongs to a class that is not part of the guarded tag of the &lt;&lt;guarded&gt;&gt; stereotype.
	 */
	@Test
	public final void testFail() {
		assertNull(this.modelres);
		loadModel("testGuardedAccess1.uml");
		GuardedAccessCheck check = new GuardedAccessCheck();
		StereotypeApplication guardedAccessApp = UMLsecUtil.getStereotypeApplication(this.model, UMLsec.GUARDED_ACCESS);
		assertNotNull(guardedAccessApp);
		TestHost analysisHost = new TestHost();
		assertFalse(check.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	/**
	 * This test tests if the check returns true when checking an empty model.
	 */
	@Test
	public final void testSuccessEmptyModel() {
		assertNull(this.modelres);
		loadModel("testGuardedAccess2.uml");
		GuardedAccessCheck check = new GuardedAccessCheck();
		StereotypeApplication guardedAccessApp = UMLsecUtil.getStereotypeApplication(this.model, UMLsec.GUARDED_ACCESS);
		assertNotNull(guardedAccessApp);
		TestHost analysisHost = new TestHost();
		assertTrue(check.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	/**
	 * This test checks a correct model.
	 */
	@Test
	public final void testSuccess() {
		assertNull(this.modelres);
		loadModel("testGuardedAccess3.uml");
		GuardedAccessCheck check = new GuardedAccessCheck();
		StereotypeApplication guardedAccessApp = UMLsecUtil.getStereotypeApplication(this.model, UMLsec.GUARDED_ACCESS);
		assertNotNull(guardedAccessApp);
		TestHost analysisHost = new TestHost();
		assertTrue(check.perform(null, analysisHost));
		this.modelres.unload();
	}
	
}
