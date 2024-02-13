package carisma.check.requirescheck;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.UserAbortedAnalysisException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.tests.modelutils.uml.TestHelper;

/**
 * JUnit tests for the required check.
 * @author Klaus Rudack
 *
 */
public class RequiresCheckTest {

	/**
	 * MokHost to save and deliver the model and get reports.
	 * @author CARiSMA
	 *
	 */
	private class TestHost implements AnalysisHost {

		@Override
		public void addResultMessage(final AnalysisResultMessage detail) {
			System.out.println(detail.getText());
			
		}

		@Override
		public void appendToReport(final String text) {
			System.out.println(text);			
		}

		@Override
		public void appendLineToReport(final String text) {
			System.out.println(text);			
		}

		@Override
		public Resource getAnalyzedModel() {
			if (model != null) {
				return model.eResource();
			} else {
				return null;
			}
		}

		@Override
		public String getCurrentModelFilename() {
			return model.eResource().getURI().toFileString();
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
			System.out.println(message);
		}

		@Override
		public File getFileToBeWritten(final File file)
				throws UserAbortedAnalysisException {
			// TODO Auto-generated method stub
			return file;
		}
	}
	
	/**
	 * the path to the testmodel.
	 */
	private String filepath = "resources/models/";
		
	/**
	 * the test model.
	 */
	private Model model;
		
	/**
	 * tests if the check fails if the given model is null.
	 */
	@Test
	public final void testNullPointer() {
		TestHost analysisHost = new TestHost();
		RequiresCheck rc = new RequiresCheck(null);
		assertFalse(rc.perform(null, analysisHost));
	}
	
	/**
	 * tests if the checks works correct with a model without any paths in it.
	 */
	@Test
	public final void testNoPaths() {
		model = TestHelper.loadModel(filepath, "testRequiresNoPath.uml");
		TestHost analysisHost = new TestHost();
		RequiresCheck rc = new RequiresCheck(null);
		assertTrue(rc.perform(null, analysisHost));
	}
	
	/**
	 * tests a violated model.
	 */
	@Test
	public final void testViolated() {
		model = TestHelper.loadModel(filepath, "newTestRequiresViolated.uml");
		TestHost analysisHost = new TestHost();
		RequiresCheck rc = new RequiresCheck(null);
		assertFalse(rc.perform(null, analysisHost));
	}
	
	/**
	 * tests a correct model.
	 */
	@Test
	public final void testCorrect2() {
		model = TestHelper.loadModel(filepath, "testRequiresViolated.uml");
		TestHost analysisHost = new TestHost();
		RequiresCheck rc = new RequiresCheck(null);
		assertTrue(rc.perform(null, analysisHost));
	}
	
	/**
	 * tests a correct model.
	 */
	@Test
	public final void testCorrect() {
		model = TestHelper.loadModel(filepath, "testRequiresCorrect.uml");
		TestHost analysisHost = new TestHost();
		RequiresCheck rc = new RequiresCheck(null);
		assertTrue(rc.perform(null, analysisHost));
	}
	
	/**
	 * tests a model without Stereotype <<requires>>.
	 */
	@Test
	public final void testNoStereotype() {
		model = TestHelper.loadModel(filepath, "testRequiresNoStereotype.uml");
		TestHost analysisHost = new TestHost();
		RequiresCheck rc = new RequiresCheck(null);
		assertTrue(rc.perform(null, analysisHost));
	}
	
	/**
	 * Unloads the model.
	 */
	@After
	public final void cleanUp() {
		if (model != null) {
			TestHelper.unloadModel(model);
			model = null;
		}
	}
	
}
