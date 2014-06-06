package carisma.check.staticcheck.fairexchange;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.junit.Test;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.UserAbortedAnalysisException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;




/**
 * JUnit test-file for the FairExchange plugin.
 * @author Klaus Rudack
 *
 */
public class FairExchangeTest {

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
			return modelres;
		}

		@Override
		public String getCurrentModelFilename() {
			return modelres.getURI().toFileString();
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
	 * path to the testmodel folder.
	 */
	private String filepath = "resources/models/fair_exchange";
	
	/**
	 * UML2ModelLoader.
	 */
	private UML2ModelLoader ml = null;
	
	/**
	 * the modelresource.
	 */
	private Resource modelres = null;
	
	/**
	 * the model.
	 */
	private Model model = null;

	
	/**
	 * method to load a model from a UML file.
	 * @param testmodelname - the name of the UML file
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
		assertNotNull(modelres);
		model = (Model) modelres.getContents().get(0);
		assertNotNull(model);
	}
	
	/**
	 * Tests if the test returns true if the model is correct.
	 */
	@Test
	public final void testSuccess() {
		loadModel("testFairExchangeSuccess.uml");
		FairExchangeCheck fe = new FairExchangeCheck();
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(fairExchangeApp);
		TestHost analysisHost = new TestHost();
		assertTrue(fe.perform(null, analysisHost));
		modelres.unload();
	}
	
	/**
	 * Tests if the test returns false if the model is false with respect to fair exchange.
	 */
	@Test
	public final void testViolated() {
		TestHost analysisHost = new TestHost();
		loadModel("testFairExchangeViolated.uml");
		FairExchangeCheck fe = new FairExchangeCheck();
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(fairExchangeApp);
		assertFalse(fe.perform(null, analysisHost));
		modelres.unload();
	}
	
	/**
	 * tests if the check fails if the given model is null.
	 */
	@Test
	public final void testNull() {
		TestHost analysisHost = new TestHost();
		FairExchangeCheck fe = new FairExchangeCheck();
		assertFalse(fe.perform(null, analysisHost));
	}
	
	/**
	 * Tests if the test returns true when no profile or stereotype is applied.
	 */
	@Test
	public final void testNoProfileStereotype() {
		loadModel("testFairExchangeNoProfile.uml");
		TestHost analysisHost = new TestHost();
		FairExchangeCheck fe = new FairExchangeCheck();
		assertTrue(fe.perform(null, analysisHost));
		modelres.unload();
		loadModel("testFairExchangeNoStereotype.uml");
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(model, UMLsec.FAIR_EXCHANGE);
		assertNull(fairExchangeApp);
		assertTrue(fe.perform(null, analysisHost));
		modelres.unload();
	}
	
	/**
	 * Tests if the test returns false when the start-tag of the stereotype fair-exchange are empty.
	 */
	@Test
	public final void testStartTag() {
		loadModel("testFairExchangeNoStart.uml");
		TestHost analysisHost = new TestHost();
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(fairExchangeApp);
		EObjectResolvingEList<?> start = (EObjectResolvingEList<?>) model.getValue(model.getAppliedStereotype("UMLsec::fair exchange"), "start");
		FairExchangeCheck fe = new FairExchangeCheck();
		assertTrue(start.size() == 0);
		assertFalse(fe.perform(null, analysisHost));
		modelres.unload();
	}
	
	/**
	 * Tests if the test returns false when the stop-tag of the stereotype fair-exchange are empty.
	 */
	@Test
	public final void testStopTag() {
		loadModel("testFairExchangeNoStop.uml");
		TestHost analysisHost = new TestHost();
		
		EObjectResolvingEList<?> start;
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(fairExchangeApp);
		start = (EObjectResolvingEList<?>) model.getValue(model.getAppliedStereotype("UMLsec::fair exchange"), "start");
		EObjectResolvingEList<?> stop = (EObjectResolvingEList<?>) model.getValue(model.getAppliedStereotype("UMLsec::fair exchange"), "stop");
		FairExchangeCheck fe = new FairExchangeCheck();
		assertFalse(start.size() == 0);
		assertTrue(stop.size() == 0);
		assertFalse(fe.perform(null, analysisHost));
		modelres.unload();
	}
	
	/**
	 * Tests if the test returns true if there are no ways in the diagram.
	 */
	@Test
	public final void testNoWays() {
		TestHost analysisHost = new TestHost();
		loadModel("testFairExchangeNoWay.uml");
		ActivityDiagramManager adm = new ActivityDiagramManager(model, new DummyHost(true));
		List<List<Element>> list = adm.getAllPaths();
		assertTrue(list.size() == 0);
		FairExchangeCheck fe = new FairExchangeCheck();
		assertTrue(fe.perform(null, analysisHost));
	}
}
