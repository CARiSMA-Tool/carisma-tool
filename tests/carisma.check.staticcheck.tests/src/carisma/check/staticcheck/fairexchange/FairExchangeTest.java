package carisma.check.staticcheck.fairexchange;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.junit.After;
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

		public TestHost() {
			// TODO Auto-generated constructor stub
		}

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
			return FairExchangeTest.this.modelres;
		}

		@Override
		public String getCurrentModelFilename() {
			return FairExchangeTest.this.modelres.getURI().toFileString();
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
	private ResourceSet rs = new ResourceSetImpl();
	
	/**
	 * the modelresource.
	 */
	Resource modelres = null;
	
	/**
	 * the model.
	 */
	private Model model = null;

	
	/**
	 * method to load a model from a UML file.
	 * @param testmodelname - the name of the UML file
	 */
	public final void loadModel(final String testmodelname) throws IOException {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
		this.model = (Model) this.modelres.getContents().get(0);
	}
	
	/**
	 * Tests if the test returns true if the model is correct.
	 * @throws IOException 
	 */
	@Test
	public final void testSuccess() throws IOException {
		loadModel("testFairExchangeSuccess.uml");
		FairExchangeCheck fe = new FairExchangeCheck();
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(fairExchangeApp);
		TestHost analysisHost = new TestHost();
		assertTrue(fe.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	/**
	 * Tests if the test returns false if the model is false with respect to fair exchange.
	 * @throws IOException 
	 */
	@Test
	public final void testViolated() throws IOException {
		TestHost analysisHost = new TestHost();
		loadModel("testFairExchangeViolated.uml");
		FairExchangeCheck fe = new FairExchangeCheck();
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(fairExchangeApp);
		assertFalse(fe.perform(null, analysisHost));
		this.modelres.unload();
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
	 * @throws IOException 
	 */
	@Test
	public final void testNoProfileStereotype() throws IOException {
		loadModel("testFairExchangeNoProfile.uml");
		TestHost analysisHost = new TestHost();
		FairExchangeCheck fe = new FairExchangeCheck();
		assertTrue(fe.perform(null, analysisHost));
		this.modelres.unload();
		loadModel("testFairExchangeNoStereotype.uml");
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(this.model, UMLsec.FAIR_EXCHANGE);
		assertNull(fairExchangeApp);
		assertTrue(fe.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	/**
	 * Tests if the test returns false when the start-tag of the stereotype fair-exchange are empty.
	 * @throws IOException 
	 */
	@Test
	public final void testStartTag() throws IOException {
		loadModel("testFairExchangeNoStart.uml");
		TestHost analysisHost = new TestHost();
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(fairExchangeApp);
		EObjectResolvingEList<?> start = (EObjectResolvingEList<?>) this.model.getValue(this.model.getAppliedStereotype("UMLsec::fair exchange"), "start");
		FairExchangeCheck fe = new FairExchangeCheck();
		assertTrue(start.size() == 0);
		assertFalse(fe.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	/**
	 * Tests if the test returns false when the stop-tag of the stereotype fair-exchange are empty.
	 * @throws IOException 
	 */
	@Test
	public final void testStopTag() throws IOException {
		loadModel("testFairExchangeNoStop.uml");
		TestHost analysisHost = new TestHost();
		
		EObjectResolvingEList<?> start;
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(fairExchangeApp);
		start = (EObjectResolvingEList<?>) this.model.getValue(this.model.getAppliedStereotype("UMLsec::fair exchange"), "start");
		EObjectResolvingEList<?> stop = (EObjectResolvingEList<?>) this.model.getValue(this.model.getAppliedStereotype("UMLsec::fair exchange"), "stop");
		FairExchangeCheck fe = new FairExchangeCheck();
		assertFalse(start.size() == 0);
		assertTrue(stop.size() == 0);
		assertFalse(fe.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	/**
	 * Tests if the test returns true if there are no ways in the diagram.
	 * @throws IOException 
	 */
	@Test
	public final void testNoWays() throws IOException {
		TestHost analysisHost = new TestHost();
		loadModel("testFairExchangeNoWay.uml");
		ActivityDiagramManager adm = new ActivityDiagramManager(this.model, new DummyHost(true));
		List<List<Element>> list = adm.getAllPaths();
		assertTrue(list.size() == 0);
		FairExchangeCheck fe = new FairExchangeCheck();
		assertTrue(fe.perform(null, analysisHost));
	}
	
	@After
	public void unloadModel(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}
}
