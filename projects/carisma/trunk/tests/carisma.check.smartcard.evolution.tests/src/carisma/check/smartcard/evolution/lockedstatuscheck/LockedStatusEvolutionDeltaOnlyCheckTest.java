package carisma.check.smartcard.evolution.lockedstatuscheck;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.UserAbortedAnalysisException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.DeltaFactoryCheck;
import carisma.evolution.uml2.umlchange.UMLchangeParserCheck;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlchange.UMLchange;
import carisma.tests.modelutils.uml.TestHelper;

/**
 * 
 * @author jkowald
 *
 */
public class LockedStatusEvolutionDeltaOnlyCheckTest {
	
	/**
	 * Path to the test model folder.
	 */
	private final String filepath = "resources/models/lockedStatusEvolutionDeltaOnly";
	
	/**
	 * The test model.
	 */
	private Model model;
		
	/**
	 * A dummy AnalysisHost for the test.
	 */
	private AnalysisHost testHost;
	
	// ################################################################################
	// Testcases
	/**
	 * A transition is added with a source state that owns the stereotype locked-state.
	 */
	@Test
	public final void testAddTransitionWithSourceStateIsLocked() {
		assertFalse(performCheck("testAddTransitionWithSourceStateIsLocked.uml"));
	}
	
	/**
	 * A transition is added with a source state that gets the stereotype locked-state.
	 */
	@Test
	public final void testAddTransitionWithSourceStateGetsLocked() {
		assertFalse(performCheck("testAddTransitionWithSourceStateGetsLocked.uml"));
	}
	
	/**
	 * A transition is added with a source state that loses the stereotype locked-state.
	 */
	@Test
	public final void testAddTransitionWithSourceStateLosesLocked() {
		assertTrue(performCheck("testAddTransitionWithSourceStateLosesLocked.uml"));
	}
	
	/**
	 * The stereotype locked-status is added to a state with a outgoing transition.
	 */
	@Test
	public final void testAddLockedStatusWithOutgoingTransition() {
		assertFalse(performCheck("testAddLockedStatusWithOutgoingTransition.uml"));
	}
	
	/**
	 * The stereotype locked-status is added to a state without outgoing transitions.
	 */
	@Test
	public final void testAddLockedStatusWithNoOutgoingTransition() {
		assertTrue(performCheck("testAddLockedStatusWithNoOutgoingTransition.uml"));
	}
	
	/**
	 * The stereotype locked-status is added to a state with a outgoing transition which will be deleted.
	 */
	@Test
	public final void testAddLockedStatusDeletingOutgoingTransition() {
		assertTrue(performCheck("testAddLockedStatusDeletingOutgoingTransition.uml"));
	}
	
	/**
	 * A transition gets a new source state which owns the stereotype locked-status.
	 */
	@Test
	public final void testEditTransition() {
		assertFalse(performCheck("testEditTransition.uml"));
	}
	
	
	/**
	 * Cleans up the test model variables.
	 */
	@After
	public final void cleanUp() {
		if (model != null) {
			TestHelper.unloadModel(model);
			model = null;
		}
	}
	
	// ################################################################################
	// Helping methods & classes
	/**
	 * Shortcut to perform a check.
	 * @param testModelFileName the filename of the test model
	 * @return A boolean which indicates the result of the check
	 */
	private boolean performCheck(final String testModelFileName) {
		boolean umlChangeParserSuccess = false;
		boolean deltaCalculatorSuccess = false;
		boolean checkSuccess = false;
		initTest(testModelFileName);
		UMLchangeParserCheck umlChangeParser = new UMLchangeParserCheck();
		umlChangeParserSuccess = umlChangeParser.perform(null, testHost);
		DeltaFactoryCheck deltaCalculator = new DeltaFactoryCheck();
		deltaCalculatorSuccess = deltaCalculator.perform(null, testHost);
		LockedStatusEvolutionDeltaOnlyCheck theCheck = new LockedStatusEvolutionDeltaOnlyCheck();
		checkSuccess = theCheck.perform(null, testHost);
		return (umlChangeParserSuccess & deltaCalculatorSuccess & checkSuccess);
	}
	
	/**
	 * Initializes the dummy AnalysisHost and loads the test model.
	 * @param testModelFileName the filename of the test model
	 */
	private void initTest(final String testModelFileName) {
		testHost = new TestHost();
		assertNotNull(testHost);
		model = TestHelper.loadModel(filepath, testModelFileName);
		assertTrue(testHost.getAnalyzedModel().isLoaded());
		assertTrue(UMLHelper.isProfileApplied(model, UMLsec.DESCRIPTOR));
		assertTrue(UMLHelper.isProfileApplied(model, UMLchange.DESCRIPTOR));
	}
		
	/**
	 * Dummy analysis host.
	 */
	private class TestHost implements AnalysisHost {
		
		/**
		 * Dummy register.
		 */
		private Map<String, Object> register = null;
		
		/**
		 * Constructor.
		 */
		public TestHost() {
			register = new HashMap<String, Object>();
		}
		
		@Override
		public Resource getAnalyzedModel() {
			if (model != null) {
				return model.eResource();
			}
			return null;
		}

		@Override
		public void addResultMessage(final AnalysisResultMessage detail) {
			// Auto-generated method stub
		}

		@Override
		public void appendToReport(final String text) {
			// Auto-generated method stub
		}

		@Override
		public void appendLineToReport(final String text) {
			// Auto-generated method stub
		}

		@Override
		public String getCurrentModelFilename() {
			return null;
		}

		@Override
		public final void putToRegister(final String registerName, final Object data) throws RegisterInUseException {
			if (isRegisterInUse(registerName)) {
				throw new RegisterInUseException(registerName);
			}
			this.register.put(registerName, data);
		}

		/**
		 * @param registerName the register name7
		 * @return boolean 
		 */
		@Override
		public final boolean isRegisterInUse(final String registerName) {
			return this.register.containsKey(registerName);
		}

		@Override
		public final Object getFromRegister(final String registerName) throws RegisterNotInUseException {
			if (!isRegisterInUse(registerName)) {
				throw new RegisterNotInUseException(registerName);
			}
			return this.register.get(registerName);
		}

		@Override
		public final Object removeFromRegister(final String registerName) throws RegisterNotInUseException {
			if (!isRegisterInUse(registerName)) {
				throw new RegisterNotInUseException(registerName);
			}
			return this.register.remove(registerName);
		}


		@Override
		public void displayError(final String message) {
			Logger.log(LogLevel.INFO, message);
		}


		@Override
		public File getFileToBeWritten(final File file)
				throws UserAbortedAnalysisException {
			return file;
		}
		
	}

}