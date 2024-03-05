package carisma.check.smartcard.evolution.authorizedstatuscheck;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.junit.Ignore;
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
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlsec.UMLsec;
import carisma.tests.modelutils.uml.TestHelper;

/**
 * 
 * @author jkowald
 *
 */

public class AuthorizedStatusEvolutionDeltaOnlyCheckTest {

	/**
	 * Path to the test model folder.
	 */
	private final String filepath = "resources/models/authorizedStatusEvolutionDeltaOnly";
	
	/**
	 * The test model.
	 */
	Model model;
	
	/**
	 * A dummy AnalysisHost for the test.
	 */
	private AnalysisHost testHost;
	
	// ################################################################################
	// Testcases
	/**
	 * Adding a guard to a random transition.
	 */
	@Ignore
	@Test
	public final void testAddGuardToTransition() {
		assertTrue(performCheck("testAddGuardToTransition.uml"));
	}
	
	/**
	 * Adding the <<authorized-status>> stereotype to a state which 
	 * has an incoming transition with the right guard.
	 */
	@Test
	public final void testAddStereotype() {
		assertTrue(performCheck("testAddStereotype.uml"));
	}
	
	/**
	 * Adding a transition without the right guard to a state with the
	 * <<authorized-status>> stereotype applied.
	 */
	@Test
	public final void testAddTransitionToAuthState() {
		assertFalse(performCheck("testAddTransitionToAuthState.uml"));
	}
	
	/**
	 * Adding a transition with the right guard.
	 */
	@Test
	public final void testAddTransitionToAuthStateWithGuard() {
		assertTrue(performCheck("testAddTransitionToAuthStateWithGuard.uml"));
	}
	
	/**
	 * Adding a transition to a state which gets the 
	 * <<authorized-status>> stereotype.
	 */
	@Test
	public final void testAddTransitionToStateGetsAuth() {
		assertFalse(performCheck("testAddTransitionToStateGetsAuth.uml"));
	}
	
	/**
	 * Adding a transition with the right guard to a state which 
	 * gets the <<authorized-status>> stereotype.
	 */
	@Test
	public final void testAddTransitionWithGuardToStateGetsAuth() {
		assertTrue(performCheck("testAddTransitionWithGuardToStateGetsAuth.uml"));
	}
	
	/**
	 * Deleting a guard of a transition with the destination set 
	 * to a state with the stereotype <<authorized-status>>.
	 */
	@Test
	public final void testDeleteGuard() {
		assertFalse(performCheck("testDeleteGuard.uml"));
	}
	
	/**
	 * Deleting and adding a new guard of a transition with the 
	 * destination set to a state with the stereotype <<authorized-status>>.
	 */
	@Test
	public final void testDeleteGuardAddNewGuard() {
		assertTrue(performCheck("testDeleteGuardAddNewGuard.uml"));
	}
	
	/**
	 * Deleting the old guard and adding a new guard with a contraint
	 * similar to the new value of the permission belonging to
	 * <<authorized-status>>.
	 */
	@Ignore
	@Test
	public final void testDeleteGuardAddNewGuardEditPermission() {
		assertTrue(performCheck("testDeleteGuardAddNewGuardEditPermission.uml"));
	}
	
	/**
	 * Editing Guard and Permission in the same way.
	 */
	@Ignore
	@Test
	public final void testEditGuardEditPermission() {
		assertTrue(performCheck("testEditGuardEditPermission.uml"));
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
		umlChangeParserSuccess = umlChangeParser.perform(null, this.testHost);
		DeltaFactoryCheck deltaCalculator = new DeltaFactoryCheck();
		deltaCalculatorSuccess = deltaCalculator.perform(null, this.testHost);
		AuthorizedStatusEvolutionDeltaOnlyCheck theCheck = new AuthorizedStatusEvolutionDeltaOnlyCheck();
		checkSuccess = theCheck.perform(null, this.testHost);
		return (umlChangeParserSuccess & deltaCalculatorSuccess & checkSuccess);
	}
	
	/**
	 * Initializes the dummy AnalysisHost and loads the test model.
	 * @param testModelFileName the filename of the test model
	 */
	private void initTest(final String testModelFileName) {
		this.testHost = new TestHost();
		assertNotNull(this.testHost);
		this.model = TestHelper.loadModel(this.filepath, testModelFileName);
		assertTrue(this.testHost.getAnalyzedModel().isLoaded());
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLsec.DESCRIPTOR));
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
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
			this.register = new HashMap<>();
		}
		
		@Override
		public Resource getAnalyzedModel() {
			if (AuthorizedStatusEvolutionDeltaOnlyCheckTest.this.model != null) {
				return AuthorizedStatusEvolutionDeltaOnlyCheckTest.this.model.eResource();
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
