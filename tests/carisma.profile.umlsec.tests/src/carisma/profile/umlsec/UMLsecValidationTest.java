package carisma.profile.umlsec;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;

/**
 * JUnit testcases for the {@link UMLsecValidation}.
 * @author Klaus Rudack
 *
 */
public class UMLsecValidationTest {

	/** Path to the fair exchange testmodel folder. */
	private String fairExchangeFilePath = "resources" + File.separator + "models" + File.separator + "fair_exchange";
	
	/** Path to the authorized status testmodel folder. */
	private String authorizedStatusFilePath = "resources" + File.separator + "models" + File.separator + "authorized_status";
	
	/** Path to the guarded testmodel folder. */
	private String guardedFilePath = "resources" + File.separator + "models" + File.separator + "guarded";
	
	/** Path to the locked status testmodel folder. */
	private String lockedStatusFilePath = "resources" + File.separator + "models" + File.separator + "locked_status";
	
	/** Path to the secure links testmodel folder. */
	private String secureLinksFilePath = "resources" + File.separator + "models" + File.separator + "secure_links";
	
	/** Path to the critical testmodel folder. */
	private String criticalFilePath = "resources" + File.separator + "models" + File.separator + "critical";
	
	/** Path to the requires testmodel folder. */
	private String requiresFilePath = "resources" + File.separator + "models" + File.separator + "requires";
	
	/** Path to the provable testmodel folder. */
	private String provableFilePath = "resources" + File.separator + "models" + File.separator + "provable";
	
	/** Path to the noDownFlow testmodel folder. */
	private String noDownFlowFilePath = "resources" + File.separator + "models" + File.separator + "no_down_flow";
	
	/** Path to the secure dependency testmodel folder. */
	private String secureDependencyFilePath = "resources" + File.separator + "models" + File.separator + "secure_dependency";
	
	/** Path to the guarded access testmodel folder. */
	private String guardedAccessFilePath = "resources" + File.separator + "models" + File.separator + "guarded_access";
	
	/** Path to the no up flow testmodel folder. */
	private String noUpFlowFilePath = "resources" + File.separator + "models" + File.separator + "no_up_flow";
	
	/** Path to the data security testmodel folder. */
	private String dataSecurityFilePath = "resources" + File.separator + "models" + File.separator + "data_security";
	
	/** Path to the rbac testmodel folder. */
	private String rbacFilePath = "resources" + File.separator + "models" + File.separator + "rbac";
	
	/** Path to the mutlipleStereotype testmodel folder. */
	private String multiplePath = "resources" + File.separator + "models" + File.separator + "multipleWrongStereotypes";
	
	/** Path to the findModelElements testmodel folder. */
	private String modelElementsFilePath = "resources" + File.separator + "models";
	
	/** UML2ModelLoader. */
	private ResourceSet rs = new ResourceSetImpl();
	
	/** The modelresource. */
	private Resource modelres = null;
	
	/** The model. */
	private Model model = null;
	
	/**
	 * Method to load a model from an UML file.
	 * @param testmodelname - the name of the UML file with its path
	 */
	public final void loadModel(final String testmodelname) {
		File testmodelfile = new File(testmodelname);
		assertTrue(testmodelfile.exists());
		try (FileInputStream in = new FileInputStream(testmodelfile)){
			this.modelres = this.rs.createResource(URI.createFileURI(testmodelname));
			this.modelres.load(in, Collections.EMPTY_MAP);
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
		assertNotNull(this.modelres);
		this.model = (Model) this.modelres.getContents().get(0);
		assertNotNull(this.model);
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;fair exchange&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;fair exchange&gt;&gt; Stereotype.
	 */
	@Test
	public final void testFairExchangeWrongSterotype() {
		String modelName = "AuthorizedStatusNoContent.uml";
		assertNull(this.modelres);
		loadModel(this.authorizedStatusFilePath + File.separator + modelName);
		Element authorizedElement = null;
		try {
			authorizedElement = UMLHelper.getElementByName(this.model, "State2");
		} catch (ModelElementNotFoundException e) {
			fail("Element State2 has not been found!");
		}
		assertNotNull(authorizedElement);
		StereotypeApplication fairExchangeApp = UMLsecUtil.getStereotypeApplication(authorizedElement, UMLsec.AUTHORIZED_STATUS);
		assertNotNull(fairExchangeApp);
		List<String> result = UMLsecValidation.validateFairExchange(fairExchangeApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<fair exchange>> expected, but was <<authorized-status>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;fair exchange&gt;&gt; Stereotype where the Stereotype is applied to the Model.
	 * This test should fail (return false)
	 */
	@Test
	public final void testFairExchangeModelStereotype() {
		//this Stereotype is correct according to fair exchange but is applied to the Model, not to a Package
		//the testing algorithm where the Stereotype is applied to is the same in every check (if needed), so its just tested once
		String modelName = "FairExchangeModelStereotype.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication fairExchangeApp = stereoList.get(0);
		assertNotNull(fairExchangeApp);
		List<String> result = UMLsecValidation.validateFairExchange(fairExchangeApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Stereotype <<fair exchange>> is only  allowed to be applied to a Package! Actually it is applied to FairExchangeModelStereotypeModel!"
				, result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;fair exchange&gt;&gt; Stereotype with an empty start tag.
	 */
	@Test
	public final void testFairExchangeNoStart() {
		String modelName = "FairExchangeNoStart.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication fairExchangeApp = stereoList.get(0);
		assertNotNull(fairExchangeApp);
		List<String> resultSingle = UMLsecValidation.validateFairExchange(fairExchangeApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Empty Tag start of Stereotype <<fair exchange>> at Element FairExchangeNoStartPackage!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Empty Tag start of Stereotype <<fair exchange>> at Element FairExchangeNoStartPackage!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;fair exchange&gt;&gt; Stereotype with an empty stopt tag.
	 */
	@Test
	public final void testFairExchangeNoStop() {
		String modelName = "FairExchangeNoStop.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication fairExchangeApp = stereoList.get(0);
		assertNotNull(fairExchangeApp);
		List<String> result = UMLsecValidation.validateFairExchange(fairExchangeApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Empty Tag stop of Stereotype <<fair exchange>> at Element FairExchangeNoStopPackage!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a correct &lt;&lt;fair exchange&gt;&gt; Stereotype.
	 */
	@Test
	public final void testFairExchangeCorrect() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication fairExchangeApp = stereoList.get(0);
		assertNotNull(fairExchangeApp);
		List<String> result = UMLsecValidation.validateFairExchange(fairExchangeApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of an &lt;&lt;authorized status&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not an &lt;&lt;authorized status&gt;&gt; Stereotype.
	 */
	@Test
	public final void testAuthorizedStatusWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication authorizedStatusApp = stereoList.get(0);
		assertNotNull(authorizedStatusApp);
		List<String> result = UMLsecValidation.validateAuthorizedStatus(authorizedStatusApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<authorized status>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of an &lt;&lt;authorized status&gt;&gt; Stereotype with no content in tag permission.
	 */
	@Test
	public final void testAuthorizedStatusNoContent() {
		String modelName = "AuthorizedStatusNoContent.uml";
		assertNull(this.modelres);
		loadModel(this.authorizedStatusFilePath + File.separator + modelName);
		Element authorizedElement = null;
		try {
			authorizedElement = UMLHelper.getElementByName(this.model, "State2");
		} catch (ModelElementNotFoundException e) {
			fail("Element State2 has not been found!");
		}
		assertNotNull(authorizedElement);
		StereotypeApplication authorizedStatusApp = UMLsecUtil.getStereotypeApplication(authorizedElement, UMLsec.AUTHORIZED_STATUS);
		assertNotNull(authorizedStatusApp);
		List<String>  resultSingle = UMLsecValidation.validateAuthorizedStatus(authorizedStatusApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Empty permission tag of Stereotype <<authorized-status>> at Element State2!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Empty permission tag of Stereotype <<authorized-status>> at Element State2!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of model with an &lt;&lt;authorized status&gt;&gt; Stereotype applied to a Pseudostate.
	 * In this case, the Pseudostate is a FinalState.
	 */
	@Test
	public final void testAuthorizedStatusPseudostate() {
		String modelName = "AuthorizedStatusPseudostate.uml";
		assertNull(this.modelres);
		loadModel(this.authorizedStatusFilePath + File.separator + modelName);
		Element authorizedElement = null;
		try {
			authorizedElement = UMLHelper.getElementByName(this.model, "FinalState1");
		} catch (ModelElementNotFoundException e) {
			fail("Element FinalState1 has not been found!");
		}
		assertNotNull(authorizedElement);
		StereotypeApplication authorizedStatusApp = UMLsecUtil.getStereotypeApplication(authorizedElement, UMLsec.AUTHORIZED_STATUS);
		assertNotNull(authorizedStatusApp);
		List<String> result = UMLsecValidation.validateAuthorizedStatus(authorizedStatusApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Stereotype <<authorized status>> is not allowed to be applied to a Final State! Actually it is applied to FinalState1!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a correct &lt;&lt;authorized status&gt;&gt; Stereotype.
	 */
	@Test
	public final void testAuthorizedStatusCorrect() {
		String modelName = "AuthorizedStatusCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.authorizedStatusFilePath + File.separator + modelName);
		Element authorizedElement = null;
		try {
			authorizedElement = UMLHelper.getElementByName(this.model, "State2");
		} catch (ModelElementNotFoundException e) {
			fail("Element State2 has not been found!");
		}
		assertNotNull(authorizedElement);
		StereotypeApplication authorizedStatusApp = UMLsecUtil.getStereotypeApplication(authorizedElement, UMLsec.AUTHORIZED_STATUS);
		assertNotNull(authorizedStatusApp);
		List<String> result = UMLsecValidation.validateAuthorizedStatus(authorizedStatusApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}

	/**
	 * This test tests the validation of a &lt;&lt;guarded&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;guarded&gt;&gt; Stereotype.
	 */
	@Test
	public final void testGuardedWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication guardedApp = stereoList.get(0);
		assertNotNull(guardedApp);
		List<String> result = UMLsecValidation.validateGuarded(guardedApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <guarded>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;guarded&gt;&gt; Stereotype with no content in tag gaurd.
	 */
	@Test
	public final void testGuardedNoContent() {
		String modelName = "GuardedNoContent.uml";
		assertNull(this.modelres);
		loadModel(this.guardedFilePath + File.separator + modelName);
		Element guardedElement = null;
		try {
			guardedElement = UMLHelper.getElementByName(this.model, "Class1");
		} catch (ModelElementNotFoundException e) {
			fail("Element Class1 has not been found!");
		}
		assertNotNull(guardedElement);
		StereotypeApplication guardedApp = UMLsecUtil.getStereotypeApplication(guardedElement, UMLsec.GUARDED);
		assertNotNull(guardedApp);
		List<String> resultSingle = UMLsecValidation.validateGuarded(guardedApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Empty guard tag of Stereotype <<guarded>> at Element Class1!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Empty guard tag of Stereotype <<guarded>> at Element Class1!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a correct &lt;&lt;guarded&gt;&gt; Stereotype.
	 */
	@Test
	public final void testGuardedCorrect() {
		String modelName = "GuardedCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.guardedFilePath + File.separator + modelName);
		Element guardedElement = null;
		try {
			guardedElement = UMLHelper.getElementByName(this.model, "Class1");
		} catch (ModelElementNotFoundException e) {
			fail("Element Class1 has not been found!");
		}
		assertNotNull(guardedElement);
		StereotypeApplication guardedApp = UMLsecUtil.getStereotypeApplication(guardedElement, UMLsec.GUARDED);
		assertNotNull(guardedApp);
		List<String> result = UMLsecValidation.validateGuarded(guardedApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;locked status&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;locked status&gt;&gt; Stereotype.
	 */
	@Test
	public final void testLockedStatusWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication lockedStatusApp = stereoList.get(0);
		assertNotNull(lockedStatusApp);
		List<String> result = UMLsecValidation.validateLockedStatus(lockedStatusApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<lockes status>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;locked status&gt;&gt; Stereotype applied to a FinalState.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testLockedStatusFinalState() {
		String modelName = "LockedStatusFinalState.uml";
		assertNull(this.modelres);
		loadModel(this.lockedStatusFilePath + File.separator + modelName);
		Element lockedStatusElement = null;
		try {
			lockedStatusElement = UMLHelper.getElementByName(this.model, "FinalState1");
		} catch (ModelElementNotFoundException e) {
			fail("Element FinalState1 has not been found!");
		}
		assertNotNull(lockedStatusElement);
		StereotypeApplication lockedStatusApp = UMLsecUtil.getStereotypeApplication(lockedStatusElement, UMLsec.LOCKED_STATUS);
		assertNotNull(lockedStatusApp);
		List<String> resultSingle = UMLsecValidation.validateLockedStatus(lockedStatusApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Stereotype <<locked status>> is not allowed to be applied to a FinalState! Actually it is applied to FinalState1!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Stereotype <<locked status>> is not allowed to be applied to a FinalState! Actually it is applied to FinalState1!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;locked status&gt;&gt; Stereotype applied to a State.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testLockedStatusCorrect() {
		String modelName = "LockedStatusCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.lockedStatusFilePath + File.separator + modelName);
		Element lockedStatusElement = null;
		try {
			lockedStatusElement = UMLHelper.getElementByName(this.model, "State1");
		} catch (ModelElementNotFoundException e) {
			fail("Element State1 has not been found!");
		}
		assertNotNull(lockedStatusElement);
		StereotypeApplication lockedStatusApp = UMLsecUtil.getStereotypeApplication(lockedStatusElement, UMLsec.LOCKED_STATUS);
		assertNotNull(lockedStatusApp);
		List<String> result = UMLsecValidation.validateLockedStatus(lockedStatusApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}

	/**
	 * This test tests the validation of a &lt;&lt;secure links&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;secure links&gt;&gt; Stereotype.
	 */
	@Test
	public final void testSecureLinksWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication secureLinksApp = stereoList.get(0);
		assertNotNull(secureLinksApp);
		List<String> result = UMLsecValidation.validateSecureLinks(secureLinksApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<secure links>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This tests tests the validation of a &lt;&lt;secure links&gt;&gt; Stereotype where the tag 'adversary' is empty.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testSecureLinksNoAdversary() {
		String modelName = "SecureLinksNoAdversary.uml";
		assertNull(this.modelres);
		loadModel(this.secureLinksFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.SECURE_LINKS);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication secureLinksApp = stereoList.get(0);
		assertNotNull(secureLinksApp);
		List<String> resultSingle = UMLsecValidation.validateSecureLinks(secureLinksApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Empty adversary tag of Stereotype <<secure links>> at Element SecureLinksNoAttackerPackage!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Empty adversary tag of Stereotype <<secure links>> at Element SecureLinksNoAttackerPackage!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This tests tests the validation of a &lt;&lt;secure links&gt;&gt; Stereotype that is correct filled.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testSecureLinksCorrect() {
		String modelName = "SecureLinksCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.secureLinksFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.SECURE_LINKS);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication secureLinksApp = stereoList.get(0);
		assertNotNull(secureLinksApp);
		List<String> result = UMLsecValidation.validateSecureLinks(secureLinksApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;critical&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;critical&gt;&gt; Stereotype.
	 */
	@Test
	public final void testCriticalWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication criticalApp = stereoList.get(0);
		assertNotNull(criticalApp);
		List<String> result = UMLsecValidation.validateCritical(criticalApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<critical>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;critical&gt;&gt; Stereotype with no content.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testCriticalNoContent() {
		String modelName = "CriticalNoContent.uml";
		assertNull(this.modelres);
		loadModel(this.criticalFilePath + File.separator + modelName);
		Element criticalElement = null;
		try {
			criticalElement = UMLHelper.getElementByName(this.model, "Class1");
		} catch (ModelElementNotFoundException e) {
			fail("Element Class1 has not been found!");
		}
		assertNotNull(criticalElement);
		StereotypeApplication criticalApp = UMLsecUtil.getStereotypeApplication(criticalElement, UMLsec.CRITICAL);
		assertNotNull(criticalApp);
		List<String> resultSingle = UMLsecValidation.validateCritical(criticalApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("One of the tags 'secrecy, integrity, high, fresh, privacy, authenticity' of Sterotype <<critical>> has to hold content!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("One of the tags 'secrecy, integrity, high, fresh, privacy, authenticity' of Sterotype <<critical>> has to hold content!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;critical&gt;&gt; Stereotype with no content.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testCriticalCorrect() {
		String modelName = "CriticalCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.criticalFilePath + File.separator + modelName);
		Element criticalElement = null;
		try {
			criticalElement = UMLHelper.getElementByName(this.model, "Class1");
		} catch (ModelElementNotFoundException e) {
			fail("Element Class1 has not been found!");
		}
		assertNotNull(criticalElement);
		StereotypeApplication criticalApp = UMLsecUtil.getStereotypeApplication(criticalElement, UMLsec.CRITICAL);
		assertNotNull(criticalApp);
		List<String> result = UMLsecValidation.validateCritical(criticalApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;requires&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;requires&gt;&gt; Stereotype.
	 */
	@Test
	public final void testRequiresWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication requiresApp = stereoList.get(0);
		assertNotNull(requiresApp);
		List<String> result = UMLsecValidation.validateRequires(requiresApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<requires>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;requiresl&gt;&gt; Stereotype with no content.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testRequiresNoContent() {
		String modelName = "RequiresNoContent.uml";
		assertNull(this.modelres);
		loadModel(this.requiresFilePath + File.separator + modelName);
		Element requiresElement = null;
		try {
			requiresElement = UMLHelper.getElementByName(this.model, "OpaqueAction2");
		} catch (ModelElementNotFoundException e) {
			fail("Element OpaqueAction2 has not been found!");
		}
		assertNotNull(requiresElement);
		StereotypeApplication requiresApp = UMLsecUtil.getStereotypeApplication(requiresElement, UMLsec.REQUIRES);
		assertNotNull(requiresApp);
		List<String> resultSingle = UMLsecValidation.validateRequires(requiresApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Empty actions tag of Stereotype <<requires>> at Element OpaqueAction2!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Empty actions tag of Stereotype <<requires>> at Element OpaqueAction2!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;requiresl&gt;&gt; Stereotype that is filled correctly.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testRequiresCorrect() {
		String modelName = "RequiresCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.requiresFilePath + File.separator + modelName);
		Element requiresElement = null;
		try {
			requiresElement = UMLHelper.getElementByName(this.model, "OpaqueAction2");
		} catch (ModelElementNotFoundException e) {
			fail("Element OpaqueAction2 has not been found!");
		}
		assertNotNull(requiresElement);
		StereotypeApplication requiresApp = UMLsecUtil.getStereotypeApplication(requiresElement, UMLsec.REQUIRES);
		assertNotNull(requiresApp);
		List<String> result = UMLsecValidation.validateRequires(requiresApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;provable&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;provable&gt;&gt; Stereotype.
	 */
	@Test
	public final void testProvableWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication provableApp = stereoList.get(0);
		assertNotNull(provableApp);
		List<String> result = UMLsecValidation.validateProvable(provableApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<provable>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;provable&gt;&gt; Stereotype with no content in tag 'action'.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testProvableNoAction() {
		String modelName = "ProvableNoAction.uml";
		assertNull(this.modelres);
		loadModel(this.provableFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.PROVABLE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication provableApp = stereoList.get(0);
		assertNotNull(provableApp);
		List<String> resultSingle = UMLsecValidation.validateProvable(provableApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Empty action tag of Stereotype <<provable>> at Element ProvableNoAction!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Empty action tag of Stereotype <<provable>> at Element ProvableNoAction!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;provable&gt;&gt; Stereotype with no content in tag 'adversary'.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testProvableNoAdversary() {
		String modelName = "ProvableNoAdversary.uml";
		assertNull(this.modelres);
		loadModel(this.provableFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.PROVABLE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication provableApp = stereoList.get(0);
		assertNotNull(provableApp);
		List<String> result = UMLsecValidation.validateProvable(provableApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Empty adversary tag of Stereotype <<provable>> at Element ProvableNoAdversaryPackage!", result.get(0));
		this.modelres.unload();
	}
	
	
	/**
	 * This test tests the validation of a &lt;&lt;provable&gt;&gt; Stereotype with no content in tag 'cert'.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testProvableNoCert() {
		String modelName = "ProvableNoCert.uml";
		assertNull(this.modelres);
		loadModel(this.provableFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.PROVABLE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication provableApp = stereoList.get(0);
		assertNotNull(provableApp);
		List<String> result = UMLsecValidation.validateProvable(provableApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Empty cert tag of Stereotype <<provable>> at Element ProvableNoCertPackage!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;provable&gt;&gt; Stereotype that is filled correctly.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testProvableCorrect() {
		String modelName = "ProvableCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.provableFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.PROVABLE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication provableApp = stereoList.get(0);
		assertNotNull(provableApp);
		List<String> result = UMLsecValidation.validateProvable(provableApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;no down flow&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;no down flow&gt;&gt; Stereotype.
	 */
	@Test
	public final void testNoDownFlowWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication noDownFlowApp = stereoList.get(0);
		assertNotNull(noDownFlowApp);
		List<String> result = UMLsecValidation.validateNoDownFlow(noDownFlowApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<no down flow>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;no down flow&gt;&gt; Stereotype that is applied to the Model.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testNoDownFlowModelStereotype() {
		String modelName = "NoDownFlowModelStereotype.uml";
		assertNull(this.modelres);
		loadModel(this.noDownFlowFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.NO_DOWN_FLOW);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication noDownFlowApp = stereoList.get(0);
		assertNotNull(noDownFlowApp);
		List<String> resultSingle = UMLsecValidation.validateNoDownFlow(noDownFlowApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Stereotype <<no down flow>> is only  allowed to be applied to a Package! Actually it is applied to "
				+ "NoDownFlowModelStereotypeModel!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Stereotype <<no down flow>> is only  allowed to be applied to a Package! Actually it is applied to "
				+ "NoDownFlowModelStereotypeModel!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;no down flow&gt;&gt; Stereotype that is filled correctly.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testNoDownFlowCorrect() {
		String modelName = "NoDownFlowCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.noDownFlowFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.NO_DOWN_FLOW);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication noDownFlowApp = stereoList.get(0);
		assertNotNull(noDownFlowApp);
		List<String> result = UMLsecValidation.validateNoDownFlow(noDownFlowApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;secure dependency&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;secure dependency&gt;&gt; Stereotype.
	 */
	@Test
	public final void testSecureDependencyWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication secureDependencyApp = stereoList.get(0);
		assertNotNull(secureDependencyApp);
		List<String> result = UMLsecValidation.validateSecureDependency(secureDependencyApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<secure dependency>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;secure dependency&gt;&gt; Stereotype that is applied to the Model.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testSecureDependencyModelStereotype() {
		String modelName = "SecureDependencyModelStereotype.uml";
		assertNull(this.modelres);
		loadModel(this.secureDependencyFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.SECURE_DEPENDENCY);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication noDownFlowApp = stereoList.get(0);
		assertNotNull(noDownFlowApp);
		List<String> resultSingle = UMLsecValidation.validateSecureDependency(noDownFlowApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Stereotype <<secure dependency>> is only  allowed to be applied to a Package! Actually it is applied to "
				+ "SecureDependencyModelStereotypeModel!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Stereotype <<secure dependency>> is only  allowed to be applied to a Package! Actually it is applied to "
				+ "SecureDependencyModelStereotypeModel!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;secure dependency&gt;&gt; Stereotype that is filled correctly.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testSecureDependencyCorrect() {
		String modelName = "SecureDependencyCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.secureDependencyFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.SECURE_DEPENDENCY);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication secureDependencyApp = stereoList.get(0);
		assertNotNull(secureDependencyApp);
		List<String> result = UMLsecValidation.validateSecureDependency(secureDependencyApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;guarded access&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;guarded access&gt;&gt; Stereotype.
	 */
	@Test
	public final void testGuardedAccessWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication guardedAccessApp = stereoList.get(0);
		assertNotNull(guardedAccessApp);
		List<String> result = UMLsecValidation.validateGuardedAccess(guardedAccessApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<guarded access>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;guarded access&gt;&gt; Stereotype that is applied to the Model.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testGuardedAccessModelStereotype() {
		String modelName = "GuardedAccessModelStereotype.uml";
		assertNull(this.modelres);
		loadModel(this.guardedAccessFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.GUARDED_ACCESS);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication guardedAccessApp = stereoList.get(0);
		assertNotNull(guardedAccessApp);
		List<String> resultSingle = UMLsecValidation.validateGuardedAccess(guardedAccessApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Stereotype <<guarded access>> is only  allowed to be applied to a Package! Actually it is applied to GuardedAccessModelStereotypeModel!"
				, resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Stereotype <<guarded access>> is only  allowed to be applied to a Package! Actually it is applied to GuardedAccessModelStereotypeModel!"
				, resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;guarded access&gt;&gt; Stereotype that is filled correctly.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testGuardedAccessCorrect() {
		String modelName = "GuardedAccessCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.guardedAccessFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.GUARDED_ACCESS);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication guardedAccessApp = stereoList.get(0);
		assertNotNull(guardedAccessApp);
		List<String> result = UMLsecValidation.validateGuardedAccess(guardedAccessApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;no up flow&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;no up flow&gt;&gt; Stereotype.
	 */
	@Test
	public final void testNoUpFlowWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication noUpFlowApp = stereoList.get(0);
		assertNotNull(noUpFlowApp);
		List<String> result = UMLsecValidation.validateNoUpFlow(noUpFlowApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<no up flow>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;no up flow&gt;&gt; Stereotype that is applied to the Model.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testNoUpFlowModelStereotype() {
		String modelName = "NoUpFlowModelStereotype.uml";
		assertNull(this.modelres);
		loadModel(this.noUpFlowFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.NO_UP_FLOW);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication noUpFlowApp = stereoList.get(0);
		assertNotNull(noUpFlowApp);
		List<String> resultSingle = UMLsecValidation.validateNoUpFlow(noUpFlowApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Stereotype <<no up flow>> is only  allowed to be applied to a Package! Actually it is applied to NoUpFlowModelStereotypeModel!"
				, resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Stereotype <<no up flow>> is only  allowed to be applied to a Package! Actually it is applied to NoUpFlowModelStereotypeModel!"
				, resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;no up flow&gt;&gt; Stereotype that is filled correctly.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testNoUpFlowCorrect() {
		String modelName = "NoUpFlowCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.noUpFlowFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.NO_UP_FLOW);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication noUpFlowApp = stereoList.get(0);
		assertNotNull(noUpFlowApp);
		List<String> result = UMLsecValidation.validateNoUpFlow(noUpFlowApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	
	/**
	 * This test tests the validation of a &lt;&lt;data security&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;data security&gt;&gt; Stereotype.
	 */
	@Test
	public final void testDataSecurityWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication dataSecurityApp = stereoList.get(0);
		assertNotNull(dataSecurityApp);
		List<String> result = UMLsecValidation.validateDataSecurity(dataSecurityApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Wrong stereotype. <<data security>> expected, but was <<fair exchange>>!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;data security&gt;&gt; Stereotype with no content in tag 'adversary'.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testDataSecurityNoAdversary() {
		String modelName = "DataSecurityNoAdversary.uml";
		assertNull(this.modelres);
		loadModel(this.dataSecurityFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.DATA_SECURITY);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication dataSecurityApp = stereoList.get(0);
		assertNotNull(dataSecurityApp);
		List<String> resultSingle = UMLsecValidation.validateDataSecurity(dataSecurityApp);
		assertNotNull(resultSingle);
		assertEquals(1, resultSingle.size());
		assertEquals("Empty adversary tag of Stereotype <<data security>> at Element DataSecurityNoAdversaryPackage!", resultSingle.get(0));
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertEquals(1, resultModel.size());
		assertEquals("Empty adversary tag of Stereotype <<data security>> at Element DataSecurityNoAdversaryPackage!", resultModel.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;data security&gt;&gt; Stereotype with no content in tag 'authenticity'.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testDataSecurityNoAuthenticity() {
		String modelName = "DataSecurityNoAuthenticity.uml";
		assertNull(this.modelres);
		loadModel(this.dataSecurityFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.DATA_SECURITY);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication dataSecurityApp = stereoList.get(0);
		assertNotNull(dataSecurityApp);
		List<String> result = UMLsecValidation.validateDataSecurity(dataSecurityApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Empty authenthicity tag of Stereotype <<data security>> at Element DataSecurityNoAuthenticityPackage!", result.get(0));
		this.modelres.unload();
	}
	
	
	/**
	 * This test tests the validation of a &lt;&lt;data security&gt;&gt; Stereotype with no content in tag 'integrity'.
	 * The validation should fail (return false).
	 */
	@Test
	public final void testDataSecurityNoIntegrity() {
		String modelName = "DataSecurityNoIntegrity.uml";
		assertNull(this.modelres);
		loadModel(this.dataSecurityFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.DATA_SECURITY);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication dataSecurityApp = stereoList.get(0);
		assertNotNull(dataSecurityApp);
		List<String> result = UMLsecValidation.validateDataSecurity(dataSecurityApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Empty integrity tag of Stereotype <<data security>> at Element DataSecurityNoIntegrityPackage!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;data security&gt;&gt; Stereotype that is filled correctly.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testDataSecurityCorrect() {
		String modelName = "DataSecurityCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.dataSecurityFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.DATA_SECURITY);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication dataSecurityApp = stereoList.get(0);
		assertNotNull(dataSecurityApp);
		List<String> result = UMLsecValidation.validateDataSecurity(dataSecurityApp);
		assertNotNull(result);
		assertTrue(result.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;rbac&gt;&gt; Stereotype with a wrong Stereotype.
	 * In this case, the Stereotype is not a &lt;&lt;rbac&gt;&gt; Stereotype.
	 */
	@Test
	public final void testRBACWrongStereotype() {
		String modelName = "FairExchangeCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.fairExchangeFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.FAIR_EXCHANGE);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication rbacApp = stereoList.get(0);
		assertNotNull(rbacApp);
		List<String> results = UMLsecValidation.validateRBAC(rbacApp);
		assertNotNull(results);
		assertEquals(1, results.size());
		assertEquals("Wrong stereotype. <<rbac>> expected, but was <<fair exchange>>!", results.get(0));
		this.modelres.unload();
	}

	/**
	 * This test tests the validation of a &lt;&lt;rbac&gt;&gt; Stereotype that has no values in tag protected.
	 */
	@Test
	public final void testRBACnoProtected() {
		String modelName = "RBACnoProtected.uml";
		assertNull(this.modelres);
		loadModel(this.rbacFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.RBAC);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication rbacApp = stereoList.get(0);
		assertNotNull(rbacApp);
		List<String> result = UMLsecValidation.validateRBAC(rbacApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Empty protected tag of Stereotype <<rbc>> at Element RBACnoProtectedPackage!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;rbac&gt;&gt; Stereotype that has no values in tag role.
	 */
	@Test
	public final void testRBACnoRole() {
		String modelName = "testRbacNoRole.uml";
		assertNull(this.modelres);
		loadModel(this.rbacFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.RBAC);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication rbacApp = stereoList.get(0);
		assertNotNull(rbacApp);
		List<String> result = UMLsecValidation.validateRBAC(rbacApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Empty role tag of Stereotype <<rbc>> at Element RBACnoRolePackage!", result.get(0));
		this.modelres.unload();
	}
	
	/**
	 * This test tests the validation of a &lt;&lt;rbac&gt;&gt; Stereotype that has no values in tag right.
	 */
	@Test
	public final void testRBACnoRight() {
		String modelName = "RBACnoRight.uml";
		assertNull(this.modelres);
		loadModel(this.rbacFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.RBAC);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication rbacApp = stereoList.get(0);
		assertNotNull(rbacApp);
		List<String> result = UMLsecValidation.validateRBAC(rbacApp);
		assertNotNull(result);
		assertEquals(1, result.size());
		assertEquals("Empty right tag of Stereotype <<rbc>> at Element RBACnoRightPackage!", result.get(0));
		this.modelres.unload();
	}
	
	
	/**
	 * This test tests the validation of a &lt;&lt;rbac&gt;&gt; Stereotype that is filled correctly.
	 * The validation should succeed (return true).
	 */
	@Test
	public final void testRBACCorrect() {
		String modelName = "RBACCorrect.uml";
		assertNull(this.modelres);
		loadModel(this.rbacFilePath + File.separator + modelName);
		List<StereotypeApplication> stereoList  = UMLsecUtil.getStereotypeApplications(this.model, UMLsec.RBAC);
		assertNotNull(stereoList);
		assertEquals(1, stereoList.size());
		StereotypeApplication rbacApp = stereoList.get(0);
		assertNotNull(rbacApp);
		List<String> resultSingle = UMLsecValidation.validateRBAC(rbacApp);
		assertNotNull(resultSingle);
		assertTrue(resultSingle.isEmpty());
		List<String> resultModel = UMLsecValidation.validateModel(this.model);
		assertNotNull(resultModel);
		assertTrue(resultModel.isEmpty());
		this.modelres.unload();
	}
	
	/**
	 * This test tests a model with <<rbac>> and <<authorized-status>> stereotype, both wrong filled.
	 */
	@Test
	public final void testMultipleWrong() {
		String modelName = "RBACFairExchangeWrong.uml";
		assertNull(this.modelres);
		loadModel(this.multiplePath + File.separator + modelName);
		List<String> result = UMLsecValidation.validateModel(this.model);
		assertNotNull(result);
		assertEquals(5, result.size());
		assertTrue(result.contains("Empty Tag start of Stereotype <<fair exchange>> at Element RBACFairExchangeWrong!"));
		assertTrue(result.contains("Empty Tag stop of Stereotype <<fair exchange>> at Element RBACFairExchangeWrong!"));
		assertTrue(result.contains("Empty protected tag of Stereotype <<rbc>> at Element RBACFairExchangeWrong!"));
		assertTrue(result.contains("Empty role tag of Stereotype <<rbc>> at Element RBACFairExchangeWrong!"));
		assertTrue(result.contains("Empty right tag of Stereotype <<rbc>> at Element RBACFairExchangeWrong!"));
		this.modelres.unload();
	}
	
	/**
	 * This method tests if all the UMLsec-Stereotypes that have no need of checking won't produce errors.
	 */
	@Test
	
	public final void testNoCheckStereotypes() {
		String modelName = "resources" + File.separator + "models" + File.separator + "StereotypesNotToCheck" + File.separator + "StereotypesNotToCheck.uml";
		assertNull(this.modelres);
		loadModel(modelName);
		List<String> result = UMLsecValidation.validateModel(this.model);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}
	
	/**
	 * This test tests the findModelElements method.
	 */
	@Test
	public final void testFindModelElements() {
		String modelName = "ModelElements.uml";
		assertNull(this.modelres);
		loadModel(this.modelElementsFilePath + File.separator + modelName);
		assertTrue(UMLsecValidation.findModelElement(this.model, "OpaqueAction1"));
		assertFalse(UMLsecValidation.findModelElement(this.model, "OpaqueAction2"));
		assertFalse(UMLsecValidation.findModelElement(this.model, "OpaqueAction3"));
	}
	
	/**
	 * This test tests the testCheckTuple method.
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void testCheckTuple() {
		assertFalse(UMLsecValidation.checkTuple(""));
		assertFalse(UMLsecValidation.checkTuple("(,)"));
		assertFalse(UMLsecValidation.checkTuple(" (E1,E2)"));
		assertFalse(UMLsecValidation.checkTuple("(E1,E2)"));
		assertFalse(UMLsecValidation.checkTuple("(E1,E2)(E3,E4)"));
		assertTrue(UMLsecValidation.checkTuple("(E1, E2)"));
	}
	
	/**
	 * .
	 */
//	TODO JavaDoc, this test tests the method to check all Sterotypes in a model
	@Test
	public final void testAllStereo() {
		String modelName = "resources" + File.separator + "secureLinks.uml";
		assertNull(this.modelres);
		loadModel(modelName);
//		assertTrue(UMLsecValidation.validateModel(model));
	}
	
	/**
	 * This test calls the Constructor expecting one String and test if an Exception will be thrown if the String does not lead to a file.
	 * @throws IOException 
	 * @throws IllegalArgumentException 
	 * @throws FileNotFoundException 
	 */
	@SuppressWarnings("static-method")
	@Test(expected = Exception.class)
	public final void testValidateModelNullFile() throws FileNotFoundException, IllegalArgumentException, IOException {
		UMLsecValidation.validateModel(" ");	}
}
