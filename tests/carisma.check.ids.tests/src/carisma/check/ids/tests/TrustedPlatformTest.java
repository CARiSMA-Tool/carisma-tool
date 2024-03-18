package carisma.check.ids.tests;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.io.File;
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


import carisma.check.idscheck.trustedplatformcheck.TrustedPlatformCheck;
import carisma.profile.umlsec.umlsec4ids.*;

/**
 * JUnit test-file for the TrustedPlatform of the Umlsec4IDS plugin.
 * @author Alexander Peikert
 *
 */

public class TrustedPlatformTest{
	private String filepath = "resources/models/trustedplatform";
	
	private ResourceSet rs = new ResourceSetImpl();
	
	private Resource modelres = null;
	
	private Model model = null;
	
	public final void loadModel(final String testmodelname) throws IOException {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
		this.model = (Model) this.modelres.getContents().get(0);
	}
	
	// test for empty model
	@Test
	public void testEmptyModelTP() throws IOException {
		loadModel("trustedplatform_empty_model.uml");
		TrustedPlatformCheck check1 = new TrustedPlatformCheck();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(check1.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for missing stereotypes
	@Test
	public void testNoStereotypesTP() throws IOException {
		loadModel("trustedplatform_no_stereo.uml");
		TrustedPlatformCheck check2 = new TrustedPlatformCheck();
		List<Element> verified = UMLsecUtil.getStereotypedElements(model, UMLsec.VERIFIED);
		List<Element> certified = UMLsecUtil.getStereotypedElements(model, UMLsec.CERTIFIED);
		List<Element> isolated = UMLsecUtil.getStereotypedElements(model, UMLsec.ISOLATED);
		List<Element> encrypted = UMLsecUtil.getStereotypedElements(model, UMLsec.ENCRYPTION);
		assertEquals(0, verified.size());
		assertEquals(0, certified.size());
		assertEquals(0, isolated.size());
		assertEquals(0, encrypted.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check2.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for missing stereotype verification
	@Test
	public void testNoVerificationTP() throws IOException {
		loadModel("trustedplatform_no_verification.uml");
		TrustedPlatformCheck check3 = new TrustedPlatformCheck();
		List<Element> verified = UMLsecUtil.getStereotypedElements(model, UMLsec.VERIFIED);
		List<Element> certified = UMLsecUtil.getStereotypedElements(model, UMLsec.CERTIFIED);
		List<Element> isolated = UMLsecUtil.getStereotypedElements(model, UMLsec.ISOLATED);
		List<Element> encrypted = UMLsecUtil.getStereotypedElements(model, UMLsec.ENCRYPTION);
		assertEquals(1, verified.size());
		assertEquals(2, certified.size());
		assertEquals(2, isolated.size());
		assertEquals(2, encrypted.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check3.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for missing stereotype certification
	@Test
	public void testNoCertificationTP() throws IOException {
		loadModel("trustedplatform_no_certification.uml");
		TrustedPlatformCheck check4 = new TrustedPlatformCheck();
		List<Element> verified = UMLsecUtil.getStereotypedElements(model, UMLsec.VERIFIED);
		List<Element> certified = UMLsecUtil.getStereotypedElements(model, UMLsec.CERTIFIED);
		List<Element> isolated = UMLsecUtil.getStereotypedElements(model, UMLsec.ISOLATED);
		List<Element> encrypted = UMLsecUtil.getStereotypedElements(model, UMLsec.ENCRYPTION);
		assertEquals(2, verified.size());
		assertEquals(1, certified.size());
		assertEquals(2, isolated.size());
		assertEquals(2, encrypted.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check4.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for missing stereotype encryption
	@Test
	public void testNoEncryptionTP() throws IOException {
		loadModel("trustedplatform_no_encryption.uml");
		TrustedPlatformCheck check5 = new TrustedPlatformCheck();
		List<Element> verified = UMLsecUtil.getStereotypedElements(model, UMLsec.VERIFIED);
		List<Element> certified = UMLsecUtil.getStereotypedElements(model, UMLsec.CERTIFIED);
		List<Element> isolated = UMLsecUtil.getStereotypedElements(model, UMLsec.ISOLATED);
		List<Element> encrypted = UMLsecUtil.getStereotypedElements(model, UMLsec.ENCRYPTION);
		assertEquals(2, verified.size());
		assertEquals(2, certified.size());
		assertEquals(2, isolated.size());
		assertEquals(1, encrypted.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check5.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for missing stereotype isolation
	@Test
	public void testNoIsolationTP() throws IOException {
		loadModel("trustedplatform_no_isolation.uml");
		TrustedPlatformCheck check6 = new TrustedPlatformCheck();
		List<Element> verified = UMLsecUtil.getStereotypedElements(model, UMLsec.VERIFIED);
		List<Element> certified = UMLsecUtil.getStereotypedElements(model, UMLsec.CERTIFIED);
		List<Element> isolated = UMLsecUtil.getStereotypedElements(model, UMLsec.ISOLATED);
		List<Element> encrypted = UMLsecUtil.getStereotypedElements(model, UMLsec.ENCRYPTION);
		assertEquals(2, verified.size());
		assertEquals(2, certified.size());
		assertEquals(1, isolated.size());
		assertEquals(2, encrypted.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check6.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for correct model
	@Test
	public void testCorrectModelTP() throws IOException {
		loadModel("trustedplatform_correct_model.uml");
		TrustedPlatformCheck check7 = new TrustedPlatformCheck();
		List<Element> verified = UMLsecUtil.getStereotypedElements(model, UMLsec.VERIFIED);
		List<Element> certified = UMLsecUtil.getStereotypedElements(model, UMLsec.CERTIFIED);
		List<Element> isolated = UMLsecUtil.getStereotypedElements(model, UMLsec.ISOLATED);
		List<Element> encrypted = UMLsecUtil.getStereotypedElements(model, UMLsec.ENCRYPTION);
		assertEquals(2, verified.size());
		assertEquals(2, certified.size());
		assertEquals(2, isolated.size());
		assertEquals(2, encrypted.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(check7.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	
	
}