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


import carisma.check.idscheck.identitymanagementcheck.IdentityManagementCheck;
import carisma.profile.umlsec.umlsec4ids.*;

/**
 * JUnit test-file for the IdentityManagement of the Umlsec4IDS plugin.
 * @author Alexander Peikert
 *
 */

public class IdentityManagementTest{
	private String filepath = "resources/models/identitymanagement";
	
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
	public void testEmptyModelIM() throws IOException {
		loadModel("identitymanagement_empty_model.uml");
		IdentityManagementCheck check1 = new IdentityManagementCheck();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(check1.perform(null, analysisHost));
		this.modelres.unload();
	}
	

	
	// test for a single node without communication
	@Test
	public void testNoCommunicationIM() throws IOException {
		loadModel("identitymanagement_no_communication.uml");
		IdentityManagementCheck check2 = new IdentityManagementCheck();
		List<Element> x509 = UMLsecUtil.getStereotypedElements(model, UMLsec.X509);
		List<Element> x509tls = UMLsecUtil.getStereotypedElements(model, UMLsec.X509TLS);
		assertEquals(1, x509.size());
		assertEquals(1, x509tls.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(check2.perform(null, analysisHost));
		this.modelres.unload();
	}

	
	// test for date in the past
	@Test
	public void testDateInPastIM() throws IOException {
		loadModel("identitymanagement_date_in_past.uml");
		IdentityManagementCheck check3 = new IdentityManagementCheck();
		List<Element> x509 = UMLsecUtil.getStereotypedElements(model, UMLsec.X509);
		List<Element> x509tls = UMLsecUtil.getStereotypedElements(model, UMLsec.X509TLS);
		assertEquals(1, x509.size());
		assertEquals(1, x509tls.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check3.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for an invalid date
	@Test
	public void testInvalidDateIM() throws IOException {
		loadModel("identitymanagement_invalid_date.uml");
		IdentityManagementCheck check4 = new IdentityManagementCheck();
		List<Element> x509 = UMLsecUtil.getStereotypedElements(model, UMLsec.X509);
		List<Element> x509tls = UMLsecUtil.getStereotypedElements(model, UMLsec.X509TLS);
		assertEquals(1, x509.size());
		assertEquals(1, x509tls.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check4.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	
	// test for missing stereotype X.509
	@Test
	public void testMissingX509StereoIM() throws IOException {
		loadModel("identitymanagement_no_x509.uml");
		IdentityManagementCheck check5 = new IdentityManagementCheck();
		List<Element> x509 = UMLsecUtil.getStereotypedElements(model, UMLsec.X509);
		List<Element> x509tls = UMLsecUtil.getStereotypedElements(model, UMLsec.X509TLS);
		assertEquals(0, x509.size());
		assertEquals(1, x509tls.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check5.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	
	// test for missing stereotype X.509TLS
	@Test
	public void testMissingX509TLSStereoIM() throws IOException {
		loadModel("identitymanagement_no_x509tls.uml");
		IdentityManagementCheck check6 = new IdentityManagementCheck();
		List<Element> x509 = UMLsecUtil.getStereotypedElements(model, UMLsec.X509);
		List<Element> x509tls = UMLsecUtil.getStereotypedElements(model, UMLsec.X509TLS);
		assertEquals(1, x509.size());
		assertEquals(0, x509tls.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check6.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for non existing date
	@Test
	public void testNonExistingDateIM() throws IOException {
		loadModel("identitymanagement_non_existing_date.uml");
		IdentityManagementCheck check7 = new IdentityManagementCheck();
		List<Element> x509 = UMLsecUtil.getStereotypedElements(model, UMLsec.X509);
		List<Element> x509tls = UMLsecUtil.getStereotypedElements(model, UMLsec.X509TLS);
		assertEquals(1, x509.size());
		assertEquals(1, x509tls.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check7.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for correct model
	@Test
	public void testCorrectModelTP() throws IOException {
		loadModel("identitymanagement_correct_model.uml");
		IdentityManagementCheck check8 = new IdentityManagementCheck();
		List<Element> x509 = UMLsecUtil.getStereotypedElements(model, UMLsec.X509);
		List<Element> x509tls = UMLsecUtil.getStereotypedElements(model, UMLsec.X509TLS);
		assertEquals(2, x509.size());
		assertEquals(2, x509tls.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(check8.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	
	
}