package carisma.check.ids.tests;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Node;
import org.junit.Test;

import carisma.check.idschecks.trustmanagementcheck.TrustManagementCheck;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.umlsec4ids.*;

/**
 * JUnit test-file for the TrustManagement of the Umlsec4IDS plugin.
 * @author Alexander Peikert
 *
 */

public class TrustManagementTest{
	private String filepath = "resources/models/trustmanagement";
	
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
	public void testEmptyModelTM() throws IOException {
		loadModel("trustmanagement_empty_model.uml");
		TrustManagementCheck check1 = new TrustManagementCheck();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(check1.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for communication with no security profile
	@Test
	public void testNoProfile1TM() throws IOException {
		loadModel("trustmanagement_no_profile.uml");
		TrustManagementCheck check2 = new TrustManagementCheck();
		ArrayList<Node> nodeList = (ArrayList<Node>) UMLHelper.getAllElementsOfType(model, Node.class);
		List<Element> basefree = UMLsecUtil.getStereotypedElements(model, UMLsec.BASEFREE);
		List<Element> base = UMLsecUtil.getStereotypedElements(model, UMLsec.BASE);
		List<Element> trust = UMLsecUtil.getStereotypedElements(model, UMLsec.TRUST);
		List<Element> trustplus = UMLsecUtil.getStereotypedElements(model, UMLsec.TRUSTPLUS);
		assertNotEquals(nodeList.size(), (basefree.size()+base.size()+trust.size()+trustplus.size()));
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check2.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for no communication but no security profile
	@Test
	public void testNoProfile2TM() throws IOException {
		loadModel("trustmanagement_single_node_without_profile.uml");
		TrustManagementCheck check3 = new TrustManagementCheck();
		List<Element> basefree = UMLsecUtil.getStereotypedElements(model, UMLsec.BASEFREE);
		List<Element> base = UMLsecUtil.getStereotypedElements(model, UMLsec.BASE);
		List<Element> trust = UMLsecUtil.getStereotypedElements(model, UMLsec.TRUST);
		List<Element> trustplus = UMLsecUtil.getStereotypedElements(model, UMLsec.TRUSTPLUS);
		assertEquals(0, basefree.size());
		assertEquals(0, base.size());
		assertEquals(0, trust.size());
		assertEquals(0, trustplus.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check3.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for unallowed communication
	// gives 2 because communication is investigated bidirectional
	@Test
	public void testUnallowedCommunicationTM() throws IOException {
		loadModel("trustmanagement_communication_not_allowed.uml");
		TrustManagementCheck check4 = new TrustManagementCheck();
		List<Element> basefree = UMLsecUtil.getStereotypedElements(model, UMLsec.BASEFREE);
		List<Element> base = UMLsecUtil.getStereotypedElements(model, UMLsec.BASE);
		List<Element> trust = UMLsecUtil.getStereotypedElements(model, UMLsec.TRUST);
		List<Element> trustplus = UMLsecUtil.getStereotypedElements(model, UMLsec.TRUSTPLUS);
		assertEquals(1, basefree.size());
		assertEquals(1, base.size());
		assertEquals(0, trust.size());
		assertEquals(0, trustplus.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check4.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for multiple security profiles on 1 node
	@Test
	public void testMultipleProfilesTM() throws IOException {
		loadModel("trustmanagement_multiple_profiles.uml");
		TrustManagementCheck check5 = new TrustManagementCheck();
		ArrayList<Node> nodeList = (ArrayList<Node>) UMLHelper.getAllElementsOfType(model, Node.class);
		List<Element> basefree = UMLsecUtil.getStereotypedElements(model, UMLsec.BASEFREE);
		List<Element> base = UMLsecUtil.getStereotypedElements(model, UMLsec.BASE);
		List<Element> trust = UMLsecUtil.getStereotypedElements(model, UMLsec.TRUST);
		List<Element> trustplus = UMLsecUtil.getStereotypedElements(model, UMLsec.TRUSTPLUS);
		assertNotEquals(nodeList.size(), (basefree.size()+base.size()+trust.size()+trustplus.size()));
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check5.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for correct model
	@Test
	public void testCorrectModelTM() throws IOException {
		loadModel("trustmanagement_correct_model.uml");
		TrustManagementCheck check6 = new TrustManagementCheck();
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(check6.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	
	
}