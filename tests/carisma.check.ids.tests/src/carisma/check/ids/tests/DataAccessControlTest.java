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

import carisma.check.idscheck.dataaccesscheck.DataAccessCheck;
import carisma.profile.umlsec.umlsec4ids.*;



/**
 * JUnit test-file for the DataAccessControl of the Umlsec4IDS plugin.
 * @author Alexander Peikert
 *
 */

public class DataAccessControlTest{
	private String filepath = "resources/models/dataaccesscontrol";
	
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
	
	//tests for empty model
	// should be 1, but it returns 4 as no owner, no consumer and missing protected action s returned also
	@Test
	public void testEmptyModelDAC() throws IOException {
		loadModel("data_access_control_empty_model.uml");
		DataAccessCheck check1 = new DataAccessCheck();
		List<Element> owner = UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		List<Element> consumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		assertEquals(0, owner.size());
		assertEquals(0, consumer.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check1.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	//tests for no existing path
	@Test
	public void testNoPathDAC() throws IOException {
		loadModel("data_access_control_no_path.uml");
		DataAccessCheck check2 = new DataAccessCheck();
		List<Element> owner = UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		List<Element> consumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		assertEquals(1, owner.size());
		assertEquals(1, consumer.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check2.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	//tests for no <<Owner>> stereotype
	@Test
	public void testNoOwnerDAC() throws IOException {
		loadModel("data_access_control_no_owner.uml");
		DataAccessCheck check3 = new DataAccessCheck();
		List<Element> owner = UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		List<Element> consumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		assertEquals(0, owner.size());
		assertEquals(1, consumer.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check3.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	//tests for no <<Consumer>> stereotype
	@Test
	public void testNoConsumerDAC() throws IOException {
		loadModel("data_access_control_no_consumer.uml");
		DataAccessCheck check4 = new DataAccessCheck();
		List<Element> owner = UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		List<Element> consumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		assertEquals(1, owner.size());
		assertEquals(0, consumer.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check4.perform(null, analysisHost));
		this.modelres.unload();
	}

	//tests for <<Owner>> and <<Consumer>> on same partition
	@Test
	public void testStereoSamePartitionDAC() throws IOException {
		loadModel("data_access_control_owner_consumer_same_partition.uml");
		DataAccessCheck check5 = new DataAccessCheck();
		List<Element> owner = UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		List<Element> consumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		assertEquals(1, owner.size());
		assertEquals(1, consumer.size());
		assertEquals(consumer, owner);
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check5.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	//tests for requested attributes not sufficient
	@Test
	public void testRequestedAttributesDAC() throws IOException {
		loadModel("data_access_control_requested_attributes.uml");
		DataAccessCheck check6 = new DataAccessCheck();
		List<Element> owner = UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		List<Element> consumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		assertEquals(1, owner.size());
		assertEquals(1, consumer.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check6.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	//tests for requested actions not sufficient
	@Test
	public void testRequestedActiosDAC() throws IOException {
		loadModel("data_access_control_requested_actions.uml");
		DataAccessCheck check7 = new DataAccessCheck();
		List<Element> owner = UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		List<Element> consumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		assertEquals(1, owner.size());
		assertEquals(1, consumer.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check7.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	//tests for subpartitions
	@Test
	public final void testSuccess_subpartitions() throws IOException {
		loadModel("data_access_control_subpartitions.uml");
		DataAccessCheck check8 = new DataAccessCheck();
		List<Element> owner = UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		List<Element> consumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		assertEquals(1, owner.size());
		assertEquals(1, consumer.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(check8.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	//tests for correct model
	@Test
	public final void testSuccess() throws IOException {
		loadModel("data_access_correct_model.uml");
		DataAccessCheck check9 = new DataAccessCheck();
		List<Element> owner = UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		List<Element> consumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		assertEquals(1, owner.size());
		assertEquals(1, consumer.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(check9.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	
}