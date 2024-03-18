package carisma.check.ids.tests;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
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


import carisma.check.idscheck.datausagecheck.DataUsageCheck;

import carisma.modeltype.uml2.StereotypeApplication;
import carisma.profile.umlsec.umlsec4ids.*;

/**
 * JUnit test-file for the DataUsageControl of the Umlsec4IDS plugin.
 * @author Alexander Peikert
 *
 */

public class DataUsageControlTest{
	private String filepath = "resources/models/datausagecontrol";
	
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
	public void testEmptyModelDUC() throws IOException {
		loadModel("datausagecontrol_empty_model.uml");
		DataUsageCheck check1 = new DataUsageCheck();
		List<Element> duc = UMLsecUtil.getStereotypedElements(model, UMLsec.DATAUSAGECONTROL);
		assertEquals(0, duc.size());
		StereotypeApplication ducApp = UMLsecUtil.getStereotypeApplication(this.model, UMLsec.DATAUSAGECONTROL);
		assertNull(ducApp);
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check1.perform(null, analysisHost));
		this.modelres.unload();
	}
	

	
	
	// test for no valid path
	@Test
	public void testNoValidPathDUC() throws IOException {
		loadModel("datausagecontrol_no_valid_path.uml");
		DataUsageCheck check2 = new DataUsageCheck();
		List<Element> duc = UMLsecUtil.getStereotypedElements(model, UMLsec.DATAUSAGECONTROL);
		assertEquals(1, duc.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertTrue(check2.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for action prohibitted
	@Test
	public void testActionProhibittedDUC() throws IOException {
		loadModel("datausagecontrol_action_prohibitted.uml");
		DataUsageCheck check3 = new DataUsageCheck();
		List<Element> duc = UMLsecUtil.getStereotypedElements(model, UMLsec.DATAUSAGECONTROL);
		assertEquals(1, duc.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check3.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for not all actions are permitted
	@Test
	public void testNotAllPermittedDUC() throws IOException {
		loadModel("datausagecontrol_not_all_permitted.uml");
		DataUsageCheck check4 = new DataUsageCheck();
		List<Element> duc = UMLsecUtil.getStereotypedElements(model, UMLsec.DATAUSAGECONTROL);
		assertEquals(1, duc.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check4.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for obligation stop not executed
	@Test
	public void testObligationStopNotExecutedDUC() throws IOException {
		loadModel("datausagecontrol_obligation_stop_not_executed.uml");
		DataUsageCheck check5 = new DataUsageCheck();
		List<Element> duc = UMLsecUtil.getStereotypedElements(model, UMLsec.DATAUSAGECONTROL);
		assertEquals(1, duc.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check5.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for different amount of obligation starts and stops
	@Test
	public void testDifferentAmountStartStopsDUC() throws IOException {
		loadModel("datausagecontrol_different_amount_obligation_start_stop.uml");
		DataUsageCheck check6 = new DataUsageCheck();
		List<Element> duc = UMLsecUtil.getStereotypedElements(model, UMLsec.DATAUSAGECONTROL);
		assertEquals(1, duc.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check6.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	// test for start action before fork and just one path has stop action
	@Test
	public void testStartBeforeForkDUC() throws IOException {
		loadModel("datausagecontrol_start_before_fork.uml");
		DataUsageCheck check7 = new DataUsageCheck();
		List<Element> duc = UMLsecUtil.getStereotypedElements(model, UMLsec.DATAUSAGECONTROL);
		assertEquals(1, duc.size());
		TestHost analysisHost = new TestHost(this.modelres);
		assertFalse(check7.perform(null, analysisHost));
		this.modelres.unload();
	}
	
	
	
}