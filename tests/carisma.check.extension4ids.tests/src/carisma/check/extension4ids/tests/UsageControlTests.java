package carisma.check.extension4ids.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.Collections;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.junit.After;
import org.junit.Test;

import carisma.check.extension4ids.usagecontrol.UsageControl;
import carisma.check.extension4ids.usagecontrol.UsageControlHelper;
import carisma.check.staticcheck.securelinks.SecureLinks;
import carisma.check.staticcheck.securelinks.SecureLinksHelper;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.profile.umlsec.extension4ids.Extension4IDS;
import carisma.profile.umlsec.extension4ids.Extension4IDSUtil;

public class UsageControlTests {
	private String filepath = "resources/usage_control";
	
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
	
	
	@Test
	public final void testNoCommunicatonPath() throws IOException {
		loadModel("usage_control_no_communication_path.uml");
		UsageControl theCheck = new UsageControl(null);
		assertEquals(1, theCheck.checkUsageControl(this.model));
	}
	
	@Test
	public final void testNoDependencyLink() throws IOException {
		loadModel("usage_control_no_dependency_link.uml");
		UsageControl theCheck = new UsageControl(null);
		assertEquals(0, theCheck.checkUsageControl(this.model));
	}
	
	@Test
	public final void testOnlyOneIdsConnector() throws IOException {
		loadModel("usage_control_no_ids_connector.uml");
		UsageControl theCheck = new UsageControl(null);
		assertEquals(1, theCheck.checkUsageControl(this.model));
	}
	
	
	@Test
	public final void testNoRequirement() throws IOException {
		loadModel("usage_control_no_usage_control_requirement.uml");
		UsageControl theCheck = new UsageControl(null);
		assertEquals(0, theCheck.checkUsageControl(this.model));
	}
	
	@Test
	public final void testCheckSuccess() throws IOException {
		loadModel("usage_control_check_success.uml");
		UsageControl theCheck = new UsageControl(null);
		assertEquals(0, theCheck.checkUsageControl(this.model));
	}
	
	@After
	public void unloadModel(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}
}
