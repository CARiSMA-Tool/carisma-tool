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

import carisma.check.staticcheck.securelinks.SecureLinks;
import carisma.check.staticcheck.securelinks.SecureLinksHelper;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.profile.umlsec.extension4ids.Extension4IDS;
import carisma.profile.umlsec.extension4ids.Extension4IDSUtil;

public class UsageControlTests {
	private String filepath = "resources/models/usage_control";
	
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
	public final void testRequirements() throws IOException {
		loadModel("testRequirements.uml");
		NamedElement ne = this.model.getMember("pkg");
		assertNotNull(ne);
		Package pkg = (Package) ne;
		ne = pkg.getMember("dep");
		Dependency dep = (Dependency) ne;
		assertEquals(4, dep.getAppliedStereotypes().size());
		StereotypeApplication requirementApp = Extension4IDSUtil.getStereotypeApplication(dep, Extension4IDS.USAGECONTROL);
		assertNotNull(requirementApp);
		assertTrue(SecureLinksHelper.isSecureLinksRequirement(requirementApp.getAppliedStereotype()));
		
		this.modelres.unload();
	}
	
	@Test
	public final void testCheckWrongLinktype() throws IOException {
		loadModel("testDeploymentWrongLinktype.uml");
		SecureLinks theCheck = new SecureLinks(null);
		assertEquals(1, theCheck.checkSecureLinks(this.model));
	}
	
	@Test
	public final void testCheckRightLinktype() throws IOException {
		loadModel("testDeploymentRightLinktype.uml");
		SecureLinks theCheck = new SecureLinks(null);
		assertEquals(0, theCheck.checkSecureLinks(this.model));
	}
	
	@Test
	public final void testCheckWrongCustomMultipleRequirements() throws IOException {
		loadModel("testDeploymentWrongCustomMultipleRequirements.uml");
		SecureLinks theCheck = new SecureLinks(null);
		assertEquals(1, theCheck.checkSecureLinks(this.model));
	}
	
	@Test
	public final void testCheckNonExtension4idsStereotypes() throws IOException {
		loadModel("testDeploymentNonExtension4idsStereotype.uml");
		SecureLinks theCheck = new SecureLinks(null);
		assertEquals(0, theCheck.checkSecureLinks(this.model));
	}
	
	@After
	public void unloadModel(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}
}
