package carisma.check.securedependency.inheritance.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import carisma.check.SecureDependencyInheritanceViolation;

import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage.Registry;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.URIConverter;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.mapping.ecore2xml.Ecore2XMLPackage;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.UMLPackage;
import org.eclipse.uml2.uml.resource.UMLResource;
import org.eclipse.uml2.uml.resources.util.UMLResourcesUtil;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import carisma.check.SecureDependencyInheritanceChecks;
import carisma.check.SecureDependencyInheritanceViolation;
import carisma.profile.umlsec.UMLsecActivator;
import carisma.profile.umlsec.UmlsecPackage;

public class SecureDependencyInheritanceChecksTest {

	private String filepath = "resources";

	private ResourceSet rs;
	private Model model = null;

	@After
	public final void cleanup() {
		this.model.eResource().unload();
		this.model = null;
	}

	@Before
	public final void init() throws FileNotFoundException, IOException {
		UMLResourcesUtil.initGlobalRegistries();
		
		this.rs = new ResourceSetImpl();
		UMLResourcesUtil.initLocalRegistries(rs);
		
		Map<String, Object> extensionToFactoryMap = Resource.Factory.Registry.INSTANCE.getExtensionToFactoryMap();
		extensionToFactoryMap.put(UMLResource.FILE_EXTENSION,
				UMLResource.Factory.INSTANCE);
		extensionToFactoryMap.put("*",
				UMLResource.Factory.INSTANCE);

		Registry packageRegistry = this.rs.getPackageRegistry();
		packageRegistry.put(UmlsecPackage.eNS_URI, UmlsecPackage.eINSTANCE);
		UMLsecActivator.loadUMLsecProfile(this.rs);	
		EcoreUtil.resolveAll(this.rs);
	}

	@Test
	public final void SecureDependencyInheritanceTest1() {
		this.model = loadModel(this.filepath, "resources.uml");
		// callDep-i-but-S-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	@Test
	public final void SecureDependencyInheritanceTest2() {
		this.model = loadModel(this.filepath, "model.uml");
		// callDep-i-but-S-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	@Test
	public final void SecureDependencyInheritanceTest3() {
		this.model = loadModel(this.filepath, "model2.uml");
		// callDep-i-but-S-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}
	@Test
	public final void SecureDependencyInheritanceTest4() {
		this.model = loadModel(this.filepath, "model3.uml");
		// callDep-i-but-S-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}
	@Test
	public final void SecureDependencyInheritanceTest5() {
		this.model = loadModel(this.filepath, "model4.uml");
		// callDep-i-but-S-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}	
	@Test
	public final void SecureDependencyInheritanceTest6() {
		this.model = loadModel(this.filepath, "model5.uml");
		// callDep-i-but-S-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}	

	@Test
	public final void SecureDependencyInheritanceTest7() {
		this.model = loadModel(this.filepath, "model6.uml");
		// callDep-i-but-S-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}	
	@Test
	public final void SecureDependencyInheritanceTest8() {
		this.model = loadModel(this.filepath, "model7.uml");
		// callDep-i-but-S-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}	
	@Test
	public final void SecureDependencyInheritanceTest9() {
		this.model = loadModel(this.filepath, "model8.uml");
		// callDep-i-but-S-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}

	
/*	@Test
	public final void SecureDependencyInheritanceTest2() {
		this.model = loadModel(this.filepath, "callDep-i.uml");
		// callDep-i.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}

	@Test
	public final void SecureDependencyInheritanceTest3() {
		this.model = loadModel(this.filepath, "callDep-sec-but-i-V.uml");
		// callDep-sec-but-i-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}

	@Test
	public final void SecureDependencyInheritanceTest4() {
		this.model = loadModel(this.filepath, "callDep-V.uml");
		// callDep-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}

	@Test
	public final void SecureDependencyInheritanceTest5() {
		this.model = loadModel(this.filepath, "callDep-Vi.uml");
		// callDep-Vi.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}

	@Test
	public final void SecureDependencyInheritanceTest6() {
		this.model = loadModel(this.filepath, "callDep.uml");
		// callDep.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	// this diagram is not viewable somehow...
	@Test
	public final void SecureDependencyInheritanceTest7() {
		this.model = loadModel(this.filepath, "ClientWithSec-noSE-V.uml");
		// ClientWithSec-noSE-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	// to do: recheck this diagram.
	@Test
	public final void SecureDependencyInheritanceTest8() {
		this.model = loadModel(this.filepath, "ClientWithSecInt-InhInt-V.uml");
		// ClientWithSecInt-InhInt-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	// to do: recheck this diagram.
	@Test
	public final void SecureDependencyInheritanceTest9() {
		this.model = loadModel(this.filepath, "ClientWithSecInt-InhSec-UInt-V.uml");
		// ClientWithSecInt-InhSec-UInt-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	@Test
	public final void SecureDependencyInheritanceTest10() {
		this.model = loadModel(this.filepath, "Inh-SP-addI-callDep-int-V.uml");
		// Inh-SP-addI-callDep-int-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}

	@Test
	public final void SecureDependencyInheritanceTest11() {
		this.model = loadModel(this.filepath, "Inh-SP-addI-callDep-sec-V.uml");
		// Inh-SP-addI-callDep-sec-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}

	@Test
	public final void SecureDependencyInheritanceTest12() {
		this.model = loadModel(this.filepath, "Inh-SP-addI-callDep.uml");
		// Inh-SP-addI-callDep.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}

	@Test
	public final void SecureDependencyInheritanceTest13() {
		this.model = loadModel(this.filepath, "Inh-SP-addInt-callDep-int-sec.uml");
		// Inh-SP-addInt-callDep-int-sec.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	// to do: recheck, has no violation
	@Test
	public final void SecureDependencyInheritanceTest14() {
		this.model = loadModel(this.filepath, "Inh-SP-callD-w-SP-V.uml");
		// Inh-SP-addInt-callDep-int-sec.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	@Test
	public final void SecureDependencyInheritanceTest15() {
		this.model = loadModel(this.filepath, "Inh-SP-callDep.uml");
		// Inh-SP-callDep.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	@Test
	public final void SecureDependencyInheritanceTest16() {
		this.model = loadModel(this.filepath, "Inherite-SP-callD-V.uml");
		// Inherite-SP-callD-V.di
		SecureDependencyInheritanceChecks sdc = new SecureDependencyInheritanceChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyInheritanceViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	
*/

	private Model loadModel(String filepath, String name) {
		File file = new File(new File(filepath), name);
		if (file.exists()) {
			URI umlResourcePluginURI = URI.createURI(file.getAbsolutePath());
			Resource r = rs.createResource(umlResourcePluginURI);
			try (FileInputStream in = new FileInputStream(file)) {
				r.load(in, Collections.EMPTY_MAP);
				EList<EObject> contents = r.getContents();
				assertTrue(1 <= contents.size());
				EObject obj = contents.get(0);
				assertTrue(obj instanceof Model);
				return (Model) obj;
			} catch (IOException e) {
				e.printStackTrace();
				fail();
			}
		} else {
			fail("File \"" + new File(new File(filepath), name) + "\" doesn't exist!");
		}
		return null;
	}

	protected static void registerPathmaps(URI umlResourcePluginURI) {
		URIConverter.URI_MAP.put(URI.createURI(UMLResource.LIBRARIES_PATHMAP),
				umlResourcePluginURI.appendSegment("libraries").appendSegment(""));
		URIConverter.URI_MAP.put(URI.createURI(UMLResource.METAMODELS_PATHMAP),
				umlResourcePluginURI.appendSegment("metamodels").appendSegment(""));
		URIConverter.URI_MAP.put(URI.createURI(UMLResource.PROFILES_PATHMAP),
				umlResourcePluginURI.appendSegment("profiles").appendSegment(""));

	}

}
