package carisma.check.securedependency.inheritance.test;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.check.SecureDependencyChecks;
import carisma.check.SecureDependencyViolation;


public class SecureDependencyInheritanceChecksTest {
	
	private String filepath = "resources/InheritanceCases";
	
	private Model model = null; 
	
	@After
	public final void cleanup() {
		this.model.eResource().unload();
		this.model = null;
	}
	
	@Test
	public final void SecureDependencyInheritanceTest1(){
		this.model = loadModel(this.filepath, "callDep-i-but-S-V.uml");
		//callDep-i-but-S-V.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest2(){
		this.model = loadModel(this.filepath, "callDep-i.uml");
		//callDep-i.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest3(){
		this.model = loadModel(this.filepath, "callDep-i-but-i-V.uml");
		//callDep-sec-but-i-V.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest4(){
		this.model = loadModel(this.filepath, "callDep-V.uml");
		//callDep-V.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest5(){
		this.model = loadModel(this.filepath, "callDep-Vi.uml");
		//callDep-Vi.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest6(){
		this.model = loadModel(this.filepath, "callDep.uml");
		//callDep.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}
	// this diagram is not viewable somehow... 
	public final void SecureDependencyInheritanceTest7(){
		this.model = loadModel(this.filepath, "ClientWithSec-noSE.uml");
		//ClientWithSecInt-InhInt-V.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}
	//to do: recheck this diagram. 
	public final void SecureDependencyInheritanceTest8(){
		this.model = loadModel(this.filepath, "ClientWithSecInt-InhInt-V.uml");
		//ClientWithSecInt-InhInt-V.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}
	//to do: recheck this diagram. 
	public final void SecureDependencyInheritanceTest9(){
		this.model = loadModel(this.filepath, "ClientWithSecInt-InhSec-UInt-V.uml");
		//ClientWithSecInt-InhSec-UInt-V.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest10(){
		this.model = loadModel(this.filepath, "Inh-SP-addI-callDep-int-V.uml");
		//Inh-SP-addI-callDep-int-V.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest11(){
		this.model = loadModel(this.filepath, "Inh-SP-addI-callDep-sec-V.uml");
		//Inh-SP-addI-callDep-sec-V.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest12(){
		this.model = loadModel(this.filepath, "Inh-SP-addI-callDep.uml");
		//Inh-SP-addI-callDep.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest13(){
		this.model = loadModel(this.filepath, "Inh-SP-addInt-callDep-int-sec.uml");
		//Inh-SP-addInt-callDep-int-sec.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}
	// to do: recheck, has no violation
	public final void SecureDependencyInheritanceTest14(){
		this.model = loadModel(this.filepath, "Inh-SP-callDep-w-SP-V.uml");
		//Inh-SP-addInt-callDep-int-sec.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest15(){
		this.model = loadModel(this.filepath, "Inh-SP-callDep.uml");
		//Inh-SP-callDep.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}
	public final void SecureDependencyInheritanceTest16(){
		this.model = loadModel(this.filepath, "Inherite-SP-callD-V.uml");
		//Inherite-SP-callD-V.di
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
	}
	
	
	
	
	
	
	private Model loadModel(String filepath, String name) {
		File file = new File(new File(filepath), name);
		if(file.exists()){
			Resource r = new ResourceSetImpl().createResource(URI.createURI(file.getAbsolutePath()));
			try(FileInputStream in = new FileInputStream(file)){
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
		}
		fail();
		return null;
	}
	
	


}
