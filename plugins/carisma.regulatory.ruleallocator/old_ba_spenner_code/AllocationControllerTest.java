package carisma.regulatory.ruleallocator.OLD;

import static org.junit.Assert.*;

import java.io.IOException;
import java.util.Iterator;


import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.junit.Test;

import carisma.regulatory.ruleallocator.datamodel.Allocation;
import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.DatamodelFactory;
import carisma.regulatory.ruleallocator.datamodel.ModelElementType;
import carisma.regulatory.ruleallocator.datamodel.RuleElement;
import carisma.regulatory.ruleallocator.datamodel.RuleElementType;


public class AllocationControllerTest {
	AllocationControllerOLD allo = new AllocationControllerOLD();
	DatamodelFactory modelFactory = DatamodelFactory.eINSTANCE;
	
	@Test
	public void testAllocate() {
		Boolean set = false;
		BPMNElement bpmn = modelFactory.createBPMNElement();
		RuleElement rule = modelFactory.createRuleElement();
		RuleElementType ruleType = modelFactory.createRuleElementType();
		ModelElementType modelType = modelFactory.createModelElementType();
		Container con = modelFactory.createContainer();
		
		
		modelType.setName("task");
		bpmn.setName("fahren");
		ruleType.setName("Activity");
		modelType.getAssociatsWith().add(ruleType);
		bpmn.setType(modelType);
		rule.setName("fahren");
		rule.setType(ruleType);
		try {
			allo.allocate(bpmn, rule);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		ResourceSet resourceSet = new ResourceSetImpl();
		
		// Register XML resource factory
		resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put("xmi", new XMIResourceFactoryImpl());

		Resource resource = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("c:/data1.xmi"));
		Resource resource1 = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("c:/data1.xmi"));
//		Resource resource = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("/tmp/dynamicData.xmi"));
//		Resource resource1 = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("/tmp/dynamicData.xmi"));
		
		try {
			resource.load(null);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		con =  (Container) resource.getContents().get(0);
		
		for (Iterator<RuleElementType> iterator = con.getContainsAllocation().iterator(); iterator.hasNext();) {
			  
		      Allocation allo = (Allocation) iterator.next();
		      if(allo.getRuleElement().getName().equals(rule.getName()) && allo.getBpmnElement().getName().equals(bpmn.getName())) {
		    	  set = true;
		      }
		      
		}
		
		assertTrue(set);
		
		
	}

	@Test
	public void testRemoveAllocation() {
		Boolean set = false;
		Boolean set2 = false;
		BPMNElement bpmn = modelFactory.createBPMNElement();
		RuleElement rule = modelFactory.createRuleElement();
		RuleElementType ruleType = modelFactory.createRuleElementType();
		ModelElementType modelType = modelFactory.createModelElementType();
		Allocation allo2 = modelFactory.createAllocation();
		Container con = modelFactory.createContainer();
		Container con2 = modelFactory.createContainer();
		
		modelType.setName("task");
		bpmn.setName("fahren");
		ruleType.setName("Activity");
		modelType.getAssociatsWith().add(ruleType);
		bpmn.setType(modelType);
		rule.setName("fahren");
		rule.setType(ruleType);
		
		allo2.setBpmnElement(bpmn);
		allo2.setRuleElement(rule);
		
		ResourceSet resourceSet = new ResourceSetImpl();
		
		// Register XML resource factory
		resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put("xmi", new XMIResourceFactoryImpl());

		Resource resource = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("c:/data1.xmi"));
		Resource resource1 = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("c:/data1.xmi"));
//		Resource resource = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("/tmp/dynamicData.xmi"));
//		Resource resource1 = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("/tmp/dynamicData.xmi"));
		
		try {
			resource.load(null);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		con =  (Container) resource.getContents().get(0);
		
		for (Iterator<RuleElementType> iterator = con.getContainsAllocation().iterator(); iterator.hasNext();) {
			  
		      Allocation allo3 = (Allocation) iterator.next();
		      if(allo3.getRuleElement().getName().equals(allo2.getRuleElement().getName()) && allo3.getBpmnElement().getName().equals(allo2.getBpmnElement().getName())) {
		    	  set = true;
		      }
		      
		}
		
		assertTrue(set);
		
		try {
			allo.removeAllocation(allo2);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			resource1.load(null);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		con2 =  (Container) resource1.getContents().get(0);
		
		
		
		for (Iterator<RuleElementType> iterator = con2.getContainsAllocation().iterator(); iterator.hasNext();) {
			  
		      Allocation allo3 = (Allocation) iterator.next();
		      if(allo3.getRuleElement().getName().equals(allo2.getRuleElement().getName()) && allo3.getBpmnElement().getName().equals(allo2.getBpmnElement().getName())) {
		    	  set2 = true;
		      }
		      
		}
		
		assertFalse(set2);
		
		try {
			allo.allocate(bpmn, rule);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

	@Test
	public void testCheckTypes() {
		BPMNElement bpmn = modelFactory.createBPMNElement();
		RuleElement rule = modelFactory.createRuleElement();
		RuleElementType ruleType = modelFactory.createRuleElementType();
		ruleType.setName("Role");
		ModelElementType modelType = modelFactory.createModelElementType();
		modelType.setName("swimlane");
		modelType.getAssociatsWith().add(ruleType);
		bpmn.setType(modelType);
		rule.setType(ruleType);
		rule.setName("Manager");
		try {
			assertTrue(allo.checkTypes(bpmn, rule));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		ModelElementType modelType2 = modelFactory.createModelElementType();
		RuleElementType ruleType2 = modelFactory.createRuleElementType();
		ruleType2.setName("Object");
		modelType2.setName("DataInput");
		rule.setType(ruleType2);
		modelType2.getAssociatsWith().add(ruleType2);
		try {
			assertFalse(allo.checkTypes(bpmn, rule));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		

		
		
		
		
		
	}

	@Test
	public void testReadBPMN() {
		BPMNElement bpmn = modelFactory.createBPMNElement();
		ModelElementType modelType = modelFactory.createModelElementType();
		bpmn.setName("AuthorizePayment");
		bpmn.setID("_7");
		modelType.setName("callActivity");
		bpmn.setType(modelType);
		
		ResourceSet resourceSet = new ResourceSetImpl();
		
		// Register XML resource factory
		resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put("xmi", new XMIResourceFactoryImpl());
		Resource resource = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("c:/a1.xmi"));

		
		Container con = modelFactory.createContainer();
		con.getContainsBPMNElement().add(bpmn);
		con.getContainsModelType().add(modelType);
		resource.getContents().add(con);
		try {
			resource.save(null);
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		
		
		
		try {
			assertEquals(bpmn.getName(), allo.readBPMN().getName());
			assertEquals(bpmn.getID(), allo.readBPMN().getID());
			assertEquals(bpmn.getType().getName(), allo.readBPMN().getType().getName());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

}
