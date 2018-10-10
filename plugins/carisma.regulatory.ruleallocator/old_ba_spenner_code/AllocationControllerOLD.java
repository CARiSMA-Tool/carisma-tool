package carisma.regulatory.ruleallocator.OLD;


import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;

import carisma.regulatory.ruleallocator.datamodel.Allocation;
import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.DatamodelFactory;
import carisma.regulatory.ruleallocator.datamodel.DatamodelPackage;
import carisma.regulatory.ruleallocator.datamodel.ModelElementType;
import carisma.regulatory.ruleallocator.datamodel.RuleElement;
import carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation;
import carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType;
import carisma.regulatory.ruleallocator.datamodel.RuleElementType;
import carisma.regulatory.ruleallocator.datamodel.Situation;



public class AllocationControllerOLD {

	public RuleElement rule;
	public DatamodelPackage package1 = DatamodelPackage.eINSTANCE;
	public DatamodelFactory modelFactory = DatamodelFactory.eINSTANCE;
	public org.eclipse.emf.common.util.URI fileURI;
	static String path1;
	static Boolean one = false;
	static Boolean two = false;
	static Boolean three = false;
	
	/**
	 * Constructor.
	 * (jk)
	 */
	public AllocationControllerOLD() {
		
	}
	
	public void setRecource (String path) {
		path1 = path;
	}
	
	

	
	public Allocation allocate(BPMNElement bpmn, RuleElement rule) throws IOException{

		Container newContainer = modelFactory.createContainer();
		Allocation allo = modelFactory.createAllocation();
		BPMNElement bpmn1 = modelFactory.createBPMNElement();
		ModelElementType bpmnType = modelFactory.createModelElementType();

		
		
		

		bpmnType.setName(bpmn.getType().getName());

		bpmn1.setName(bpmn.getName());
		bpmn1.setID(bpmn.getID());
		bpmn1.setProcessId(bpmn.getID());
		bpmn1.setType(bpmnType);
		

		
		
		// create resource set and resource 
		ResourceSet resourceSet = new ResourceSetImpl();
		
		// Register XML resource factory
		resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put("xmi", new XMIResourceFactoryImpl());

		Resource resourceToLoad = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI(path1));
		Resource resourceToSave = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI(path1));
//		Resource resource = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("/tmp/dynamicData.xmi"));
//		Resource resource1 = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("/tmp/dynamicData.xmi"));
		
		resourceToLoad.load(null);
		List<RuleElement> ruleElementList = new ArrayList<RuleElement>();
		List<RuleElementType> ruleElementTypeList = new ArrayList<RuleElementType>();
		List<RuleElementAssociation> ruleElementAssociationList = new ArrayList<RuleElementAssociation>();
		List<RuleElementAssociationType> ruleElementAssociationTypeList = new ArrayList<RuleElementAssociationType>();
		List<BPMNElement> bpmnElementList = new ArrayList<BPMNElement>();
		List<ModelElementType> modelElementTypeList = new ArrayList<ModelElementType>();
		List<Allocation> allocationList = new ArrayList<Allocation>();
		List<Situation> situationList = new ArrayList<Situation>();
		Container loadedContainer = (Container) resourceToLoad.getContents().get(0);
	
		
		
		for (Iterator<RuleElement> iterator = loadedContainer.getContainsRuleElement().iterator(); iterator.hasNext();) {
		      RuleElement rule1 = iterator.next();
		      if (rule1.getName().equals(rule.getName())) {
		    	  allo.setRuleElement(rule1);
		      }
		      ruleElementList.add(rule1);
		}

		for (Iterator<RuleElementType> iterator = loadedContainer.getContainsRuleElementType().iterator(); iterator.hasNext();) {
		      RuleElementType rule2 = iterator.next();
		      ruleElementTypeList.add(rule2);
		}
		
		for (Iterator<RuleElementAssociation> iterator = loadedContainer.getContainsRuleElementAssociation().iterator(); iterator.hasNext();) {  
			RuleElementAssociation rule2 = iterator.next();
		    ruleElementAssociationList.add(rule2);
		}
		
		for (Iterator<RuleElementAssociationType> iterator = loadedContainer.getContainsRuleElementAssociationType().iterator(); iterator.hasNext();) {
			  
			RuleElementAssociationType rule2 = iterator.next();
		      ruleElementAssociationTypeList.add(rule2);
		}
		
		for (Iterator<BPMNElement> iterator = loadedContainer.getContainsBPMNElement().iterator(); iterator.hasNext();) {
			  
			BPMNElement bpmn3 = iterator.next();
			if (bpmn3.getName().equals(bpmn.getName())) {
				one = true;
			}
			bpmnElementList.add(bpmn3);
		}
		
		if (one == false ) {
			newContainer.getContainsBPMNElement().add(bpmn1);
			allo.setBpmnElement(bpmn1);
		}
		else {
			for (Iterator<BPMNElement> iterator = loadedContainer.getContainsBPMNElement().iterator(); iterator.hasNext();) {
			      BPMNElement rule1 = iterator.next();
			      if (rule1.getName().equals(bpmn.getName())) {
			    	  allo.setBpmnElement(rule1);
			      }
			  
		}
		}
		
		for (Iterator<ModelElementType> iterator = loadedContainer.getContainsModelType().iterator(); iterator.hasNext();) {
			  
			ModelElementType modelType = iterator.next();
			if(modelType.getName().equals(bpmn.getType().getName())) {
				bpmn1.setType(modelType);
				two = true;
			}
			modelElementTypeList.add(modelType);
		}
		
		if (two == false)
			newContainer.getContainsModelType().add(bpmnType);
		
		
		for (Iterator<Allocation> iterator = loadedContainer.getContainsAllocation().iterator(); iterator.hasNext();) {
			  
		Allocation allo1 = iterator.next();
		if(allo1.getBpmnElement().getName().equals(bpmn.getName()) && allo1.getRuleElement().getName().equals(rule.getName())){
			three = true;
		}
			allocationList.add(allo1);
		}
		ModelElementType modelType2 = null;
		
		if(three == false )

		{
			for (Iterator<ModelElementType> iterator = loadedContainer.getContainsModelType().iterator(); iterator.hasNext();) {
			ModelElementType modelType = iterator.next();
			if(modelType.getName().equals(bpmn.getType().getName())){
			modelType2 = modelType;
			}
			
			}
			if (modelType2.getAssociatsWith().size() > 0) {
			RuleElementType typ = (RuleElementType) modelType2.getAssociatsWith().get(0);
			if (modelType2.getAssociatsWith().size() > 0 &&   typ.getName().equals(rule.getType().getName()))
				
			newContainer.getContainsAllocation().add(allo); 
			}
		}
		
		
		for (Iterator<Situation> iterator = loadedContainer.getContainsSituation().iterator(); iterator.hasNext();) {
			  
			Situation rule2 = iterator.next();
		      situationList.add(rule2);
		}
		
		
		
		
		
		
		for (Iterator<RuleElement> iterator = ruleElementList.iterator(); iterator.hasNext();) {
			RuleElement rule1= iterator.next();
			newContainer.getContainsRuleElement().add(rule1);
		}
		for (Iterator<RuleElementType> iterator = ruleElementTypeList.iterator(); iterator.hasNext();) {
			RuleElementType rule2= iterator.next();
			newContainer.getContainsRuleElementType().add(rule2);
		}
		
		for (Iterator<RuleElementAssociation> iterator = ruleElementAssociationList.iterator(); iterator.hasNext();) {
			RuleElementAssociation rule2= iterator.next();
			newContainer.getContainsRuleElementAssociation().add(rule2);
		}
		
		for (Iterator<RuleElementAssociationType> iterator = ruleElementAssociationTypeList.iterator(); iterator.hasNext();) {
			RuleElementAssociationType rule2= iterator.next();
			newContainer.getContainsRuleElementAssociationType().add(rule2);
		}
		
		for (Iterator<BPMNElement> iterator = bpmnElementList.iterator(); iterator.hasNext();) {
			BPMNElement bpmn2 = iterator.next();
			newContainer.getContainsBPMNElement().add(bpmn2);
		}
		
		for (Iterator<ModelElementType> iterator = modelElementTypeList.iterator(); iterator.hasNext();) {
			ModelElementType model1 = iterator.next();
			newContainer.getContainsModelType().add(model1);			
		}
		
		
		for (Iterator<Allocation> iterator = allocationList.iterator(); iterator.hasNext();) {
			Allocation allo1 = iterator.next();
			newContainer.getContainsAllocation().add(allo1);
		}
		
		for (Iterator<Situation> iterator = situationList.iterator(); iterator.hasNext();) {
			Situation allo1 = iterator.next();
			newContainer.getContainsSituation().add(allo1);
		}
		
		

		
		
		
		resourceToSave.getContents().add(newContainer);

	
		resourceToSave.save(null);	
		
		return allo;
	}
	
	public void removeAllocation(Allocation allo) throws IOException {
		
		Container con = modelFactory.createContainer();
		List<Situation> sitList = new ArrayList<Situation>();
		
		// create resource set and resource 
				ResourceSet resourceSet = new ResourceSetImpl();
				
				// Register XML resource factory
				resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put("xmi", new XMIResourceFactoryImpl());

				Resource resource = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI(path1));
				Resource resource1 = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI(path1));
//				Resource resource = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("/tmp/dynamicData.xmi"));
//				Resource resource1 = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("/tmp/dynamicData.xmi"));
				
				resource.load(null);
				List<RuleElement> rulelist = new ArrayList<RuleElement>();
				List<RuleElementType> rulelist1 = new ArrayList<RuleElementType>();
				List<RuleElementAssociation> rulelist2 = new ArrayList<RuleElementAssociation>();
				List<RuleElementAssociationType> rulelist3 = new ArrayList<RuleElementAssociationType>();
				List<BPMNElement> bpmnlist1 = new ArrayList<BPMNElement>();
				List<ModelElementType> bpmntypelist = new ArrayList<ModelElementType>();
				List<Allocation> allolist = new ArrayList<Allocation>();
				Container con2 = (Container) resource.getContents().get(0);
				
			
				for (Iterator<RuleElement> iterator = con2.getContainsRuleElement().iterator(); iterator.hasNext();) {
				      RuleElement rule1 = iterator.next();
				     
				      rulelist.add(rule1);
				}
				
				
				
			
				
				
				
				
				
				
				for (Iterator<RuleElementType> iterator = con2.getContainsRuleElementType().iterator(); iterator.hasNext();) {
					  
				      RuleElementType rule2 = iterator.next();
				      rulelist1.add(rule2);
				}
				
				for (Iterator<RuleElementAssociation> iterator = con2.getContainsRuleElementAssociation().iterator(); iterator.hasNext();) {
					  
					RuleElementAssociation rule2 = iterator.next();
				      rulelist2.add(rule2);
				}
				
				for (Iterator<RuleElementAssociationType> iterator = con2.getContainsRuleElementAssociationType().iterator(); iterator.hasNext();) {
					  
					RuleElementAssociationType rule2 = iterator.next();
				      rulelist3.add(rule2);
				}
				
				for (Iterator<BPMNElement> iterator = con2.getContainsBPMNElement().iterator(); iterator.hasNext();) {
					  
					BPMNElement bpmn3 = iterator.next();
				
					bpmnlist1.add(bpmn3);
				}
				
				
			
				
				for (Iterator<ModelElementType> iterator = con2.getContainsModelType().iterator(); iterator.hasNext();) {
					  
					ModelElementType modelType = iterator.next();
					
					
					bpmntypelist.add(modelType);
				}
				
				
				
				
				for (Iterator<Allocation> iterator = con2.getContainsAllocation().iterator(); iterator.hasNext();) {
					  
				Allocation allo2 = iterator.next();
				if(!(allo.getRuleElement().getName().equals(allo2.getRuleElement().getName()) &&  allo.getBpmnElement().getName().equals(allo2.getBpmnElement().getName()))) {

					allolist.add(allo2);
				}
					
				
				
				}
				
				for (Iterator<Situation> iterator = con2.getContainsSituation().iterator(); iterator.hasNext();) {
					  
					Situation rule2 = iterator.next();
				      sitList.add(rule2);
				}

				
				
				
				
				
				
				
				for (Iterator<RuleElement> iterator = rulelist.iterator(); iterator.hasNext();) {
					RuleElement rule1= iterator.next();
					con.getContainsRuleElement().add(rule1);
				}
				for (Iterator<RuleElementType> iterator = rulelist1.iterator(); iterator.hasNext();) {
					RuleElementType rule2= iterator.next();
					con.getContainsRuleElementType().add(rule2);
				}
				
				for (Iterator<RuleElementAssociation> iterator = rulelist2.iterator(); iterator.hasNext();) {
					RuleElementAssociation rule2= iterator.next();
					con.getContainsRuleElementAssociation().add(rule2);
				}
				
				for (Iterator<RuleElementAssociationType> iterator = rulelist3.iterator(); iterator.hasNext();) {
					RuleElementAssociationType rule2= iterator.next();
					con.getContainsRuleElementAssociationType().add(rule2);
				}
				
				for (Iterator<BPMNElement> iterator = bpmnlist1.iterator(); iterator.hasNext();) {
					BPMNElement bpmn2 = iterator.next();
					con.getContainsBPMNElement().add(bpmn2);
				}
				
				for (Iterator<ModelElementType> iterator = bpmntypelist.iterator(); iterator.hasNext();) {
					ModelElementType model1 = iterator.next();
					con.getContainsModelType().add(model1);
				}
				
				for (Iterator<Allocation> iterator = allolist.iterator(); iterator.hasNext();) {
					Allocation allo2 = iterator.next();
					con.getContainsAllocation().add(allo2);
				}
				
				for (Iterator<Situation> iterator = sitList.iterator(); iterator.hasNext();) {
					Situation sit1 = iterator.next();
					con.getContainsSituation().add(sit1);
				}
				
				

				
				
				
				resource1.getContents().add(con);

			
				resource1.save(null);				
		
    }
	
	/**
	 * (jk)
	 * @param bpmn
	 * @param rule
	 * @return
	 * @throws IOException
	 */
	public Boolean checkTypes(BPMNElement bpmn, RuleElement rule) throws IOException {
		if (path1 != null && (new File(path1)).exists()) {
			ResourceSet resourceSet = new ResourceSetImpl();
			resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put("xmi", new XMIResourceFactoryImpl());
			Resource resource =  resourceSet.createResource(URI.createFileURI(path1));
			resource.load(null); // Null are the options, not the file to be loaded!
			Container container = null;
			if (resource.getContents().size() > 0) {
				container = (Container) resource.getContents().get(0);
			}
			if (container != null) {
				ModelElementType modelElementType = null;
				for (ModelElementType actualModelElementType : (List<ModelElementType>) container.getContainsModelType()) {
					if (actualModelElementType.getName().equalsIgnoreCase(bpmn.getType().getName())) {
						modelElementType = actualModelElementType;
					}
				}
				if (modelElementType != null) {
					if (modelElementType.getAssociatsWith().size() > 0) {
						RuleElementType ruleElementType = (RuleElementType) modelElementType.getAssociatsWith().get(0);
						if (ruleElementType.getName().equals(rule.getType().getName())) {
							return true;
						}
					}
				}
			}
		}
		return false;
	}
	
	
	
	public BPMNElement readBPMN() throws IOException{	
		// create resource set and resource 
		ResourceSet resourceSet = new ResourceSetImpl();
				
		// Register XML resource factory
		resourceSet.getResourceFactoryRegistry().getExtensionToFactoryMap().put("xmi", new XMIResourceFactoryImpl());

		Resource resource2 = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("c:/chosenElement.xmi"));
		//Resource resource2 = resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI("/tmp/chosenElement.xmi"));

		BPMNElement bpmn1 = null;
		resource2.load(null);

		if (resource2.getContents().size() > 0) {
			Container con2 = (Container) resource2.getContents().get(0);
			ModelElementType modelType2 = null;
			for (Iterator<ModelElementType> iterator = con2.getContainsModelType().iterator(); iterator.hasNext();) {
				ModelElementType modelType = iterator.next();
				modelType2 = modelType;
			}
			for (Iterator<BPMNElement> iterator = con2.getContainsBPMNElement().iterator(); iterator.hasNext();) {
				BPMNElement bpmn  = iterator.next();
				bpmn1 = bpmn;
			}
			bpmn1.setType(modelType2);
		}		
		return bpmn1;	
	}
}
