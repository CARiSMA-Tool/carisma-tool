package carisma.regulatory.ruleallocator.OLD;



import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;

import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.DatamodelFactory;
import carisma.regulatory.ruleallocator.datamodel.DatamodelPackage;
import carisma.regulatory.ruleallocator.datamodel.ModelElementType;
import carisma.regulatory.ruleallocator.datamodel.RuleElementType;


public class DataLoader {

	
    ResourceSet resourceSet2 = new ResourceSetImpl();
    public DatamodelFactory modelFactory = DatamodelFactory.eINSTANCE;
    static Boolean set = false;
    //TODO: (jk) Fix this awful local path
    static String path = "file:///C:/data1.xmi";
    static Boolean one = false;
    static Boolean two = false;
    static Boolean three = false;
    static Boolean four = false;
    static Boolean five = false;
    static Boolean set2 = false;
//    static String path = "/tmp/dynamicData.xmi";
    
	public void setRecource (String path) {
		DataLoader.path = path;
		set = true;
	}
	

	public DataLoader() {
		
	}


	public synchronized Container load(){
	    // Initialize the model
	    DatamodelPackage.eINSTANCE.eClass();
	    
	   

	    Resource.Factory.Registry reg = Resource.Factory.Registry.INSTANCE;
	    Map<String, Object> m = reg.getExtensionToFactoryMap();
	    m.put("xmi", new XMIResourceFactoryImpl());

	    // Obtain a new resource set
	    ResourceSet resSet = new ResourceSetImpl();

	    // Get the resource
	   
//	    Resource resource3 = resSet.getResource(URI.createURI("file:///" +  path), true);
	    Resource resource3 = resSet.getResource(URI.createURI( path) , true);
//	    Resource resource = resSet.getResource(URI.createURI("/tmp/dynamicData.xmi"), true);
	    Container container;
	    RuleElementType ruletype =  modelFactory.createRuleElementType();
	    RuleElementType ruletype2 =  modelFactory.createRuleElementType();
	    RuleElementType ruletype3 =  modelFactory.createRuleElementType();
	    RuleElementType ruletype4 =  modelFactory.createRuleElementType();
	    RuleElementType ruletype5 =  modelFactory.createRuleElementType();
	    ModelElementType bpmnType = modelFactory.createModelElementType();
	    ModelElementType bpmnType2 = modelFactory.createModelElementType();
	    ModelElementType bpmnType3 = modelFactory.createModelElementType();
	    ModelElementType bpmnType4 = modelFactory.createModelElementType();
	    ModelElementType bpmnType5 = modelFactory.createModelElementType();
	    ModelElementType bpmnType6 = modelFactory.createModelElementType();
	    ModelElementType bpmnType7 = modelFactory.createModelElementType();
	    ModelElementType bpmnType8 = modelFactory.createModelElementType();
	    ModelElementType bpmnType9 = modelFactory.createModelElementType();
	    ModelElementType bpmnType10 = modelFactory.createModelElementType();
	    ModelElementType bpmnType11 = modelFactory.createModelElementType();
	    ModelElementType bpmnType12 = modelFactory.createModelElementType();
	    ModelElementType bpmnType13 = modelFactory.createModelElementType();
	    ModelElementType bpmnType14 = modelFactory.createModelElementType();
	    ModelElementType bpmnType15 = modelFactory.createModelElementType();
	    ModelElementType bpmnType16 = modelFactory.createModelElementType();
	    ModelElementType bpmnType17 = modelFactory.createModelElementType();
	    ModelElementType bpmnType18 = modelFactory.createModelElementType();
	    ModelElementType bpmnType19 = modelFactory.createModelElementType();
	    ModelElementType bpmnType20 = modelFactory.createModelElementType();
	    ModelElementType bpmnType21 = modelFactory.createModelElementType();
	    ModelElementType bpmnType22 = modelFactory.createModelElementType();
	    ModelElementType bpmnType23 = modelFactory.createModelElementType();
	    ModelElementType bpmnType24 = modelFactory.createModelElementType();
	    ModelElementType bpmnType25 = modelFactory.createModelElementType();
	    ModelElementType bpmnType26 = modelFactory.createModelElementType();
	    ModelElementType bpmnType27 = modelFactory.createModelElementType();
		   
	   
	    
//	    if (set == false) {
//	    container = (Container) resource.getContents().get(0);
//	    }
//	    else {
	    	container = (Container) resource3.getContents().get(0);
//	    }
	    
	    for (Iterator<RuleElementType> iterator = container.getContainsRuleElementType().iterator(); iterator.hasNext();) {
			  
		      RuleElementType rule2 = iterator.next();
		      if (rule2.getName().equals("Process")) {
		    	  one = true;
		    	  ruletype = rule2;
		      }
		      
		}
	    
	    for (Iterator<RuleElementType> iterator = container.getContainsRuleElementType().iterator(); iterator.hasNext();) {
			  
		      RuleElementType rule2 = iterator.next();
		      if (rule2.getName().equals("Activity")) {
		    	  two = true;
		    	  ruletype2 = rule2;
		      }
		      
		}
	    
	    if (two.equals(false)) {
	    	ruletype2.setName("Activity");
	    	container.getContainsRuleElementType().add(ruletype2);
	    }
	    
	    for (Iterator<RuleElementType> iterator = container.getContainsRuleElementType().iterator(); iterator.hasNext();) {
			  
		      RuleElementType rule2 = iterator.next();
		      if (rule2.getName().equals("Property")) {
		    	  three = true;
		    	  ruletype3 = rule2;
		      }
		      
		}
	    
	    if (three.equals(false)) {
	    	ruletype3.setName("Property");
	    	container.getContainsRuleElementType().add(ruletype3);
	    }
	    
	    for (Iterator<RuleElementType> iterator = container.getContainsRuleElementType().iterator(); iterator.hasNext();) {
			  
		      RuleElementType rule2 = iterator.next();
		      if (rule2.getName().equals("Object")) {
		    	  four = true;
		    	  ruletype4 = rule2;
		      }
		      
		}
	    
	    if (four.equals(false)) {
	    	ruletype4.setName("Object");
	    	container.getContainsRuleElementType().add(ruletype4);
	    }
	    
	    for (Iterator<RuleElementType> iterator = container.getContainsRuleElementType().iterator(); iterator.hasNext();) {
			  
		      RuleElementType rule2 = iterator.next();
		      if (rule2.getName().equals("Role")) {
		    	  five = true;
		    	  ruletype5 = rule2;
		      }
		      
		}
	    
	    if (five.equals(false)) {
	    	ruletype5.setName("Role");
	    	container.getContainsRuleElementType().add(ruletype5);
	    }
	    
	    for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("task")) {
		    	 set2 = true;
		      
		}
	    }
	    
		      if (set2.equals(false)) {
		    	  
	    bpmnType.setName("task");
	    bpmnType.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType);
		}
		set2 = false;      
		 
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("callActivity")) {
		    	 set2 = true;
		      
		}
	    }
		
		 if (set2.equals(false)) {
	    bpmnType2.setName("callActivity");
	    bpmnType2.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType2);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("taskSend")) {
		    	 set2 = true;
		      
		}
	    }
		
		if (set2.equals(false)) {
	    bpmnType3.setName("taskSend");
	    bpmnType3.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType3);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("taskReceive")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType4.setName("taskReceive");
	    bpmnType4.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType4);
		}
		set2 = false;
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("taskService")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType5.setName("taskService");
	    bpmnType5.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType5);
		}
		set2 = false;
	    
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("taskUser")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType6.setName("taskUser");
	    bpmnType6.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType6);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("taskManual")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType7.setName("taskManual");
	    bpmnType7.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType7);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("taskScript")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType8.setName("taskScript");
	    bpmnType8.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType8);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("taskBusinessRule")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType9.setName("taskBusinessRule");
	    bpmnType9.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType9);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("callUser")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType10.setName("callUser");
	    bpmnType10.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType10);
		}
		set2 = false;
	    
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("callManual")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType11.setName("callManual");
	    bpmnType11.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType11);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("taskBusinessRule")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType12.setName("taskBusinessRule");
	    bpmnType12.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType12);
		}
		set2=false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("callManual")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType13.setName("callManual");
	    bpmnType13.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType13);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("callBusinessRule")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType14.setName("callBusinessRule");
	    bpmnType14.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType14);
		}
		set2 =false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("callSubprocess")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType15.setName("callSubprocess");
	    bpmnType15.getAssociatsWith().add(ruletype2);
	    container.getContainsModelType().add(bpmnType15);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("annotation")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType16.setName("annotation");
	    bpmnType16.getAssociatsWith().add(ruletype3);
	    container.getContainsModelType().add(bpmnType16);
		}
		set2=false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("DataInput")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType17.setName("DataInput");
	    bpmnType17.getAssociatsWith().add(ruletype4);
	    container.getContainsModelType().add(bpmnType17);
		}
		set2=false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("DataOutput")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType18.setName("DataOutput");
	    bpmnType18.getAssociatsWith().add(ruletype4);
	    container.getContainsModelType().add(bpmnType18);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("dataobject")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType19.setName("dataobject");
	    bpmnType19.getAssociatsWith().add(ruletype4);
	    container.getContainsModelType().add(bpmnType19);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("dataobjects")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType20.setName("dataobjects");
	    bpmnType20.getAssociatsWith().add(ruletype4);
	    container.getContainsModelType().add(bpmnType20);
		}
		set2 = false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("datainputs")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType21.setName("datainputs");
	    bpmnType21.getAssociatsWith().add(ruletype4);
	    container.getContainsModelType().add(bpmnType21);
		}
		set2=false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("dataoutputs")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType22.setName("dataoutputs");
	    bpmnType22.getAssociatsWith().add(ruletype4);
	    container.getContainsModelType().add(bpmnType22);
		}
		set2=false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("datastore")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType23.setName("datastore");
	    bpmnType23.getAssociatsWith().add(ruletype4);
	    container.getContainsModelType().add(bpmnType23);
		}
		set2=false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("initiatingMessage")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType24.setName("initiatingMessage");
	    bpmnType24.getAssociatsWith().add(ruletype4);
	    container.getContainsModelType().add(bpmnType24);
		}
		set2=false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("nonInitiatingMessage")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType25.setName("nonInitiatingMessage");
	    bpmnType25.getAssociatsWith().add(ruletype4);
	    container.getContainsModelType().add(bpmnType25);
		}
		set2=false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("swimlane")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType26.setName("swimlane");
	    bpmnType26.getAssociatsWith().add(ruletype5);
	    container.getContainsModelType().add(bpmnType26);
		}
		set2=false;
		
		for (Iterator<ModelElementType> iterator = container.getContainsModelType().iterator(); iterator.hasNext();) {
			  
		      ModelElementType rule2 = iterator.next();
		      if (rule2.getName().equals("group")) {
		    	 set2 = true;
		      
		}
	    }
		if (set2.equals(false)) {
	    bpmnType27.setName("group");
	    bpmnType27.getAssociatsWith().add(ruletype5);
	    container.getContainsModelType().add(bpmnType27);
		} 
		set2 = false;
	    resource3.getContents().add(container);

		// serialize resource – you can specify also serialization
		// options which defined on org.eclipse.emf.ecore.xmi.XMIResource
		try {
			resource3.save(null);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	    
		try {
			resource3.load(null);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		container = (Container) resource3.getContents().get(0);
	    
	    return container;
	    
	  }

	public synchronized Container load2(){
		DatamodelPackage.eINSTANCE.eClass();
	   
	    Resource.Factory.Registry reg = Resource.Factory.Registry.INSTANCE;
	    Map<String, Object> m = reg.getExtensionToFactoryMap();
	    m.put("xmi", new XMIResourceFactoryImpl());

	    // Obtain a new resource set
	    ResourceSet resSet = new ResourceSetImpl();

	    // Get the resource
//	    Resource resource = resSet.getResource(URI.createURI("/tmp/dynamicData.xmi"), true);
//	    Resource resource3 = resSet.getResource(URI.createURI("file:///" +  path), true);
	    Resource resource3 = resSet.getResource(URI.createURI(path) , true);
	    Container container;
	    container = (Container) resource3.getContents().get(0);
	    
	    return container;
	}
	
	
	
}
