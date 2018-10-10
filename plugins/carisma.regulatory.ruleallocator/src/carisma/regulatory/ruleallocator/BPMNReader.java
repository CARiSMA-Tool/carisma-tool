package carisma.regulatory.ruleallocator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.bpmn2.BaseElement;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.util.EObjectUtil;
import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.DatamodelFactory;
import carisma.regulatory.ruleallocator.datamodel.ModelElementType;
import carisma.modeltype.bpmn2.BPMN2ModelLoader;

/**
 * Original BPMNReader from BA S.Penner.
 * @author spenner
 */
public class BPMNReader {
	
	ArrayList<Object> list = new ArrayList<Object>();
	public DatamodelFactory modelFactory = DatamodelFactory.eINSTANCE;
	
	public String ID;

	static String path;
	
	public void setFile(String path) {
		BPMNReader.path = path; 
	}
	
	/**
	 * Reads a given BPMN file and stores all BPMN elements in a list.
	 * @return A list of new BPMNElemens in the Datamodel of the RuleAllocator
	 * @throws IOException Throws an IOException if the BPMN2ModelLoader fails to load the model
	 */
	public ArrayList<Object> readXMLFile() throws IOException {
		
		ArrayList<Object> elementList = new ArrayList<Object>();
		File bpmnFile = new File(path);
		BPMN2ModelLoader bpmn2ModelLoader = new BPMN2ModelLoader();
		Resource bpmnResource = bpmn2ModelLoader.load(bpmnFile);		
		List<EObject> allElements = getAllReachableBPMNElements(bpmnResource);
		
		// Iterate over all BPMN elements
		for (EObject singleElement : allElements) {
			BPMNElement newDatamodelBPMNElement = modelFactory.createBPMNElement();
			// Set name
			newDatamodelBPMNElement.setName(EObjectUtil.getName(singleElement));
			// Set type
			String typeName = (singleElement).eClass().getName();
			if (typeName.equalsIgnoreCase("participant")) {
				// The original code renamed it in the same way
				typeName = "swimlane";
			}
			ModelElementType modelElementType = modelFactory.createModelElementType();
			modelElementType.setName(typeName);
			newDatamodelBPMNElement.setType(modelElementType);
			// Set id
			newDatamodelBPMNElement.setID(((BaseElement)singleElement).getId());
			// Set incoming
			findIncoming(singleElement, newDatamodelBPMNElement);
			// Set outgoing
			findOutgoing(singleElement, newDatamodelBPMNElement);
			elementList.add(newDatamodelBPMNElement);
		}
		
		return elementList;
	}
	
	/**
	 * Tries to find references to incoming edges.
	 * @param eObject The element, which potentially has incoming edges
	 * @param newDatamodelBPMNElement A new BPMNElement in the Datamodel of the RuleAllocator
	 */
	@SuppressWarnings("unchecked")
	private void findIncoming(EObject eObject, BPMNElement newDatamodelBPMNElement) {
		EStructuralFeature sf = eObject.eClass().getEStructuralFeature("incoming");
		if (sf != null) {
			if (sf.isMany()) {
				EList<EObject> valueList = (EList<EObject>) eObject.eGet(sf);
				for (EObject singleValue : valueList) {
					if (singleValue instanceof BaseElement) {
						BaseElement singleBaseElement = (BaseElement) singleValue;
						newDatamodelBPMNElement.getIncoming().add(singleBaseElement.getId());
					}
				}
			}
		}
	}
	
	/**
	 * Tries to find references to outgoing edges.
	 * @param eObject The element, which potentially has outgoing edges
	 * @param newDatamodelBPMNElement A new BPMNElement in the Datamodel of the RuleAllocator
	 */
	@SuppressWarnings("unchecked")
	private void findOutgoing(EObject eObject, BPMNElement newDatamodelBPMNElement) {
		EStructuralFeature sf = eObject.eClass().getEStructuralFeature("outgoing");
		if (sf != null) {
			if (sf.isMany()) {
				EList<EObject> valueList = (EList<EObject>) eObject.eGet(sf);
				for (EObject singleValue : valueList) {
					if (singleValue instanceof BaseElement) {
						BaseElement singleBaseElement = (BaseElement) singleValue;
						newDatamodelBPMNElement.getIncoming().add(singleBaseElement.getId());
					}
				}
			}
		}
	}
	
	/**
	 * This method returns all BPMN elements of the given resource.
	 * @param resource The resource which contains the BPMN diagram
	 * @return A list of BPMN elements as EObjects
	 */
	public List<EObject> getAllReachableBPMNElements(Resource resource) {
		ArrayList<EObject> list = null;
        if (resource!=null) {
        	list = new ArrayList<EObject>();
        	TreeIterator<EObject> contents = resource.getAllContents();
        	while (contents.hasNext()) {
        		Object element = contents.next();
        		if (element instanceof BaseElement) {
        			list.add((EObject) element);
        		}
        	}
        }
        return list;
	}
}