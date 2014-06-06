package carisma.regulatory.ruleallocator;

import org.eclipse.emf.common.util.EList;

import carisma.regulatory.ruleallocator.datamodel.Allocation;
import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.DatamodelFactory;
import carisma.regulatory.ruleallocator.datamodel.DatamodelPackage;
import carisma.regulatory.ruleallocator.datamodel.ModelElementType;
import carisma.regulatory.ruleallocator.datamodel.RuleElement;
import carisma.regulatory.ruleallocator.datamodel.RuleElementType;

public class AllocationController {
	
	/**
	 * An eInstance of the Datamodel package to make the metamodel available for EMF.
	 */
	@SuppressWarnings("unused")
	private DatamodelPackage datamodelPackage;
	
	/**
	 * eIstance of the DatamodelFactory to create and handle Datamodel related objects.
	 */
	private DatamodelFactory datamodelFactory;
	
	/**
	 * An instance of the DatamodelManager to get the content of the loaded RuleElement file.
	 */
	private DatamodelManager datamodelManager;
	
	/**
	 * Constructor
	 */
	public AllocationController(DatamodelManager datamodelManager) {
		this.datamodelPackage = DatamodelPackage.eINSTANCE;
		this.datamodelFactory = DatamodelFactory.eINSTANCE;
		this.datamodelManager = datamodelManager;
	}
	
	/**
	 * This method creates a allocation between the given RuleElement and BPMNElement if suitable.
	 * @param ruleElement The RuleElement which is intended to be part of the allocation
	 * @param bpmnElement The BPMNElement which is intended to be part of the allocation
	 * @return The created Allocation or null otherwise
	 */
	public Allocation allocate(RuleElement ruleElement, BPMNElement bpmnElement) {
		ModelElementType actualModelElementType = getOrCreateModelElementType(bpmnElement.getType());
		BPMNElement actualBPMNElement = getOrCreateBPMNElement(bpmnElement, actualModelElementType);
		if (!allocationExist(ruleElement, actualBPMNElement)
				&& checkTypes(ruleElement, actualBPMNElement)) {
			Allocation allocation = datamodelFactory.createAllocation();
			allocation.setRuleElement(ruleElement);
			allocation.setBpmnElement(actualBPMNElement);
			Container container = datamodelManager.getContainer();
			@SuppressWarnings("unchecked")
			EList<Allocation> allocationList = container.getContainsAllocation();
			allocationList.add(allocation);
			
			datamodelManager.saveFile();
			return allocation;
		}
		removeBPMNElementIfUnused(
				actualBPMNElement, 
				datamodelManager.getContainer().getContainsAllocation());
		return null;
	}
	
	/**
	 * Removes a given Allocation object from the list if present.
	 * @param allocation The Allocation object which should be removed
	 */
	public void removeAllocation(Allocation allocation) {
		Container container = datamodelManager.getContainer();
		@SuppressWarnings("unchecked")
		EList<Allocation> allocationList = container.getContainsAllocation();
		if (allocationList.contains(allocation)) {
			BPMNElement bpmnElement = allocation.getBpmnElement();
			allocationList.remove(allocation);
			removeBPMNElementIfUnused(bpmnElement, allocationList);
		}
		datamodelManager.saveFile();
	}
	
	/**
	 * Checks, if there is already an allocation between the given RuleElement and BPMNElement.
	 * @param RuleElement The RuleElement object
	 * @param BPMNElement The BPMNElement object
	 * @return A boolean which indicates if an allocation is already present
	 */
	private boolean allocationExist(RuleElement ruleElement, BPMNElement bpmnElement) {
		Container container = datamodelManager.getContainer();
		@SuppressWarnings("unchecked")
		EList<Allocation> allocationList = container.getContainsAllocation();
		for (Allocation allocation : allocationList) {
			if (allocation.getRuleElement().equals(ruleElement) && allocation.getBpmnElement().equals(bpmnElement)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Gets a specific ModelElementType from the container or creates it, when it is not present yet.
	 * @param modelElementType The ModelElementType object
	 * @return The result of the search of a new ModelElementType object
	 */
	private ModelElementType getOrCreateModelElementType(ModelElementType modelElementType) {
		Container container = datamodelManager.getContainer();
		@SuppressWarnings("unchecked")
		EList<ModelElementType> modelElementTypeList = container.getContainsModelType();
		for (ModelElementType searchingModelElementType : modelElementTypeList) {
			if (searchingModelElementType.getName().equalsIgnoreCase(modelElementType.getName())) {
				return searchingModelElementType;
			}
		}
		ModelElementType newModelElementType = datamodelFactory.createModelElementType();
		newModelElementType.setName(modelElementType.getName());
		modelElementTypeList.add(newModelElementType);
		return newModelElementType;
	}
	
	/**
	 * Gets a specific BPMNElement from the container or creates it, when it is not present yet.
	 * @param bpmnElement The BPMNElement object
	 * @param modelElementType The ModelElementType of the new BPMNElement
	 * @return The result of the search of a new BPMNElement object
	 */
	private BPMNElement getOrCreateBPMNElement(BPMNElement bpmnElement, ModelElementType modelElementType) {
		Container container = datamodelManager.getContainer();
		@SuppressWarnings("unchecked")
		EList<BPMNElement> bpmnElementList = container.getContainsBPMNElement();
		for (BPMNElement searchingBPMNElement : bpmnElementList) {
			if (searchingBPMNElement.getName().equalsIgnoreCase(bpmnElement.getName())) {
				return searchingBPMNElement;
			}
		}
		BPMNElement newBPMNElement = datamodelFactory.createBPMNElement();
		newBPMNElement.setName(bpmnElement.getName());
		newBPMNElement.setID(bpmnElement.getID());
		newBPMNElement.setProcessId(bpmnElement.getProcessId());
		newBPMNElement.setType(modelElementType);
		bpmnElementList.add(newBPMNElement);
		return newBPMNElement;
	}
	
	/**
	 * Removes the given BPMNElement if it is not part of an allocation.
	 * @param bpmnElement The BPMNElement which is removed maybe
	 * @param allocationList The list of allocations
	 */
	private void removeBPMNElementIfUnused(BPMNElement bpmnElement, EList<Allocation> allocationList) {
		boolean foundUsage = false;
		for (Allocation allocation : allocationList) {
			if (allocation.getBpmnElement().equals(bpmnElement)) {
				foundUsage = true;
				break;
			}
		}
		if (!foundUsage) {
			Container container = datamodelManager.getContainer();
			container.getContainsBPMNElement().remove(bpmnElement);
		}
	}
	
	/**
	 * Checks if the type of the RuleElement object fits to the type of the BPMNElement object.
	 * @param ruleElement The RuleElement object
	 * @param bpmnElement The BPMNElement object
	 * @return If the types fit, the method returns true, false otherwise
	 */
	protected boolean checkTypes(RuleElement ruleElement, BPMNElement bpmnElement) {
		Container container = datamodelManager.getContainer();
		if (container != null) {
			ModelElementType modelElementType = null;
			for (Object actualModelElementTypeObject : container.getContainsModelType()) {
				if (actualModelElementTypeObject instanceof ModelElementType) {
					ModelElementType actualModelElementType = (ModelElementType) actualModelElementTypeObject;
					if (actualModelElementType.getName().equalsIgnoreCase(bpmnElement.getType().getName())) {
						modelElementType = actualModelElementType;
					}
				}
			}
			if (modelElementType != null) {
				if (modelElementType.getAssociatsWith().size() > 0) {
					RuleElementType ruleElementType = (RuleElementType) modelElementType.getAssociatsWith().get(0);
					if (ruleElementType.getName().equals(ruleElement.getType().getName())) {
						return true;
					}
				}
			}
		}
		return false;
	}
}
