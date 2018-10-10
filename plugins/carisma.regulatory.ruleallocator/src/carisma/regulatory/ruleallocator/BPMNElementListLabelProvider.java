package carisma.regulatory.ruleallocator;

import org.eclipse.jface.viewers.LabelProvider;

import carisma.regulatory.ruleallocator.datamodel.BPMNElement;

/**
 * {@link LabelProvider} for the bpmnElementList of the {@link RuleAllocatorView}.
 */
public class BPMNElementListLabelProvider extends LabelProvider {

	@Override  
	public String getText(Object element) {
		if (element instanceof BPMNElement) {
			BPMNElement bpmnElement = (BPMNElement) element;
			String returnString = "";
			if (bpmnElement.getType() != null && bpmnElement.getType().getName() != null) {
				if (bpmnElement.getType().getName().equalsIgnoreCase("swimlane")) {
					return "[Swimlane] "
							+ "Name: " + bpmnElement.getName()
							+ ", ID: " + bpmnElement.getID();
				}
				if (bpmnElement.getType().getName().equalsIgnoreCase("process")) {
					return "[Process] "
							+ "ID: " + bpmnElement.getID();
				}
			} else {
				returnString = "[No Type] "
						+ "Name: " + bpmnElement.getName()
						+ ", ID: " + bpmnElement.getID();
				if (bpmnElement.getIncoming().size() > 0) {
					returnString += ", Incoming Edges (IDs): " + bpmnElement.getIncoming();
				}
				if (bpmnElement.getOutgoing().size() > 0) {
					returnString += ", Outgoing Edges (IDs): " + bpmnElement.getOutgoing();
				}
				return returnString;
			}
			returnString = "[" + bpmnElement.getType().getName() + "] ";
			if (!bpmnElement.getName().equals(null) 
					&& !bpmnElement.getName().equals("null") 
					&& !bpmnElement.getName().isEmpty()) { 
				returnString += "Name: " + bpmnElement.getName() + ", ";
			}
			returnString += "ID: " + bpmnElement.getID();
			if (bpmnElement.getIncoming().size() > 0) {
				returnString += ", Incoming Edges (IDs): " + bpmnElement.getIncoming();
			}
			if (bpmnElement.getOutgoing().size() > 0) {
				returnString += ", Outgoing Edges (IDs): " + bpmnElement.getOutgoing();
			}
			return returnString;
		}
	     
		if (element instanceof String) {
			return (String) element;
		}
		
		// If no instance of applies
		return null;
	}
}