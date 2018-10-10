package carisma.regulatory.ruleallocator;

import org.eclipse.jface.viewers.LabelProvider;

import carisma.regulatory.ruleallocator.datamodel.Allocation;

/**
 * {@link LabelProvider} for the allocationList of the {@link RuleAllocatorView}.
 */
public class AllocationListLabelProvider extends LabelProvider {
	@Override
	public String getText(Object element) {
		if (element instanceof Allocation) {
			Allocation allocation = (Allocation) element;
			return "RuleElement '" + allocation.getRuleElement().getName() + "'"
					+ " is allocated to BPMNElement '" + allocation.getBpmnElement().getName() + "'"; 
		}
		if (element instanceof String) {
			return element.toString();
		}
		// If no instance of applies
		return null;
	}
}


