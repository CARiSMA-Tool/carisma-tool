package carisma.regulatory.ruleallocator;

import java.util.List;
import org.eclipse.jface.viewers.LabelProvider;

import carisma.regulatory.ruleallocator.datamodel.RuleElement;
import carisma.regulatory.ruleallocator.datamodel.Situation;

/**
 * {@link LabelProvider} for the ruleElementList of the {@link RuleAllocatorView}.
 */
public class RuleElementListLabelProvider extends LabelProvider {

	@Override
	public String getText(Object element) {
		if (element instanceof String) {
			return (String) element;
		}
		
		if (element instanceof Situation) {
			Situation situation = (Situation) element;
			List<RuleElement> ruleElementList = situation.getHas();
			String ownedRuleElements = "";
			for (RuleElement ruleElement : ruleElementList) {
				if (!ownedRuleElements.isEmpty()) {
					ownedRuleElements += ", ";
				}
				ownedRuleElements += ruleElement.getName();
			}
			if (ownedRuleElements.isEmpty()) {
				ownedRuleElements = "none";
			}
			return "[Situation] Name: " + situation.getName()
					+ " , has RuleElements: " + ownedRuleElements;  
		}
		
		if (element instanceof RuleElement) {
			RuleElement ruleElement = (RuleElement) element;
			if (ruleElement.getBelongsToSituation().size() > 0) {
				String belongsToSituation = "";
				for (Object situationObject : ruleElement.getBelongsToSituation()) {
					if (situationObject instanceof Situation) {	
						if (!belongsToSituation.isEmpty()) {
							belongsToSituation += ", ";
						}
						Situation situation = (Situation) situationObject;
						belongsToSituation += situation.getName();
					}
				}
				return "[RuleElement] Name: " + ruleElement.getName()
						+ ", Type: " + ruleElement.getType().getName()
						+ ", belongs to Situation: " + belongsToSituation;
			} else {
				return "[RuleElement] Name: " + ruleElement.getName()
						+ ", Type: " + ruleElement.getType().getName()
						+ ", belongs to no Situation";
			}
		}
		
		// If no instance of applies
	    return null;
	}
}






