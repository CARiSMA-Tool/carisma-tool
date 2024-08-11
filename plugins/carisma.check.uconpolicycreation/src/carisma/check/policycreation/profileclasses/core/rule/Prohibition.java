package carisma.check.policycreation.profileclasses.core.rule;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.uml2.uml.Element;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileclasses.core.failure.Remedy;

public class Prohibition extends Rule {
	Remedy remedy;


	public Remedy getRemedy() {
		return remedy;
	}

	public void setRemedy(Remedy remedy) {
		this.remedy = remedy;
	}
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		//Currently leads to properties of unrelated duty being taken over (did with references)
		EStructuralFeature remedyFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getProhibition_Remedies().getName());
		if (UMLModelConverter.getValue(currentEObject,odrlPackage.getProhibition_Remedies()) != null) {
			Object attributeValueOdrl = handler.addElement(remedyFeature, this, activityElement);
			System.out.println("attributeValue remedy: " + attributeValueOdrl);
			if (attributeValueOdrl instanceof Remedy newRemedy) {
				this.setRemedy(newRemedy);
			}
		}//TODO only set the remedy if its rules-Property is not empty (in ecore it has the empty list, making the remedy and List non-null in any case). Possibility: fillRemedies
		Object attributeValue = UMLModelConverter.getValue(currentEObject,odrlPackage.getProhibition_Remedies());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<Duty> attributeValueOdrl = handler.addElement(list, this.getRemedy(), activityElement, Duty.class);
			if (attributeValueOdrl!=null) {
				if (this.getRemedy().getRules()==null)
					this.getRemedy().setRules(new LinkedList<>());
				this.getRemedy().getRules().addAll(attributeValueOdrl);//TODO change getters to conditional generators or add null-checks with additional creation everywhere were gotten objects are further used
			}
		}
	}
}
