package carisma.check.policycreation.profileimpl.core.asset;

import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.constraint.ConstraintInterfaceImpl;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;

public class AssetCollectionImpl extends AssetImpl {
	ConstraintInterfaceImpl refinement;
	String source;

	
	
	public ConstraintInterfaceImpl getRefinement() {
		return refinement;
	}
	public void setRefinement(ConstraintInterfaceImpl refinement) {
		this.refinement = refinement;
	}
	public String getSource() {
		return source;
	}
	public void setSource(String source) {
		this.source = source;
	}
	
	@Override
	public void fill(EObject currentEObject, EObject activityElement, UMLModelConverter handler) {
		super.fill(currentEObject, activityElement, handler);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getAssetCollection_Source());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {
			
			this.setSource(stringValue);
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getRefinableElement_Refinement()); 
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, activityElement);
			if (attributeValueOdrl instanceof ConstraintInterfaceImpl refinement) {
				this.setRefinement(refinement);
			}
		}
	}
	
	@Override
	public Object fillMapIndividual(Map<String, Object> map, Set<ODRLClassImpl> circlePreventionSet)
			throws NoSuchFieldException, SecurityException {
		map.put(getTypeKeyword(), gatClassTerm());
		return null;
	}
	
}
