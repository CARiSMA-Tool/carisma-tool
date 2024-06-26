package carisma.check.policycreation.profileimpl.core;

import java.util.Set;

import org.eclipse.emf.ecore.ENamedElement;
import org.eclipse.emf.ecore.EObject;

import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import carisma.check.policycreation.UMLModelConverter;

public abstract class ODRLClassImpl{
	protected Set<ODRLClassImpl> referredBy;
	public ENamedElement containingUMLElement;
	public ODRLClassImpl directParent;//maybe

	protected ODRLCommonVocabularyPackage odrlPackage = UMLModelConverter.odrlPackage;
	
	public void fill(EObject input, EObject activityElement, UMLModelConverter handler) {
	}

	

	public String getType() {//for the automatic JSON-Conversion currently used for testing
		return this.getClass().getSimpleName();
	}


	
	
}
