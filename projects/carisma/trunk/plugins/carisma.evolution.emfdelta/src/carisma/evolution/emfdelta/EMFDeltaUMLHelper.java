package carisma.evolution.emfdelta;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;

import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.ChangeConstraint;
import carisma.evolution.ConstraintType;
import carisma.evolution.DelElement;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;

/**
 * 
 * @author Johannes Kowald
 *
 */
public final class EMFDeltaUMLHelper {
	
	//########################################################################################
	/**
	 * Converts the given EObject element to a Stereotype element if possible and suitable.
	 * @param element The given EObject object
	 * @return The given EObject object or, if possible and suitable, a Stereotype object
	 */
	protected static EObject convertToStereotypeIfSuitable(EObject element) {
		int analysis_mode = EMFDeltaHelper.getAnalysisMode();
		if (analysis_mode == EMFDeltaHelper.ANALYSIS_MODE_UML && element != null) {
			org.eclipse.uml2.uml.Stereotype stereotype = org.eclipse.uml2.uml.util.UMLUtil.getStereotype(element);
			if (stereotype != null) {
				return stereotype;
			}
			return element;
		}
		return element;
	}
	
	//########################################################################################
	/**
	 * Converts the given EObject element to a StereotypeApplication element if possible and suitable.
	 * @param element The given EObject object
	 * @param parent The parent UML element of the given object
	 * @return The given EObject object or, if possible and suitable, a StereotypeApplication object
	 */
	protected static EObject convertToStereotypeApplicationIfSuitable(EObject element) {
		int analysis_mode = EMFDeltaHelper.getAnalysisMode();
		if (analysis_mode == EMFDeltaHelper.ANALYSIS_MODE_UML && element != null) {
			EObject parent = findStereotypeParent(element);
			if (parent != null) {
				org.eclipse.uml2.uml.Stereotype stereotype = org.eclipse.uml2.uml.util.UMLUtil.getStereotype(element);
				if (stereotype != null) {
					return new carisma.modeltype.uml2.StereotypeApplication(stereotype, (org.eclipse.uml2.uml.Element) parent);
				}
				return element;
			}
		}
		return element;
	}
	
	//########################################################################################
	/**
	 * Returns true whether the element is a Stereotype or false whether the model is not a 
	 * UML model or the element is not a Stereotype.
	 * @param element The element which is maybe a Stereotype
	 * @return A boolean
	 */
	protected static boolean isStereotype(EObject element) {
		int analysis_mode = EMFDeltaHelper.getAnalysisMode();
		if (analysis_mode == EMFDeltaHelper.ANALYSIS_MODE_UML) {
			return (element instanceof org.eclipse.uml2.uml.Stereotype);
		}
		return false;
	}
	
	//########################################################################################
	/**
	 * Returns true whether the element is a StereotypeApplication or false whether the model 
	 * is not a UML model or the element is not a StereotypeApplication.
	 * @param element The element which is maybe a Stereotype
	 * @return A boolean
	 */
	protected static boolean isStereotypeApplication(EObject element) {
		int analysis_mode = EMFDeltaHelper.getAnalysisMode();
		if (analysis_mode == EMFDeltaHelper.ANALYSIS_MODE_UML) {
			return (element instanceof carisma.modeltype.uml2.StereotypeApplication);
		}
		return false;
	}
	
	//########################################################################################
	/**
	 * Tries to find the target value of the given Stereotype object (only UML analysis mode).
	 * @param element The Stereotype object as an EObject of the changed model
	 * @return The target value as an EObject
	 */
	protected static EObject findAddedStereotypeTarget(EObject element) {	
		if (EMFDeltaHelper.getAnalysisMode() == EMFDeltaHelper.ANALYSIS_MODE_UML && element != null) {
			EClass elementEClass = element.eClass();
			EObject returnValue = null;
			for (EReference eReference : elementEClass.getEAllReferences()) {
				if (!eReference.isDerived()) { 
					if (eReference.getName().startsWith("base_")
							&& element.eGet(eReference) != null) {
						try {
							returnValue = EMFDeltaHelper.getReferenceModelElement((EObject) element.eGet(eReference));
						} catch (MatchElementsUnsetException meue) {
							meue.printStackTrace();
							return null;
						}
					}
				}
				if (returnValue != null) {
					return returnValue;
				}
			}
		}
		return null;
	}
	
	//########################################################################################
	/**
	 * Tries to find the parent UML element of the given Stereotype element if possible
	 * (only UML analysis mode).
	 * @param element The given element as EObject
	 * @return The parent UML element of the given element or null if no parent exist
	 */
	protected static EObject findStereotypeParent(EObject element) {	
		if (EMFDeltaHelper.getAnalysisMode() == EMFDeltaHelper.ANALYSIS_MODE_UML && element != null) {
			EClass elementEClass = element.eClass();
			EObject returnValue = null;
			for (EReference eReference : elementEClass.getEAllReferences()) {
				if (!eReference.isDerived()) { 
					if (eReference.getName().startsWith("base_")
							&& element.eGet(eReference) != null) {
						returnValue = (EObject) element.eGet(eReference);
					}
				}
				if (returnValue != null) {
					return returnValue;
				}
			}
		}
		return null;
	}
	
	//########################################################################################
	/**
	 * Searches for TaggedValues corresponding either to the given Stereotype or to the Stereotypes 
	 * applied to the parent UmlElement (only UML analysis mode).
	 * @param stereoEObject The given Stereotype object
	 * @param stereoChange The Change object which contains the AddElement of the given Stereotype object
	 * @param parentUmlElement The UmlElement which is the parent to the given Stereotype object
	 * @param refString The reference string of the AddElement of the given Stereotype object
	 */
	protected static void checkForAddedTaggedValues(
			final EObject stereoEObject, 
			final Change stereoChange, 
			final EObject parentUmlElement, 
			final String refString, 
			final List<Change> deltaList) {
		if (EMFDeltaHelper.getAnalysisMode() == EMFDeltaHelper.ANALYSIS_MODE_UML) {	
			if (stereoEObject instanceof org.eclipse.uml2.uml.Stereotype) {
				org.eclipse.uml2.uml.Stereotype stereotype = (org.eclipse.uml2.uml.Stereotype) stereoEObject;
				carisma.modeltype.uml2.StereotypeApplication stereotypeApp = new carisma.modeltype.uml2.StereotypeApplication(
								stereotype, 
								(org.eclipse.uml2.uml.Element) parentUmlElement);
				AddElement stereoAddElement = (AddElement) stereoChange.getAlternatives().get(0).getDeltaElements().get(0);
				int refIDtv = 0;
				for (carisma.modeltype.uml2.TaggedValue taggedValue : stereotypeApp.getTaggedValues()) {
					AddElement 	newParent				= stereoAddElement;
					EClass 		newMetaClass			= taggedValue.getTag().eClass();
					AddElement 	addTaggedValue 			= new AddElement(null, newMetaClass, newParent);
					addTaggedValue.addKeyValuePair("name", taggedValue.getName());
					addTaggedValue.addKeyValuePair("value", taggedValue.getValue());
					stereoAddElement.addContainedElement(addTaggedValue);
					Change newChange = new Change(refString + "-tv" + Integer.toString(refIDtv));
					newChange.addConstraint(new ChangeConstraint(ConstraintType.REQ, stereoChange, newChange));
					Alternative newAlternative = new Alternative();
					newChange.addAlternative(newAlternative);
					deltaList.add(newChange);
					newAlternative.addDeltaElement(addTaggedValue);
					refIDtv++;
				}
			}
		}
	}
	
	//########################################################################################
	/**
	 * Searches for Stereotypes applied to the given AddElement and, if successful, proceeds with 
	 * a search for corresponding TaggedValues (only UML analysis mode).
	 * @param newAddElement The AddElement which is filled with the found informations
	 * @param elementEObject The element as an EObject which will be analyzed
	 * @param refID A string which contains the refID for the new Change object
	 */
	protected static void checkForAddedStereotypes(
			final AddElement newAddElement, 
			final EObject elementEObject, 
			final Change requiredChange,
			final List<Change> deltaList,
			final AnalyzeAddedObject callingAnalyzer) {
		if (EMFDeltaHelper.getAnalysisMode() == EMFDeltaHelper.ANALYSIS_MODE_UML) {
			if (elementEObject instanceof org.eclipse.uml2.uml.Element) {
				org.eclipse.uml2.uml.Element umlElement = (org.eclipse.uml2.uml.Element) elementEObject;
				AddElement stereoAddElement = null;
				Change stereoChange = null;
				int refIDstereo = 0;
				for (org.eclipse.uml2.uml.Stereotype stereotype : umlElement.getAppliedStereotypes()) {
					String stereoRefString = requiredChange.getRef() + "-s" + Integer.toString(refIDstereo);
					stereoChange = callingAnalyzer.analyzeEObjectAsChange(stereotype, newAddElement, stereoRefString, requiredChange);
					stereoAddElement = (AddElement) stereoChange.getAlternatives().get(0).getDeltaElements().get(0);
					newAddElement.addContainedElement(stereoAddElement);
					refIDstereo++;
					EMFDeltaUMLHelper.checkForAddedTaggedValues(stereotype, stereoChange, umlElement, stereoRefString, deltaList);
				}
			}
		}
	}
	
	//########################################################################################
	/**
	 * 
	 */
	protected static void checkForQualifiedName(
			final EObject element, 
			final AddElement newAddElement) {
		if (EMFDeltaHelper.getAnalysisMode() == EMFDeltaHelper.ANALYSIS_MODE_UML) {
			if (element instanceof org.eclipse.uml2.uml.NamedElement) {
				String qualifiedName = ((org.eclipse.uml2.uml.NamedElement)element).getQualifiedName();
				newAddElement.addKeyValuePair("qualifiedName", qualifiedName);
			}
		}
	}
	
	//########################################################################################
	/**
	 * Adds all tagged values as accompanying deletions to the corresponding stereotype, 
	 * which is a DelElement in the given change (only UML analysis mode).
	 * @param change A change object which holds the stereotype DelElement.  
	 * @param taggedValueObjects A list of tagged values. 
	 */
	protected static void addTaggedValuesAsAccompayingDeletionsToStereotype(
			final Change change, 
			final List<EObject> taggedValueObjects) {
		// Don't have to check for UML analysis mode because the calling code has checked this already
		for (DeltaElement de : change.getAlternatives().get(0).getDeltaElements()) {
			if (de instanceof DelElement && de.getTarget() != null) {
				DelElement delElement = (DelElement) de;
				delElement.addDeletions(taggedValueObjects);
			}
		}
	}
	
	//########################################################################################
	/**
	 * Checks the current element for applied stereotypes (only UML analysis mode).
	 * @param element - the element which will be analyzed
	 * @param refID - a string which contains the refID for the new Change Object
	 * @param newDelElement - the actual DeltaModel DelElement
	 * @return List<ChangeConstraint> - the List of constraints for the parent Element
	 */
	protected static List<ChangeConstraint> checkForDeletedStereotypes(
			final EObject element, 
			final String refID, 
			final DelElement newDelElement, 
			final Change newChange,
			final AnalyzeDeletedObject callingAnalyzer) {
		// Don't have to check for UML analysis mode because the calling code has checked this already
		ArrayList<ChangeConstraint> returnConstraintList = new ArrayList<ChangeConstraint>();
		if (element instanceof org.eclipse.uml2.uml.Element) {
			org.eclipse.uml2.uml.Element umlElement = (org.eclipse.uml2.uml.Element) element;
			
			List<EObject> accompanyingDeletions = new ArrayList<EObject>();
			int refIDsub = 0;
			// Iterate over all stereotypeApplications
			List<carisma.modeltype.uml2.StereotypeApplication> stereotypeApplications = carisma.modeltype.uml2.UMLHelper.getStereotypeApplications(umlElement);
			for (carisma.modeltype.uml2.StereotypeApplication stereotypeApplication : stereotypeApplications) {
				List<ChangeConstraint> taggedValueChangeConstraints = 
						checkForDeletedTaggedValues(
								stereotypeApplication, 
								accompanyingDeletions, 
								newChange, 
								callingAnalyzer);
				returnConstraintList.addAll(taggedValueChangeConstraints);
				
				Change stereotypeChange = callingAnalyzer.analyzeEObject(stereotypeApplication, newChange.getRef() + "-s" + Integer.toString(refIDsub++), null);
				stereotypeChange.addConstraints(returnConstraintList);
				addTaggedValuesAsAccompayingDeletionsToStereotype(stereotypeChange, accompanyingDeletions);
				callingAnalyzer.addAllInnerDeletions(accompanyingDeletions, stereotypeChange);
				ChangeConstraint newChangeConstraintStereotype = new ChangeConstraint(ConstraintType.REQ, stereotypeChange, newChange);
				returnConstraintList.add(newChangeConstraintStereotype);
				newDelElement.addDeletions(accompanyingDeletions);
			}
		}
		return returnConstraintList;
	}
	
	//########################################################################################
	/**
	 * Checks the given StereotypeApplication for corresponding TaggedValues (only UML analysis mode).
	 * @param stereotypeApplication Given StereotypeApplication object.
	 * @param accompanyingDeletions A list to put the found TaggedValues as accompanying deletions
	 * @param newChange The Change object which contains the StereotypeApplication as a deletion
	 * @return A list of ChangeConstraints
	 */
	protected static List<ChangeConstraint> checkForDeletedTaggedValues(
			final carisma.modeltype.uml2.StereotypeApplication stereotypeApplication, 
			final List<EObject> accompanyingDeletions, 
			final Change newChange,
			final AnalyzeDeletedObject callingAnalyzer) {
		// Don't have to check for UML analysis mode because the calling code has checked this already
		ArrayList<ChangeConstraint> returnConstraintList = new ArrayList<ChangeConstraint>();
		int refIDsub = 0;
		List<carisma.modeltype.uml2.TaggedValue> taggedValues = stereotypeApplication.getTaggedValues();
		Change taggedValueChange;
		for (carisma.modeltype.uml2.TaggedValue tagValue : taggedValues) {
			// Analyze the tagged values
			taggedValueChange = callingAnalyzer.analyzeEObject(tagValue, newChange.getRef() + "-tv" + Integer.toString(refIDsub++), null);
			callingAnalyzer.addAllInnerDeletions(accompanyingDeletions, taggedValueChange);
			ChangeConstraint newChangeConstraintTaggedValue = new ChangeConstraint(ConstraintType.REQ, taggedValueChange, newChange);
			returnConstraintList.add(newChangeConstraintTaggedValue);
		}
		return returnConstraintList;
	}
	
	//########################################################################################
	/**
	 * Compares the owned TaggedValues to find differences (only UML analysis mode).
	 * @param newEditElement The actual EditElement which holds the differences
	 * @param chaElement The element of the changed model 
	 * @param refElement The element of the reference model
	 */
	protected static void analyzeEditedTaggedValues(
			final EditElement newEditElement, 
			final EObject refElement,
			final EObject chaElement) {
		// Don't have to check for UML analysis mode because the calling code has checked this already 
		EObject newRefElement = convertToStereotypeIfSuitable(refElement);
		EObject newChaElement = convertToStereotypeIfSuitable(chaElement);
		if ((newRefElement instanceof org.eclipse.uml2.uml.Stereotype) 
				&& (newChaElement instanceof org.eclipse.uml2.uml.Stereotype)) {
			EObject refParent = findStereotypeParent(refElement);
			EObject chaParent = findStereotypeParent(chaElement);
			carisma.modeltype.uml2.StereotypeApplication refStereotypeApp = 
					new carisma.modeltype.uml2.StereotypeApplication(
							(org.eclipse.uml2.uml.Stereotype) newRefElement, 
							(org.eclipse.uml2.uml.Element) refParent
							);
			carisma.modeltype.uml2.StereotypeApplication chaStereotypeApp = 
					new carisma.modeltype.uml2.StereotypeApplication(
							(org.eclipse.uml2.uml.Stereotype) newChaElement, 
							(org.eclipse.uml2.uml.Element) chaParent
							);
			for (carisma.modeltype.uml2.TaggedValue refTagValue : refStereotypeApp.getTaggedValues()) {
				carisma.modeltype.uml2.TaggedValue chaTagValue = chaStereotypeApp.getTaggedValue(refTagValue.getName());
				if (chaTagValue != null) {
					if (!refTagValue.getValue().equals(chaTagValue.getValue())) {
						newEditElement.addKeyValuePair(refTagValue.getName(), chaTagValue.getValue());
					}
				}
			}
		}
	}
}
