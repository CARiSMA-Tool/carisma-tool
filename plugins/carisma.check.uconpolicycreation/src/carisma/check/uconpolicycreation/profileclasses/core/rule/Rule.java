package carisma.check.uconpolicycreation.profileclasses.core.rule;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.InputPin;
import org.eclipse.uml2.uml.OutputPin;

import carisma.check.uconpolicycreation.UMLModelConverter;
import carisma.check.uconpolicycreation.profileclasses.ODRLClass;
import carisma.check.uconpolicycreation.profileclasses.common.relation.Output;
import carisma.check.uconpolicycreation.profileclasses.core.action.Action;
import carisma.check.uconpolicycreation.profileclasses.core.asset.Asset;
import carisma.check.uconpolicycreation.profileclasses.core.constraints.ConstraintInterface;
import carisma.check.uconpolicycreation.profileclasses.core.function.Function;
import carisma.check.uconpolicycreation.profileclasses.core.relation.Relation;
import carisma.check.uconpolicycreation.profileclasses.core.relation.Target;

public abstract class Rule extends ODRLClass {
	String uid;
	List<Function> involvedParties = new LinkedList<>();
	List<Relation> involvedAssets = new LinkedList<>();
	Action action;
	ConstraintInterface constraint;
	
	public String getUid() {
		return uid;
	}
	public void setUid(String uid) {
		this.uid = uid;
	}
	
	public List<Function> getInvolvedParties() {
		return involvedParties;
	}
	public void setInvolvedParties(List<Function> involvedParties) {
		this.involvedParties = involvedParties;
	}
	public void addInvolvedParties(Function involvedParty) {
		this.involvedParties.add(involvedParty);
	}
	
	public List<Relation> getInvolvedAssets() {
		return involvedAssets;
	}
	public void setInvolvedAssets(List<Relation> involvedAssets) {
		this.involvedAssets = involvedAssets;
	}
	public void addInvolvedAssets(Relation involvedAsset) {
		this.involvedAssets.add(involvedAsset);
	}
	
	public Action getAction() {
		return action;
	}
	public void setAction(Action action) {
		this.action = action;
	}
	public ConstraintInterface getConstraint() {
		return constraint;
	}
	public void setConstraint(ConstraintInterface constraint) {
		this.constraint = constraint;
	}
	
	
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getRule_Action());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, containingUmlElement);
			if (attributeValueOdrl instanceof Action action) {
				this.setAction(action);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getRefinableElement_Refinement());
		if (attributeValue instanceof EObject newEObj) {//TODO get constraint
			Object attributeValueOdrl = handler.addElement(newEObj, this.getAction(), containingUmlElement);
			if (attributeValueOdrl instanceof ConstraintInterface constraintInterface) {
				//if (attributeValueOdrl instanceof List constraintList) {TODO add seperate cases for logicalConstraint and List of constraints (in the 2nd case possibly also add instead of set)
				//	rule.getConstraint().
				//}
				if (this.getAction()!=null) {//TODO also add null check for other cases where a gotten object is further used or keep the nullpointer as sign that something is missing
					this.getAction().setRefinement(constraintInterface);
				}
			}
			
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getRule_Uid());
		if (attributeValue instanceof String string) {
			this.setUid(string);
		}
//		InvolvedAssets removed as attribute from UML-Model. Only defined through adjacent pins.
//		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getRule_InvolvedAssets());
//		if (attributeValue instanceof List list) { //TODO List attribute
//			List<RelationImpl> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, RelationImpl.class);
//			if (attributeValueOdrl!=null) {
//				this.setInvolvedAssets(attributeValueOdrl);
//			}
//		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getRule_InvolvedParties());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<Function> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, Function.class);
			if (attributeValueOdrl!=null) {
				this.setInvolvedParties(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstrainableElement_Constraint());
		if (attributeValue instanceof EObject newEObj) {//TODO get constraint
			Object attributeValueOdrl = handler.addElement(newEObj, this, containingUmlElement);
			if (attributeValueOdrl instanceof ConstraintInterface constraintInterface) {
				//if (attributeValueOdrl instanceof List constraintList) {TODO maybe add seperate cases for logicalConstraint and List of constraints (in the 2nd case possibly also add instead of set)
				//	rule.getConstraint().
				//}
				this.setConstraint(constraintInterface);
			}
		}
		//Activity diagram: Get related Assets from neighboring pins (TODO: clear up conflicts with explicitly listed Relations?)
		if (containingUmlElement instanceof org.eclipse.uml2.uml.Action action) {
			for (InputPin inPin : action.getInputs()) {
				Object targets = handler.addElement(inPin, this, inPin);
				if (targets instanceof List<?> targetsList) {
					for (Object singleTarget : targetsList) {
						if (singleTarget instanceof Relation targetRelation) {
							this.addInvolvedAssets(targetRelation);
						}
					}
				}
//				for (EObject stereoAppl : inPin.getStereotypeApplications()) {
//					if (handler.addElement(stereoAppl, this, containingUmlElement) instanceof Asset asset) {
//						Relation newTarget = new Target();
//						newTarget.setHandler(handler);//TODO watch out: not all classes are created in the Converter, remove if handler passing is changed to constructor
//						handler.addToHandledOdrlObjects(newTarget);
//						newTarget.setAsset(asset);
//						asset.addReferredBy(newTarget);
//						this.addInvolvedAssets(newTarget);
//						newTarget.addReferredBy(this);
//					}
//				}
			}
			for (OutputPin outPin : action.getOutputs()) {
				for (EObject stereoAppl : outPin.getStereotypeApplications()) {
					Object outputs = handler.addElement(outPin, this, outPin);
					if (outputs instanceof List<?> outputsList) {
						for (Object singleOutput : outputsList) {
							if (singleOutput instanceof Relation outputRelation) {
								this.addInvolvedAssets(outputRelation);
							}
						}
					}
//					if (handler.addElement(stereoAppl, this, containingUmlElement) instanceof Asset asset) {
//						Relation newOutput = new Output();
//						newOutput.setHandler(handler);//TODO watch out: not all classes are created in the Converter, remove if handler passing is changed to constructor
//						handler.addToHandledOdrlObjects(newOutput);
//						newOutput.setAsset(asset);
//						asset.addReferredBy(newOutput);
//						this.addInvolvedAssets(newOutput);
//						newOutput.addReferredBy(this);
//					}
				}
			}
		}
	}
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		for (Function function : involvedParties) {
			Object functionMapObject = handler.createMap(function, circlePreventionSet);
			if (functionMapObject != null) {
				map.put(function.gatClassTerm(), functionMapObject);//TODO: check for duplicates (not here, in the fill()-method (or in the validity checks later))
			}
		}
		for (Relation relation : involvedAssets) {
			Object relationMapObject = handler.createMap(relation, circlePreventionSet);
			if (relationMapObject != null) {
				map.put(relation.gatClassTerm(), relationMapObject);//TODO: check for duplicates (not here, in the fill()-method (or in the validity checks later))
			}
		}
		return null;
	}
}
