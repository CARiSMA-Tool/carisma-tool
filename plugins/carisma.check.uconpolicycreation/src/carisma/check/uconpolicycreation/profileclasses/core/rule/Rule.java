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
	/**
	 * Reference to an external rule.
	 */
	String uid;
	/**
	 * Involvement of {@link Party}s with this Object.
	 */
	List<Function> involvedParties = new LinkedList<>();
	/**
	 * Involvement of {@link Asset}s with this Object.
	 */
	List<Relation> involvedAssets = new LinkedList<>();
	/**
	 * The {@link Action} this rule applies to.
	 */
	Action action;
	/**
	 * Constrains the application of this rule.
	 */
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
			if (attributeValueOdrl instanceof Action actionLocal) {
				this.setAction(actionLocal);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getRefinableElement_Refinement());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this.getAction(), containingUmlElement);
			if (attributeValueOdrl instanceof ConstraintInterface constraintInterface) {
				if (this.getAction()!=null) {//TODO also add null check for other cases where a gotten object is further used or keep the nullpointer-exception as sign that something is missing
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
//		if (attributeValue instanceof List list) {
//			List<RelationImpl> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, RelationImpl.class);
//			if (attributeValueOdrl!=null) {
//				this.setInvolvedAssets(attributeValueOdrl);
//			}
//		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getRule_InvolvedParties());
		if (attributeValue instanceof List<?> list) {
			List<Function> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, Function.class);
			if (attributeValueOdrl!=null) {
				this.setInvolvedParties(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstrainableElement_Constraint());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, containingUmlElement);
			if (attributeValueOdrl instanceof ConstraintInterface constraintInterface) {
				this.setConstraint(constraintInterface);
			}
		}
		//Activity diagram: Get related Assets from neighboring pins
		if (containingUmlElement instanceof org.eclipse.uml2.uml.Action actionUML) {
			for (InputPin inPin : actionUML.getInputs()) {
				Object targets = handler.addElement(inPin, this, inPin);
				if (targets instanceof List<?> targetsList) {
					for (Object singleTarget : targetsList) {
						if (singleTarget instanceof Relation targetRelation) {
							this.addInvolvedAssets(targetRelation);
						}
					}
				}
			}
			for (OutputPin outPin : actionUML.getOutputs()) {

					Object outputs = handler.addElement(outPin, this, outPin);
					if (outputs instanceof List<?> outputsList) {
						for (Object singleOutput : outputsList) {
							if (singleOutput instanceof Relation outputRelation) {
								this.addInvolvedAssets(outputRelation);
							}
						}
					}
			}
		}
	}
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		for (Function function : involvedParties) {
			Object functionMapObject = handler.createMap(function, circlePreventionSet);
			if (functionMapObject != null) {
				map.put(function.getClassTerm(), functionMapObject);
			}
		}
		for (Relation relation : involvedAssets) {
			Object relationMapObject = handler.createMap(relation, circlePreventionSet);
			if (relationMapObject != null) {
				map.put(relation.getClassTerm(), relationMapObject);
			}
		}
		return null;
	}
}
