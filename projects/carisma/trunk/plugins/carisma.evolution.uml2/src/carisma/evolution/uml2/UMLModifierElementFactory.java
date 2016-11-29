package carisma.evolution.uml2;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.ActivityEdge;
import org.eclipse.uml2.uml.ActivityGroup;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.ActivityPartition;
import org.eclipse.uml2.uml.AggregationKind;
import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.Association;
import org.eclipse.uml2.uml.Behavior;
import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.Comment;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Constraint;
import org.eclipse.uml2.uml.DataType;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Deployment;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Enumeration;
import org.eclipse.uml2.uml.EnumerationLiteral;
import org.eclipse.uml2.uml.ExpansionRegion;
import org.eclipse.uml2.uml.Generalization;
import org.eclipse.uml2.uml.InstanceSpecification;
import org.eclipse.uml2.uml.Interface;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.PackageableElement;
import org.eclipse.uml2.uml.Parameter;
import org.eclipse.uml2.uml.Profile;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Reception;
import org.eclipse.uml2.uml.Region;
import org.eclipse.uml2.uml.Signal;
import org.eclipse.uml2.uml.Slot;
import org.eclipse.uml2.uml.Transition;
import org.eclipse.uml2.uml.UMLFactory;
import org.eclipse.uml2.uml.UMLPackage;
import org.eclipse.uml2.uml.UseCase;
import org.eclipse.uml2.uml.ValueSpecification;
import org.eclipse.uml2.uml.Vertex;
import org.eclipse.uml2.uml.VisibilityKind;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.util.EObjectUtil;
import carisma.evolution.AddElement;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;


/**
 * The ElementFactory for the modifier for UML models.
 * @author Johannes Kowald
 *
 */
public class UMLModifierElementFactory {
	
	/**
	 * Separator for StringRepresentation of List-Elements in an Stereotype.
	 */
	public static final String VALUE_SEPARATOR = ";;";
	
	/**
	 * Name of the attribute "name".
	 */
	private static final String NAME = "name";
	
	/**
	 * Name of the attribute "value".
	 */
	private static final String VALUE = "value";
	
	/**
	 * Creates a UML model element from a given AddElement and
	 * integrates it into the model. 
	 * @param de - the add element describing the new model element.
	 * @return - the new UML Model element
	 */
	public final static EObject createElement(final AddElement de) {
		EClass classOfAddedElem = de.getMetaClass();
		if (classOfAddedElem.getName().equalsIgnoreCase("CommunicationPath")) {
			return createCommunicationPath(de);
		} else if (classOfAddedElem.getName().equalsIgnoreCase("Deployment")) {
			return createDeployment(de);
		} else if (classOfAddedElem.getName().equalsIgnoreCase("Stereotype")) {
			return createStereotypeApplication(de);
		} else if (classOfAddedElem.getName().equalsIgnoreCase("Property") && de.getTarget() != null && de.getTarget() instanceof StereotypeApplication) {
			return createTaggedValue(de);
		} else if (classOfAddedElem.getName().equalsIgnoreCase("Constraint")) {
			Constraint newConstraint = createConstraint(de);
			insertContainmentRelationship((Element) de.getTarget(), newConstraint);			
			return newConstraint;
		} else if (classOfAddedElem.getName().equalsIgnoreCase("Association")) {
			return createAssociation(de);
		}
		Element addedElem = (Element) UMLFactory.eINSTANCE.create(classOfAddedElem);
		insertContainmentRelationship((Element) de.getTarget(), addedElem);
		if (addedElem.getModel() == null) {
			Logger.log(LogLevel.ERROR, "The new element " + addedElem + " couldn't be inserted into the model.");
		}
		for (String key : de.getValues().keySet()) {
			Object mapValue = de.getValues().get(key);
			Object realValue = findRealValue(UMLHelper.getModel(de.getTarget()), key, mapValue);
			editStructuralFeatureValue(addedElem, key, realValue, false);
		}
		return addedElem;
	}
	
	/**
	 * Given the AddElement, this creates a UML Association according to the key/value pairs.
	 * @param add - the AddElement to use for the Association
	 * @return - the created Association
	 */
	private final static Association createAssociation(final AddElement add) {
		Model theModel = UMLHelper.getModel(add.getTarget());
		if (theModel == null) {
			return null;
		}
		String assocName = (String) add.getValues().get(NAME);
		String sourceName = (String) add.getValues().get("source");
		String sourceEndName = getEndName((String) add.getValues().get("sourceEndName"));
		AggregationKind sourceEndKind = getAggregationKind((String) add.getValues().get("sourceEndKind"), AggregationKind.COMPOSITE_LITERAL);
		int sourceLowerBound = getBound((String) add.getValues().get("sourceLowerBound"));
		int sourceUpperBound = getBound((String) add.getValues().get("sourceUpperBound"));
		boolean sourceNavigable = Boolean.parseBoolean((String) add.getValues().get("sourceNavigable"));

		String targetName = (String) add.getValues().get("target");
		String targetEndName = getEndName((String) add.getValues().get("targetEndName"));
		AggregationKind targetEndKind = getAggregationKind((String) add.getValues().get("targetEndKind"), AggregationKind.NONE_LITERAL);
		int targetLowerBound = getBound((String) add.getValues().get("targetLowerBound"));
		int targetUpperBound = getBound((String) add.getValues().get("targetUpperBound"));
		boolean targetNavigable = Boolean.parseBoolean((String) add.getValues().get("sourceNavigable"));
		try {
			Classifier source = UMLHelper.getElementOfNameAndType(theModel, sourceName, Classifier.class);
			Classifier target = UMLHelper.getElementOfNameAndType(theModel, targetName, Classifier.class);
			if (source != null && target != null) {
				Association newAssoc =
						source.createAssociation(
								sourceNavigable, 
								sourceEndKind, 
								sourceEndName, sourceLowerBound, sourceUpperBound, 
								target, 
								targetNavigable,
								targetEndKind,
								targetEndName, targetLowerBound, targetUpperBound);
				if (assocName != null) {
					newAssoc.setName(assocName);
				}
				return newAssoc;
			}
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);			
		}			
		return null;
	}
	
	/**
	 * Given the AddElement, this creates a UML CommunicationPath according to the key/value pairs.
	 * @param add - the AddElement to use for the CommunicationPath
	 * @return - the created CommunicationPath
	 */
	private final static CommunicationPath createCommunicationPath(final AddElement add) {
		Model theModel = UMLHelper.getModel(add.getTarget());
		if (theModel == null) {
			return null;
		}
		try {
			String sourceNodeName = (String) add.getValues().get("source");
			String targetNodeName = (String) add.getValues().get("target");
			String pathName = (String) add.getValues().get(NAME);
			Node sourceNode = UMLHelper.getElementOfNameAndType(theModel, sourceNodeName, Node.class);
			Node targetNode = UMLHelper.getElementOfNameAndType(theModel, targetNodeName, Node.class);
			if (sourceNode != null && targetNode != null) {
				CommunicationPath newPath = sourceNode.createCommunicationPath(false, AggregationKind.COMPOSITE_LITERAL,
						"", 1, 1, targetNode, false, AggregationKind.NONE_LITERAL, "", 1, 1);
				if (pathName != null) {
					newPath.setName(pathName);
				}
				return newPath;
			}
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);			
		}			
		return null;
	}
	
	/**
	 * Given the AddElement, this creates a UML Deployment according to the key/value pairs.
	 * @param add - the AddElement to use for the Deployment
	 * @return - the created Deployment
	 */
	private final static Deployment createDeployment(final AddElement add) {
		Model theModel = UMLHelper.getModel(add.getTarget());
		if (theModel == null) {
			return null;
		}
		String deploymentName = (String) add.getValues().get(NAME);
		String locationName = (String) add.getValues().get("location");
		String artifactName = (String) add.getValues().get("deployedArtifact");
		if (locationName == null || locationName.isEmpty() || artifactName == null || artifactName.isEmpty()) {
			return null;
		}
		try {
			Node location = UMLHelper.getElementOfNameAndType(theModel, locationName, Node.class);
			Artifact deployedArtifact = UMLHelper.getElementOfNameAndType(theModel, artifactName, Artifact.class);
			if (location != null && deployedArtifact != null) {
				Deployment newDeployment = location.createDeployment(deploymentName);
				newDeployment.getDeployedArtifacts().add(deployedArtifact);
				return newDeployment;
			}
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);			
		}			
		return null;
	}

	/**
	 * 
	 * @param de
	 * @return
	 */
	private static StereotypeApplication createStereotypeApplication(final AddElement de) {
		Model theModel = UMLHelper.getModel(de.getTarget());
		if (theModel == null) {
			return null;
		}
		String stereoName = (String) de.getValues().get(NAME);
		Element target = (Element) de.getTarget();
		if (stereoName.contains("::")) {
			return UMLHelper.applyStereotype(target, stereoName);			
		}
		for (Profile p : target.getModel().getAllAppliedProfiles()) {
			if (p.getOwnedStereotype(stereoName) != null) {
				return UMLHelper.applyStereotype(target, p.getName() + "::" + stereoName);
			}
		}
		return null;
	}
	
	/**
	 * 
	 * @param add
	 * @return
	 */
	private static TaggedValue createTaggedValue(final AddElement add) {
		StereotypeApplication sta = (StereotypeApplication) add.getTarget();
		String tagName = (String) add.getValues().get(NAME);
		String tagValue = (String) add.getValues().get(VALUE);
		TaggedValue tv = sta.getTaggedValue(tagName);
		tv.setValue(tagValue);
		return tv;
	}

	/**
	 * 
	 * @param add
	 * @return
	 */
	private static Constraint createConstraint(final AddElement add) {
		Model theModel = UMLHelper.getModel(add.getTarget());
		if (theModel == null) {
			return null;
		}
		String constraintName = (String) add.getValues().get(NAME);
		String specificationContent = (String) add.getValues().get("specification");
		String languageName = (String) add.getValues().get("language");
		if (constraintName != null && specificationContent != null) {
			Constraint newConstraint = UMLFactory.eINSTANCE.createConstraint();
			newConstraint.setName(constraintName);
			OpaqueExpression guard = UMLFactory.eINSTANCE.createOpaqueExpression();
			newConstraint.setSpecification(guard);
			if (languageName != null) {
				guard.getLanguages().add(languageName);
			}
			guard.getBodies().add(specificationContent);
			return newConstraint;
		}
		return null;
	}
	
	/**
	 * 
	 * @param targetElement
	 * @param key
	 * @param realValue
	 * @param substituteValue
	 */
	@SuppressWarnings("unchecked")
	public static void editStructuralFeatureValue(final Element targetElement, final String key, final Object realValue, final boolean substituteValue) {
		EStructuralFeature sf = targetElement.eClass().getEStructuralFeature(key);
		if (sf != null) {
			if (sf.isMany()) {
				if (realValue != null) {
					List<Object> featureValues = (List<Object>) targetElement.eGet(sf);
					if (substituteValue) {
						featureValues.clear();
					}
					if (realValue instanceof List) {
						featureValues.addAll((List<Object>) realValue);
					} else {
						featureValues.add(realValue);
					}
				}
			} else {
				try {
					if (key.equals("visibility") && targetElement instanceof NamedElement) {
						((NamedElement) targetElement).setVisibility((VisibilityKind) realValue);
					} else {
						targetElement.eSet(sf, realValue);
					}
					if (targetElement.eGet(sf) != realValue) {
						Logger.log(LogLevel.ERROR, "Couldn't set " + sf.getName() + " to " + realValue.toString());
					}
				} catch (ClassCastException e) {
					Logger.log(LogLevel.ERROR, "Wrong value \"" + realValue + "\" for the target.");
				}
			}
		}		
	}
	
	/**
	 * When creating new elements, some elements need to be explicitly added to their containers.
	 * @param container - the element containing the new element
	 * @param containedElement - the new element
	 */
	public static void insertContainmentRelationship(final Element container, final Element containedElement) {
		UMLPackage umlPackage = UMLPackage.eINSTANCE;
		if (umlPackage.getElement().isInstance(container)
				&& umlPackage.getComment().isInstance(containedElement)) {
			container.getOwnedComments().add((Comment) containedElement);
		}
		if (umlPackage.getConstraint().isInstance(container)) {
			Constraint con = (Constraint) container;
			if (umlPackage.getOpaqueExpression().isInstance(containedElement)) {
				con.setSpecification((OpaqueExpression) containedElement);
			}
		} else if (umlPackage.getActivity().isInstance(container)  //only exists in an ActivityDiagramm
				|| umlPackage.getExpansionRegion().isInstance(container)  //only exists in an Activity
				|| umlPackage.getActivityPartition().isInstance(container)) {  //only exists in an Activity
				insertContainmentRelationshipForActivity(container, containedElement);
		} else if (umlPackage.getPackage().isInstance(container)) {
			if (umlPackage.getNode().isInstance(containedElement)
					|| umlPackage.getExecutionEnvironment().isInstance(containedElement)
					|| umlPackage.getDevice().isInstance(containedElement)
					|| umlPackage.getArtifact().isInstance(containedElement)
					|| umlPackage.getDependency().isInstance(containedElement)
					|| umlPackage.getClass_().isInstance(containedElement)
					|| umlPackage.getDataType().isInstance(containedElement)
					|| umlPackage.getEvent().isInstance(containedElement)
					|| umlPackage.getInstanceSpecification().isInstance(containedElement)
					|| umlPackage.getPrimitiveType().isInstance(containedElement)
					|| umlPackage.getEnumeration().isInstance(containedElement)
					|| umlPackage.getSignal().isInstance(containedElement)
					|| umlPackage.getInterface().isInstance(containedElement)) {
				Package pkg = (Package) container;
				pkg.getPackagedElements().add((PackageableElement) containedElement);
			} else {
				Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) 
						+ "->" + EObjectUtil.getName(containedElement) + " not implemented!");				
			}
		} else if (umlPackage.getNode().isInstance(container)
				|| umlPackage.getDevice().isInstance(container)
				|| umlPackage.getExecutionEnvironment().isInstance(container)) { 
			Node theNode = (Node) container;
			if (umlPackage.getNode().isInstance(containedElement) 
					|| umlPackage.getExecutionEnvironment().isInstance(containedElement)
					|| umlPackage.getDevice().isInstance(containedElement)
					|| umlPackage.getArtifact().isInstance(containedElement)) {
				
				theNode.getOwnedElements().add(containedElement);
			} else if (umlPackage.getDeployment().isInstance(containedElement)
					|| umlPackage.getManifestation().isInstance(containedElement)
					|| umlPackage.getDependency().isInstance(containedElement)
					|| umlPackage.getDeploymentSpecification().isInstance(containedElement)) {
				theNode.getClientDependencies().add((Dependency) containedElement);
			} else if (umlPackage.getBehavior().isInstance(containedElement)) { 
				theNode.getOwnedBehaviors().add((Behavior) containedElement);
			} else if (umlPackage.getUseCase().isInstance(containedElement)) { 
				theNode.getOwnedUseCases().add((UseCase) containedElement);
			} else if (umlPackage.getOperation().isInstance(containedElement)) { 
				theNode.getOwnedOperations().add((Operation) containedElement);
			} else if (umlPackage.getProperty().isInstance(containedElement)) { 
				theNode.getOwnedAttributes().add((Property) containedElement);
			} else if (umlPackage.getConstraint().isInstance(containedElement)) { 
				theNode.getOwnedRules().add((Constraint) containedElement);
			} else { 
				Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) 
						+ "->" + EObjectUtil.getName(containedElement) + " not implemented!");	
			}
		} else if (umlPackage.getClass_().isInstance(container)) {
			Class theClass = (Class) container;
			if (umlPackage.getOperation().isInstance(containedElement)) {
				theClass.getOwnedOperations().add((Operation) containedElement);				
			} else if (umlPackage.getProperty().isInstance(containedElement)) {
				theClass.getOwnedAttributes().add((Property) containedElement);				
			} else if (umlPackage.getConstraint().isInstance(containedElement)) {
				theClass.getOwnedRules().add((Constraint) containedElement);
			} else if (UMLPackage.eINSTANCE.getReception().isInstance(containedElement)) {
				theClass.getOwnedReceptions().add((Reception) containedElement);
			} else if (UMLPackage.eINSTANCE.getGeneralization().isInstance(containedElement)) {
				theClass.getGeneralizations().add((Generalization) containedElement);
			} else if (UMLPackage.eINSTANCE.getDependency().isInstance(containedElement)) {
				theClass.getClientDependencies().add((Dependency) containedElement);
			} else {
				Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) 
						+ "->" + EObjectUtil.getName(containedElement) + " not implemented!");				
			}
		} else if (umlPackage.getDataType().isInstance(container)) {
			DataType dataType = (DataType) container;
			if (umlPackage.getOperation().isInstance(containedElement)) {
				dataType.getOwnedOperations().add((Operation) containedElement);				
			} else if (umlPackage.getProperty().isInstance(containedElement)) {
				dataType.getOwnedAttributes().add((Property) containedElement);				
			} else if (umlPackage.getConstraint().isInstance(containedElement)) {
				dataType.getOwnedRules().add((Constraint) containedElement);
			} else {
				Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) 
						+ "->" + EObjectUtil.getName(containedElement) + " not implemented!");				
			}
		} else if (UMLPackage.eINSTANCE.getEnumeration().isInstance(container)) {
			Enumeration enume = (Enumeration) container;
			if (UMLPackage.eINSTANCE.getEnumerationLiteral().isInstance(containedElement)) {
				enume.getOwnedLiterals().add((EnumerationLiteral) containedElement);
			}
		} else if (umlPackage.getInterface().isInstance(container)) {
			Interface interf = (Interface) container;
			if (umlPackage.getOperation().isInstance(containedElement)) {
				interf.getOwnedOperations().add((Operation) containedElement);				
			} else if (umlPackage.getProperty().isInstance(containedElement)) {
				interf.getOwnedAttributes().add((Property) containedElement);				
			} else if (umlPackage.getConstraint().isInstance(containedElement)) {
				interf.getOwnedRules().add((Constraint) containedElement);
			} else if (UMLPackage.eINSTANCE.getReception().isInstance(containedElement)) {
				interf.getOwnedReceptions().add((Reception) containedElement);
			} else {
				Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) 
						+ "->" + EObjectUtil.getName(containedElement) + " not implemented!");				
			}
		} else if (UMLPackage.eINSTANCE.getSignal().isInstance(container)) {
			Signal signal = (Signal) container;
			if (UMLPackage.eINSTANCE.getProperty().isInstance(containedElement)) {
				signal.getOwnedAttributes().add((Property) containedElement);
			}
		} else if (UMLPackage.eINSTANCE.getInstanceSpecification().isInstance(container)) { 
			InstanceSpecification instSpec = (InstanceSpecification) container;
			if (UMLPackage.eINSTANCE.getSlot().isInstance(containedElement)) { 
				instSpec.getSlots().add((Slot)containedElement);
			}
		} else if (UMLPackage.eINSTANCE.getSlot().isInstance(container)) { 
			Slot slot = (Slot) container;
			if (UMLPackage.eINSTANCE.getValueSpecification().isInstance(containedElement)) { 
				slot.getValues().add((ValueSpecification) containedElement);
			}
		} else if (umlPackage.getOperation().isInstance(container)) {
			Operation op = (Operation) container;
			if (UMLPackage.eINSTANCE.getParameter().isInstance(containedElement)) {
				op.getOwnedParameters().add((Parameter) containedElement);
			} else if (umlPackage.getConstraint().isInstance(containedElement)) { 
				op.getOwnedRules().add((Constraint) containedElement);
			}
			// TODO stopped here @ Class Diagram 
		} else if (UMLPackage.eINSTANCE.getAssociation().isInstance(container)) { 
			Association asso = (Association) container;
			if (UMLPackage.eINSTANCE.getProperty().isInstance(containedElement)) {
				asso.getOwnedEnds().add((Property) containedElement);
			}
		} else if (umlPackage.getRegion().isInstance(container)) {
			Region rgn = (Region) container;
			if (umlPackage.getTransition().isInstance(containedElement)) {
				Transition t = (Transition) containedElement;
				t.setContainer(rgn);
			} else if (umlPackage.getVertex().isInstance(containedElement)) {
				((Vertex) containedElement).setContainer(rgn);
			} else if (umlPackage.getComment().isInstance(containedElement)) {
				rgn.getOwnedComments().add((Comment) containedElement);
			} else if (umlPackage.getConstraint().isInstance(containedElement)) {
				rgn.getOwnedRules().add((Constraint) containedElement);
			} else {
				Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) + "->" 
			+ EObjectUtil.getName(containedElement) + " not implemented!");				
			}
		} else if (umlPackage.getTransition().isInstance(container)) {
			if (umlPackage.getConstraint().isInstance(containedElement)) {
				((Transition) container).setGuard((Constraint) containedElement);
			} else {
				Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) + "->" 
			+ EObjectUtil.getName(containedElement) + " not implemented!");				
			}
		} else {
			Logger.log(LogLevel.WARNING, "Invalid new owner!");
			Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) + "->" 
			+ EObjectUtil.getName(containedElement) + " not implemented!");				
		}
	}
	
	/**
	 * When creating new elements, some elements need to be explicitly added to their containers.
	 * This method is just for Elements of an ActivityDiagramm.
	 * @param container - the element containing the new element
	 * @param containedElement - the new element
	 */
	private static void insertContainmentRelationshipForActivity(final Element container, final Element containedElement) {
		UMLPackage uml = UMLPackage.eINSTANCE;
		//TODO maybe ONE UMLPackage reference for the whole class.
		if (container.equals(containedElement)) {
			Logger.log(LogLevel.WARNING, "Stopped: Tried to add an Element to itself!");
			return;
		}
		if (uml.getActivity().isInstance(container)) {
			Activity activity = (Activity) container;
			if (uml.getNode().isInstance(containedElement) 
					|| uml.getAction().isInstance(containedElement)
					|| uml.getCentralBufferNode().isInstance(containedElement)
					|| uml.getDecisionNode().isInstance(containedElement)
					|| uml.getMergeNode().isInstance(containedElement)
					|| uml.getForkNode().isInstance(containedElement)
					|| uml.getJoinNode().isInstance(containedElement)
					|| uml.getActivityFinalNode().isInstance(containedElement)
					|| uml.getFlowFinalNode().isInstance(containedElement)
					|| uml.getActivityParameterNode().isInstance(containedElement)
					|| uml.getExpansionNode().isInstance(containedElement)
					|| uml.getInitialNode().isInstance(containedElement)
					|| uml.getPin().isInstance(containedElement)) {
				activity.getNodes().add((ActivityNode) containedElement);
			} else if (uml.getControlFlow().isInstance(containedElement)) {
				activity.getEdges().add((ActivityEdge) containedElement);
			} else if (uml.getComment().isInstance(containedElement)) {
				activity.getOwnedComments().add((Comment) containedElement);
			} else if (uml.getConstraint().isInstance(containedElement)) {
				activity.getOwnedRules().add((Constraint) containedElement);
			} else if (uml.getActivityPartition().isInstance(containedElement)) {
				activity.getPartitions().add((ActivityPartition) containedElement);
			} else if (uml.getInterruptibleActivityRegion().isInstance(containedElement)) {
				activity.getGroups().add((ActivityGroup) containedElement);
			} else {
				Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) + "->" 
						+ EObjectUtil.getName(containedElement) + " not implemented!");		
			}
		} else if (uml.getExpansionRegion().isInstance(container)) {
			ExpansionRegion expansionRegion = (ExpansionRegion) container;
			if (uml.getNode().isInstance(containedElement) 
					|| uml.getAction().isInstance(containedElement)
					|| uml.getCentralBufferNode().isInstance(containedElement)
					|| uml.getDecisionNode().isInstance(containedElement)
					|| uml.getMergeNode().isInstance(containedElement)
					|| uml.getForkNode().isInstance(containedElement)
					|| uml.getJoinNode().isInstance(containedElement)
					|| uml.getActivityFinalNode().isInstance(containedElement)
					|| uml.getFlowFinalNode().isInstance(containedElement)
					|| uml.getActivityParameterNode().isInstance(containedElement)
					|| uml.getExpansionNode().isInstance(containedElement)
					|| uml.getInitialNode().isInstance(containedElement)
					|| uml.getPin().isInstance(containedElement)) {
				expansionRegion.getNodes().add((ActivityNode) containedElement);
			} else if (uml.getControlFlow().isInstance(containedElement)) {
				expansionRegion.getEdges().add((ActivityEdge) containedElement);
			} else if (uml.getComment().isInstance(containedElement)) {
				expansionRegion.getOwnedComments().add((Comment) containedElement);
			} else if (uml.getConstraint().isInstance(containedElement)) {
				expansionRegion.getOwnedRules().add((Constraint) containedElement);
			} else {
				Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) + "->" 
						+ EObjectUtil.getName(containedElement) + " not implemented!");		
			}
		} else if (uml.getActivityPartition().isInstance(container)) {
			ActivityPartition activityPartition =  (ActivityPartition) container;
			if (uml.getNode().isInstance(containedElement) 
					|| uml.getAction().isInstance(containedElement)
					|| uml.getCentralBufferNode().isInstance(containedElement)
					|| uml.getDecisionNode().isInstance(containedElement)
					|| uml.getMergeNode().isInstance(containedElement)
					|| uml.getForkNode().isInstance(containedElement)
					|| uml.getJoinNode().isInstance(containedElement)
					|| uml.getActivityFinalNode().isInstance(containedElement)
					|| uml.getFlowFinalNode().isInstance(containedElement)
					|| uml.getActivityParameterNode().isInstance(containedElement)
					|| uml.getExpansionNode().isInstance(containedElement)
					|| uml.getInitialNode().isInstance(containedElement)
					|| uml.getPin().isInstance(containedElement)) {
				activityPartition.getNodes().add((ActivityNode) containedElement);
			} else if (uml.getControlFlow().isInstance(containedElement)) {
				activityPartition.getEdges().add((ActivityEdge) containedElement);
			} else if (uml.getComment().isInstance(containedElement)) {
				activityPartition.getOwnedComments().add((Comment) containedElement);
			} else {
				Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) + "->" 
						+ EObjectUtil.getName(containedElement) + " not implemented!");
			}
		} else {
			Logger.log(LogLevel.WARNING, "Containment " + EObjectUtil.getName(container) + "->" 
					+ EObjectUtil.getName(containedElement) + " not implemented!");					
		}
	}

	/**
	 * Finds the real value of an key-value-map value. UMLchange values can be strings
	 * which can be references to elements in the model (or UMLPrimitiveTypes).
	 * This dereferences the value to the real EObject. 
	 * @param model -
	 * @param value -
	 * @return -
	 */
	public final static Object findRealValue(final Model model, final String key, final Object value) {
		try { 
			if (value instanceof String) {
				String stringValue = (String) value;
				if (key.equals(NAME) || key.equals("body")) {
					return stringValue;
				}
				if (key.equals("visibility")) {
					return getVisibilityKind(stringValue, VisibilityKind.PUBLIC_LITERAL);
				}
				if (key.equals("sourceEndKind") || key.equals("targetEndKind")) {
					return getAggregationKind(stringValue, AggregationKind.NONE_LITERAL);
				}
				if (key.equals("sourceLowerBound") || key.equals("targetLowerBound") ||
						key.equals("sourceUpperBound") || key.equals("targetUpperBound")) {
					return Integer.valueOf(getBound(stringValue));
				}
				if (key.equals("sourceNavigable") || key.equals("targetNavigable")) {
					return Boolean.valueOf(stringValue);
				}
				if (key.startsWith("is") && (stringValue.equals("true") || stringValue.equals("false"))) {
					return Boolean.valueOf(stringValue);
				}
				if (key.startsWith("end")) {
					// was ist mit "alterSource;;neuerSource"?
					Pattern endPattern = Pattern.compile("^end\\d{1,}$");
					Matcher endMatcher = endPattern.matcher(key);
					if (endMatcher.find()) {
						String [] oldNew = stringValue.split(VALUE_SEPARATOR);
						String oldElement = oldNew[0];
						Element oldRealElement = UMLHelper.getElementByName(model, oldElement);
						String newElement = oldNew[1];
						Element newRealElement = UMLHelper.getElementByName(model, newElement);
						if (oldRealElement != null && newRealElement != null) {
							Map<Element, Element> oldNewElement = new HashMap<>();
							oldNewElement.put(oldRealElement, newRealElement);
							return oldNewElement;
						}
					}
				}
				if (key.equals(VALUE)) {
					List<String> values = new ArrayList<>();
					values.addAll(Arrays.asList(((String) value).split(VALUE_SEPARATOR)));
					return values;
				}
				if (UMLHelper.isPrimitiveType(model, stringValue)) {
					return UMLHelper.getPrimitiveType(model, stringValue);
				}
				Element realElement = UMLHelper.getElementByName(model, stringValue);
				if (realElement != null) {
					return realElement;
				}
				Logger.log(LogLevel.ERROR, "Couldn't find model element " + stringValue);
			}
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);			
		}
		return value;
	}
	
	/**
	 * 
	 * @param value
	 * @return
	 */
	private static String getEndName(final String value) {
		if (value == null) {
			return "";
		}
		return value;
	}
	
	/**
	 * 
	 * @param value
	 * @param defaultValue
	 * @return
	 */
	private static AggregationKind getAggregationKind(final String value, final AggregationKind defaultValue) {
		if (value != null) {
			if (value.equalsIgnoreCase("none")) {
				return AggregationKind.NONE_LITERAL;
			}
			if (value.equalsIgnoreCase("shared")) {
				return AggregationKind.SHARED_LITERAL;
			}
			if (value.equalsIgnoreCase("composite")) {
				return AggregationKind.COMPOSITE_LITERAL;
			}
		}
		if (defaultValue != null) {
			return defaultValue;
		}
		return AggregationKind.COMPOSITE_LITERAL;

	}
	
	/**
	 * 
	 * @param value
	 * @param defaultValue
	 * @return
	 */
	private static VisibilityKind getVisibilityKind(final String value, final VisibilityKind defaultValue) {
		if (value != null) {
			if (value.equalsIgnoreCase("public")) {
				return VisibilityKind.PUBLIC_LITERAL;
			}
			if (value.equalsIgnoreCase("package")) {
				return VisibilityKind.PACKAGE_LITERAL;
			}
			if (value.equalsIgnoreCase("protected")) {
				return VisibilityKind.PROTECTED_LITERAL;
			}
			if (value.equalsIgnoreCase("private")) {
				return VisibilityKind.PRIVATE_LITERAL;
			}
		}
		if (defaultValue != null) {
			return defaultValue;
		}
		return VisibilityKind.PUBLIC_LITERAL;
	}
	
	/**
	 * 
	 * @param value
	 * @return
	 */
	private static int getBound(final String value) {
		if (value == null) {
			return 1;
		}
		Pattern number = Pattern.compile("^\\d{1,}$");
		Matcher numberMatcher = number.matcher(value);
		if (numberMatcher.find()) {
			return Integer.parseInt(numberMatcher.group());
		}
		if (value.equalsIgnoreCase("*") 
				|| value.equalsIgnoreCase("n") 
				|| value.equalsIgnoreCase("m")) {
			return -1;
		}
		return 1;
	}
}
