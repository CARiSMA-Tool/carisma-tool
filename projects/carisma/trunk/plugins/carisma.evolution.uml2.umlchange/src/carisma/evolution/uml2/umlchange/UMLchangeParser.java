/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.evolution.uml2.umlchange;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.uml2.uml.Association;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Namespace;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.Transition;
import org.eclipse.uml2.uml.Type;
import org.eclipse.uml2.uml.Vertex;
import org.eclipse.uml2.uml.VisibilityKind;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.util.EObjectUtil;
import carisma.evolution.AddElement;
import carisma.evolution.AdditiveElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.ChangeConstraint;
import carisma.evolution.ConstraintType;
import carisma.evolution.CopyElement;
import carisma.evolution.DelElement;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.IDeltaDescriptionGenerator;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.umlchange.datatype.ElementDescription;
import carisma.evolution.uml2.umlchange.datatype.GrammarAlternative;
import carisma.evolution.uml2.umlchange.datatype.GrammarBlock;
import carisma.evolution.uml2.umlchange.datatype.NamespaceDescription;
import carisma.evolution.uml2.umlchange.datatype.ParserUtils;
import carisma.evolution.uml2.umlchange.datatype.SimpleElementDescription;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.InvalidMetaclassException;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlchange.UMLchangeUtil;
/**
 * A class to parse UML models for UMLchange stereotypes.
 * The class extracts all information from the stereotypes
 * regarding the changes and stores them in change descriptions.
 * @author Daniel Warzecha
 *
 */
public class UMLchangeParser implements IDeltaDescriptionGenerator {
    /**
	 * The model containing UMLchange stereotypes.
	 */
	private Model theModel = null;
	
	/**
	 * Host.
	 */
	private AnalysisHost analysisHost = null;

	/**
	 * Constant name for the key 'name'.
	 */
	private static final String NAME = "name";
	/**
	 * Constant name for the key 'value'.
	 */
	private static final String VALUE = "value";
	
	/**
	 * Constant name for the key 'owner'.
	 */
	private static final String OWNER = "owner";
	
	/**
	 * Constant for String for asterisk in Property value.
	 */
	private static final String ASTERISK = "\\*";
	
	private Map<UMLchange,List<Element>> stereotypeMapping = new HashMap<>();
	
	/**
	 * A mapping of elements marked <<old>> to the corresponding elements in the original model. 
	 */
	private Map<NamedElement, NamedElement> oldMarkedOriginalElementMapping = null;
	
	/**
	 * A mapping of change ID's to the changes they are from.
	 */
	private Map<String, Change> changeRefChangeMapping = null;
	/**
	 * A mapping of changes to their constraint Strings.
	 */
	private Map<Change, String> changeConstraintMapping = null;

	/** Public constructor setting the used Model.
	 * 
	 * @param newModel Model to be used.
	 */
	public UMLchangeParser(final Model newModel) {
		this(newModel, null);
	}
	/**
	 * Constructs a parser working on the given model.
	 * @param newModel - model to parse 
	 * @param theHost - the Host.
	 */
	public UMLchangeParser(final Model newModel, final AnalysisHost theHost) {
		setModel(newModel);
		setHost(theHost);
	}
	
	/** Setter for host.
	 * 
	 * @param theHost if null host will be set to 'new DummyHost(true)'.
	 */
	public final void setHost(final AnalysisHost theHost) {
		if (theHost != null) {
			this.analysisHost = theHost;
		} else if (this.analysisHost == null) {
			this.analysisHost = new DummyHost(true);
		}
	}
	
	/**
	 * Sets a new model.
	 * @param newModel - the new model
	 */
	public final void setModel(final Model newModel) {
		if (newModel != null && !newModel.equals(this.theModel)) {
			this.theModel = newModel;
		}
		init();
	}
	
	/**
	 * Returns the currently used model.
	 * @return - the model
	 */
	public final Model getModel() {
		return this.theModel;
	}
	
	/** init Method provides 'fresh' fields.
	 *        oldMarkedOriginalElementMapping; 
	 *        changeRefChangeMapping;
	 *        changeConstraintMapping;
	 *  
	 */
	private void init() {
		if (this.oldMarkedOriginalElementMapping == null) {
			this.oldMarkedOriginalElementMapping = new HashMap<>();
		} else {
			this.oldMarkedOriginalElementMapping.clear();
		}
		if (this.changeRefChangeMapping == null) {
			this.changeRefChangeMapping = new HashMap<>();
		} else {
			this.changeRefChangeMapping.clear();
		}
		if (this.changeConstraintMapping == null) {
			this.changeConstraintMapping = new HashMap<>();
		} else {
			this.changeConstraintMapping.clear();
		}
		for (UMLchange e : UMLchange.values()) {
			if (this.stereotypeMapping.get(e) == null) {
				this.stereotypeMapping.put(e, new ArrayList<Element>());
			} else {
				this.stereotypeMapping.get(e).clear();
			}
		}
	}
	
	private void storeStereotypeMapping() {
		for (Element elem : this.theModel.allOwnedElements()) {
			for (UMLchange e : UMLchange.values()) {
				if (UMLchangeUtil.hasStereotype(e, elem)) {
					this.stereotypeMapping.get(e).add(elem);
				}				
			}
		}
	}
	/**
	 * Generates the list of change descriptions
	 * by parsing the model for UMLchange stereotypes.
	 * @return - list of change descriptions
	 */
	@Override
	public final List<Change> generateDeltaDescriptions() {
		List<Change> changes = new ArrayList<>();
		if (UMLHelper.isProfileApplied(this.theModel, UMLchange.DESCRIPTOR)) {
			init();
			storeStereotypeMapping();
			createOldOriginalMapping();
			for (UMLchange e : UMLchange.values()) {
				for (Element extendedElement : this.stereotypeMapping.get(e)) {
					changes.addAll(processUMLchangeApplications(extendedElement));
				}				
			}
			for (Change change : changes) {
				change.replaceConstraints(createChangeConstraints(change, this.changeConstraintMapping.get(change)));
			}
		}
		return changes;
	}

	private void createOldOriginalMapping() {
		for (Element oldElement : this.stereotypeMapping.get(UMLchange.OLD)) {
			if (oldElement instanceof NamedElement) {
				NamedElement namedOldElement = (NamedElement) oldElement;
				this.oldMarkedOriginalElementMapping.put(namedOldElement, getOriginalElement(namedOldElement, this.theModel));
			}
		}
	}
	
	private List<ChangeConstraint> createChangeConstraints(final Change change, final String constraintString) {
		List<ChangeConstraint> changeConstraints = new ArrayList<>();
		if (constraintString != null) {
			List<String> constraints = Arrays.asList(constraintString.split(","));
			for (String constraint : constraints) {
				constraint = constraint.replaceAll("[^a-zA-Z0-9\\(\\),]", "");
				try {
					ChangeConstraint con = createChangeConstraint(change, constraint);
					if (con != null) {
						changeConstraints.add(con);
					}
				} catch (IllegalArgumentException e) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, e.getMessage()));					
				}
			}
		}
		return changeConstraints;
	}
	
	private ChangeConstraint createChangeConstraint(final Change constrainedChange, final String constraint) {
		if (constraint != null && !constraint.isEmpty()) {
			String type = "";
			String referencedChange = "";
			Pattern pattern = Pattern.compile("^.*(?=\\()");
			Matcher matcher = pattern.matcher(constraint);
			if (matcher.find()) {
				type = matcher.group();
				matcher.reset();
				matcher.usePattern(Pattern.compile("(?<=\\().*(?=\\))"));
				if (matcher.find()) {
					referencedChange = matcher.group();
				}
			}
			if (!type.isEmpty() && !referencedChange.isEmpty()) {
				if (!ConstraintType.contains(type)) {
					throw new IllegalArgumentException("Constraint type " + type + " at constrained change " + constrainedChange.getRef() + " does not exist.");
				}
				ConstraintType theType = ConstraintType.valueOf(type);
				Change theReferencedChange = this.changeRefChangeMapping.get(referencedChange);
				if (theReferencedChange == null) {
					throw new IllegalArgumentException("Change " + referencedChange + " at constrained change " 
				+ constrainedChange.getRef() + " does not exist.");					
				}
				if (theType != null) {
					return new ChangeConstraint(theType, theReferencedChange, constrainedChange);
				}
			}
		}
		return null;
	}
	
	private NamedElement getOriginalElement(final NamedElement oldMarkedElement, final Element searchScope) {
		String oldName = oldMarkedElement.getName();
		for (Element elementInScope : searchScope.allOwnedElements()) {
			if (elementInScope instanceof NamedElement) {
				NamedElement candidate = (NamedElement) elementInScope;
				if (candidate.getName().equalsIgnoreCase(oldName) && !(this.stereotypeMapping.get(UMLchange.OLD).contains(candidate))) {
					return candidate;
				}
			}
		}
		return oldMarkedElement;
	}
	
	
	/**
	 * Returns all changes described by an UMLchange extended element.
	 * @param extendedElement - the element extended by UMLchange
	 * @return - the list of changes at this element
	 */
	private List<Change> processUMLchangeApplications(final Element extendedElement) {
		List<Change> changes = new ArrayList<>();
		for (StereotypeApplication changeApplication : UMLchangeUtil.getStereotypeApplications(extendedElement)) {
			UMLchange type = UMLchange.getValue(changeApplication.getAppliedStereotype().getName());
			
			if (type == UMLchange.OLD || type == UMLchange.KEEP) {
				continue;
			}
			
			List<String> refValues = changeApplication.getTaggedValue("ref").getStringValues();
			if (refValues.isEmpty()) {
				this.analysisHost.addResultMessage(
						new AnalysisResultMessage(
								StatusType.WARNING, "UMLchange application of " 
						+ type.toString() + " at " 
						+ EObjectUtil.getTypeAndName(extendedElement) + " has no change IDs!"));					
				continue;
			}
			
			List<String> extValues = changeApplication.getTaggedValue("ext").getStringValues();
			List<String> constraintValues = changeApplication.getTaggedValue("constraint").getStringValues();
			
			List<String> newValues = null;
			if (UMLchangeUtil.hasNew(type)) {
				newValues = changeApplication.getTaggedValue("new").getStringValues();
			}
			
			List<String> patternValues = null;
			if (UMLchangeUtil.hasPattern(type)) {
				patternValues = changeApplication.getTaggedValue("pattern").getStringValues();
			}
			
			List<String> toValues = null;
			if (UMLchangeUtil.hasTo(type)) {
				toValues = changeApplication.getTaggedValue("to").getStringValues();
			}
			
			for (String refValue : refValues) {
				if (refValue.isEmpty()) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Empty string as ref at " + EObjectUtil.getTypeAndName(extendedElement) + "."));					
					continue;
				}
				String extValue = ParserUtils.getMatchingValues(refValue, extValues);
				String constraintValue = ParserUtils.getMatchingValues(refValue, constraintValues);
								
				String newValue = null;
				GrammarBlock newBlock = null;
				if (UMLchangeUtil.hasNew(type)) {
					newValue = ParserUtils.getMatchingValues(refValue, newValues);
					if (newValue.isEmpty()) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Couldn't find corresponding new value for " + refValue + "."));					
						continue;						
					}
// FIXME: use UMLchangeValidator class for validating new, pattern, to, values
					newBlock = new GrammarBlock(newValue);
					if (!newBlock.isValid()) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Wrong syntax used for new value " + newValue + "."));
						continue;												
					}
				}
				
				String patternValue = null;
				List<EObject> realMatchingElements = new ArrayList<>();
				if (UMLchangeUtil.hasPattern(type)) {
					patternValue = ParserUtils.getMatchingValues(refValue, patternValues);
					if (patternValue.isEmpty()) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Couldn't find corresponding pattern value for " + refValue + "."));
						continue;						
					}
					// FIXME: use UMLchangeValidator class for validating new, pattern, to, values
					GrammarBlock patternBlock = new GrammarBlock(patternValue);
					if (!patternBlock.isValid()) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Wrong syntax used for pattern value " + patternValue + "."));
						continue;												
					}
					try {
						SimpleElementDescription patternDescription =
								(SimpleElementDescription) patternBlock.
								getAlternatives().get(0).
								getDescriptions().get(0);
						
						List<Element> matchingElements = getPatternMatches(extendedElement, patternDescription);
						if (matchingElements.isEmpty()) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Couldn't find matching elements for " + patternDescription + "."));
						}
						
						for (Element matchingElement : matchingElements) {
							try {	
								EObject changedElement = determineChangedElement(extValue, matchingElement);
								if (changedElement != null) {
									realMatchingElements.add(changedElement);
								}
							} catch (ModelElementNotFoundException e) {
								this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
										"Couldn't find the changed element described by " + extValue + "." 
										+ " Searched at " + EObjectUtil.getTypeAndName(matchingElement) + "."));
								continue;
							}
						}
					} catch (IndexOutOfBoundsException e) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Pattern description either has zero alternatives or zero descriptions. "));
					}
				}
				
				String toValue = null;
				GrammarBlock toBlock = null;
				if (UMLchangeUtil.hasTo(type)) {
					toValue = ParserUtils.getMatchingValues(refValue, toValues);
					if (toValue.isEmpty()) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Couldn't find corresponding to value for " + refValue + "."));
						continue;						
					}
					// FIXME: use UMLchangeValidator class for validating new, pattern, to, values
					toBlock = new GrammarBlock(toValue);
					if (!toBlock.isValid()) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Wrong syntax used for to value " + toValue + "."));
						continue;												
					}
				}
				List<Alternative> alternatives = new ArrayList<>();
				if (!UMLchangeUtil.hasPattern(type)) {
					EObject changedElement = null;
					try {
						changedElement = determineChangedElement(extValue, extendedElement);
					} catch (ModelElementNotFoundException e) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
								"Couldn't find the changed element described by " + extValue + "." 
								+ " Searched at " + EObjectUtil.getTypeAndName(extendedElement) + "."));
						continue;
					}
					if (type == UMLchange.DEL) {
						alternatives.add(parseDelChange(changedElement));
					} else if (type == UMLchange.ADD) {
						alternatives.addAll(parseAddChange(changedElement, extendedElement, newBlock));
					} else if (type == UMLchange.SUBST) {
						alternatives.addAll(parseSubstChange(changedElement, extendedElement, newBlock, refValue));
					} else if (type == UMLchange.EDIT) {
						List<String> editedValues = changeApplication.getTaggedValue("values").getStringValues();
						String editValue = ParserUtils.getMatchingValues(refValue, editedValues);
						if (editValue.isEmpty()) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Couldn't find corresponding values value for " + refValue + "."));
							continue;						
						}
						// FIXME: use UMLchangeValidator class for validating new, pattern, to, values
						GrammarBlock editBlock = new GrammarBlock(editValue);
						if (!editBlock.isValid()) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Wrong syntax used for edit value " + editValue + "."));
							continue;
						}
						alternatives.addAll(parseEditChange(changedElement, editBlock));
					} else if (type == UMLchange.MOVE) {
						alternatives.addAll(parseMoveChange(changedElement, toBlock));
					} else if (type == UMLchange.COPY) {
						alternatives.addAll(parseCopyChange(changedElement, toBlock));
					}
				} else {
					if (type == UMLchange.DELALL) {
						if (!realMatchingElements.isEmpty()) {
							alternatives.add(parseDelAllChange(realMatchingElements));
						}
					} else if (type == UMLchange.ADDALL) {
						if (!realMatchingElements.isEmpty()) {
							alternatives.addAll(parseAddAllChange(realMatchingElements, newBlock));
						}
					} else if (type == UMLchange.SUBSTALL
							&& !realMatchingElements.isEmpty()) {
						alternatives.addAll(parseSubstAllChange(realMatchingElements, newBlock, refValue));
					}
				}
				if (!alternatives.isEmpty()) {
					Change change = new Change(refValue);
					change.replaceAlternatives(alternatives);
					this.changeRefChangeMapping.put(refValue, change);
					this.changeConstraintMapping.put(change, constraintValue);
					changes.add(change);
				}
			}
		}
		return changes;
	}
	
	private static Alternative parseDelChange(final EObject changedElement) {
		Alternative onlyAlternative = new Alternative();
		DelElement del = new DelElement(changedElement);
		del.getAccompanyingDeletions().addAll((collectDeletedElements(changedElement, UMLchange.DEL)));
		onlyAlternative.addDeltaElement(del);
		return onlyAlternative;
	}
	
	/**
	 * Parses the <<add>> change at the extended element.
	 * If the new elements are described using simple element descriptions, normal AddElements are created. 
	 * If the new elements are described using a complex namespace, 
	 * the alternative receives one or more EditElements transferring the
	 * ownership of the new elements from the complex namespace to the normal model.
	 * @param changedElement - the element that receives the new elements
	 * @param extendedElement - the element that has <<add>> applied to it
	 * @param newBlock - the grammar describing the new elements
	 * @param refValue - the change id
	 * @return - A list of alternatives containing appropriate DeltaElements
	 */
	private List<Alternative> parseAddChange(
			final EObject changedElement, 
			final Element extendedElement, 
			final GrammarBlock newBlock) {
		List<Alternative> newAlternatives = new ArrayList<>();
		for (GrammarAlternative newAlt : newBlock.getAlternatives()) {
			List<DeltaElement> altContent = new ArrayList<>();
			for (ElementDescription desc : newAlt.getDescriptions()) {
				if (desc instanceof SimpleElementDescription) {
					altContent.addAll(parseSimpleElementDescriptions(
							(SimpleElementDescription) desc, changedElement));									
				} else if (desc instanceof NamespaceDescription && extendedElement instanceof NamedElement) {
					NamespaceDescription nsDesc = (NamespaceDescription) desc;
					Namespace complexNamespace = null;
					try {
						complexNamespace = UMLHelper.getElementOfNameAndType(this.theModel, nsDesc.getNamespaceName(), Namespace.class);
					} catch (ModelElementNotFoundException e) {
						Logger.log(LogLevel.ERROR, e.getMessage());
						continue;
					}
					if (complexNamespace != null) {
						altContent.addAll(parseNamespace(complexNamespace, changedElement));
					}
				}
			}
			if (!altContent.isEmpty()) {
				Alternative alt = new Alternative();
				alt.replaceDeltaElements(altContent);
				newAlternatives.add(alt);
			}
		}
		return newAlternatives;
	}
	
	/**
	 * Parses the <<subst>> change at the extended element.
	 * If the substitutes are described using simple element descriptions, a normal SubstElement is created.
	 * If the substitutes are described using a complex namespace, 
	 * the alternative receives a DelElement and one or more EditElements transferring the
	 * ownership of the substitutes from the complex namespace to the normal model.
	 * @param changedElement - the element that is substituted
	 * @param extendedElement - the element that has <<subst>> applied to it
	 * @param newBlock - the grammar describing the substitution
	 * @param refValue - the change id
	 * @return - A list of alternatives containing appropriate DeltaElements
	 */
	private List<Alternative> parseSubstChange(
			final EObject changedElement, 
			final Element extendedElement, 
			final GrammarBlock newBlock,
			final String refValue) {
		List<Alternative> newAlternatives = new ArrayList<>();
		EObject substituteOwner = determineSubstituteOwner(changedElement);
		int subAlternativeCount = 0;
		int subAlternativeSize = newBlock.getAlternatives().size();
		for (GrammarAlternative newAlt : newBlock.getAlternatives()) {
			subAlternativeCount++;
			Alternative alt = new Alternative();
			List<AddElement> substitutes = new ArrayList<>();
			for (ElementDescription desc : newAlt.getDescriptions()) {
				if (desc instanceof SimpleElementDescription) {
					substitutes.addAll(parseSimpleElementDescriptions(
							(SimpleElementDescription) desc, substituteOwner));
					if (!substitutes.isEmpty()) {
						SubstElement substElem = new SubstElement(changedElement, substitutes);
						substElem.getAccompanyingDeletions().addAll(((collectDeletedElements(changedElement, UMLchange.DEL))));								
						alt.addDeltaElement(substElem);
					}
				} else if (desc instanceof NamespaceDescription && extendedElement instanceof NamedElement) {
					NamespaceDescription nsDesc = (NamespaceDescription) desc;
					Namespace complexNamespace = null;
					try {
						complexNamespace = UMLHelper.getElementOfNameAndType(this.theModel, nsDesc.getNamespaceName(), Namespace.class);							
					} catch (ModelElementNotFoundException e) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, e.getMessage()));
						continue;
					}
					if (complexNamespace != null) {
						if (changedElement instanceof NamedElement) {
							alt.addDeltaElements(generateEditsOfKeep((NamedElement) changedElement, subAlternativeCount, refValue,
									subAlternativeSize, nsDesc.getNamespaceName()));
						}
						DelElement del = new DelElement(changedElement);
						del.getAccompanyingDeletions().addAll(((collectDeletedElements(changedElement, UMLchange.SUBST))));
						alt.addDeltaElement(del);
						alt.addDeltaElements(parseNamespace(complexNamespace, substituteOwner));
					}
				}
			}
			newAlternatives.add(alt);
		}
		return newAlternatives;
	}
	
	
	/**
	 * creates EditElements for all elements that are sub-elements of changedElement and got the stereotype <<keep>> applied.
	 * @param changedElement - element that will be substituted
	 * @param subAlternativeCount - counter of the alternatives
	 * @param refValue - referenceID of the change
	 * @param maxAlternatives - maximum amount of alternatives
	 * @param targetNameSpace - the new owning NameSpace of the edited Elements
	 * @return - List with all EditElements that will be generated
	 */
	private List<EditElement> generateEditsOfKeep(final NamedElement changedElement,
			final int subAlternativeCount, final String refValue, final int maxAlternatives, final String targetNameSpace) {
		List<EditElement> editList = new ArrayList<>();
		for (Element keepElement : UMLchangeUtil.getStereotypedElements(UMLchange.KEEP, changedElement)) {
				if (keepElement instanceof Association) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Associations " + ((NamedElement) keepElement).getName()
							+ " in Element " + changedElement.getName() + " won't be handled!"));
				} else {
					StereotypeApplication changeApplication = UMLchangeUtil.getStereotypeApplication(UMLchange.KEEP, keepElement);
					List<String> adoptings = changeApplication.getTaggedValue("adopter").getStringValues();
					String adoptValue = ParserUtils.getMatchingValues(refValue, adoptings);
					GrammarBlock adoptBlock = new GrammarBlock(adoptValue);
					if (adoptBlock.getAlternatives().size() > maxAlternatives) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "More alternatives in the <<keep>> stereotype of Element "
								+ keepElement + " than in the <<subst>> stereotype of " + changedElement));
					} else {
						int keepAlternativeCount = 0;
						for (GrammarAlternative alternative : adoptBlock.getAlternatives()) {
							keepAlternativeCount++;
							if (subAlternativeCount == keepAlternativeCount) {
								for (ElementDescription elementDescription : alternative.getDescriptions()) {
									SimpleElementDescription desc = (SimpleElementDescription) elementDescription;
									try {
										Element targetElement = UMLHelper.getElementByName(UMLHelper.getModel(changedElement),
												targetNameSpace + "::" + (desc.getKeyValuePairs().get(NAME)));
										EditElement editElement = new EditElement(keepElement);
										editList.add(editElement);
										for (String key : (desc.getKeyValuePairs().keySet())) {
											editElement.addKeyValuePair(key, (desc.getKeyValuePairs().get(key)));
										}
										editElement.addKeyValuePair(OWNER, targetElement);
									}
									catch (ModelElementNotFoundException e) {
										
									}
								}
							}
						}
					}
				}
		}
		return editList;
	}
		
	private List<Alternative> parseEditChange(final EObject editedElement, final GrammarBlock editBlock) {
		List<Alternative> editAlternatives = new ArrayList<>();
		if (editedElement instanceof StereotypeApplication) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "<<edit>> can't edit StereotypeApplications!"));
			return editAlternatives;
		}
		for (GrammarAlternative editAlt : editBlock.getAlternatives()) {
			List<DeltaElement> altContent = new ArrayList<>();
			for (ElementDescription desc : editAlt.getDescriptions()) {
				if (desc instanceof SimpleElementDescription) {
	 				SimpleElementDescription sed = (SimpleElementDescription) desc;
					if (editedElement instanceof Association
							&& ((sed.getKeyValuePairs().containsKey("source") 
									|| sed.getKeyValuePairs().containsKey("target")))) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Can't <<edit>> Association ends."));
						continue;
					}
					EditElement editElement = new EditElement(editedElement);
					editElement.replaceValues(((SimpleElementDescription) desc).getKeyValuePairs());
					altContent.add(editElement);
				} 
			}
			if (!altContent.isEmpty()) {
				Alternative alt = new Alternative();
				alt.replaceDeltaElements(altContent);
				editAlternatives.add(alt);				
			}
		}
		return editAlternatives;
	}

	private List<Alternative> parseMoveChange(final EObject movedElement, final GrammarBlock toBlock) {
		List<Alternative> toAlternatives = new ArrayList<>();
		if (movedElement instanceof Association) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Associations cannot be moved!"));
			return toAlternatives;
		}
		for (GrammarAlternative toAlt : toBlock.getAlternatives()) {
			if (toAlt.getDescriptions().size() > 1) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Too many targets for <<move>>. The following are ignored: "));
				for (ElementDescription desc : toAlt.getDescriptions()) {
					Logger.log(LogLevel.ERROR, desc.getGrammarString());
				}
			}
			SimpleElementDescription sed = (SimpleElementDescription) toAlt.getDescriptions().get(0);
			String newOwnerName = sed.getMetaclassName();
			Element newOwner = null;
			try {
				newOwner = UMLHelper.getElementByName(this.theModel, newOwnerName);					
			} catch (ModelElementNotFoundException e) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Can't find new owner of moved element. " + e.getMessage()));
				continue;
			}
			EditElement editElement = new EditElement(movedElement);
			editElement.replaceValues(sed.getKeyValuePairs());
			editElement.addKeyValuePair(OWNER, newOwner);
			Alternative alt = new Alternative();
			alt.addDeltaElement(editElement);
			toAlternatives.add(alt);
		}
		return toAlternatives;
	}

	
	/**
	 * gets an element that is to copy and a grammarblock with the new namespaces and values for the copys and creates all alternativ changes.
	 * @param copiedElement - the Element to copy
	 * @param toBlock - GrammarBlock with the values of the to-Tag
	 * @return - alternatives
	 */
	private List<Alternative> parseCopyChange(final EObject copiedElement, final GrammarBlock toBlock) {
		List<Alternative> toAlternatives = new ArrayList<>();
		if (copiedElement instanceof Association) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Associations cannot be copied!"));
			return toAlternatives;
		}
		EObject originalParent = copiedElement.eContainer();
		if (originalParent == null) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Can't copy "
		+ EObjectUtil.getTypeAndName(copiedElement) + "." + " Element not integrated into model (parent = null)."));
			return toAlternatives;
		}
		String oldName = UMLHelper.getName(copiedElement);
		for (GrammarAlternative grammarAlt : toBlock.getAlternatives()) {
			List<DeltaElement> altContent = new ArrayList<>();
			for (ElementDescription desc : grammarAlt.getDescriptions()) {
				if (desc instanceof SimpleElementDescription) {
					SimpleElementDescription copyDesc = (SimpleElementDescription) desc;
					Element receivingTarget = null;
					try {
						receivingTarget = UMLHelper.getElementByName(this.theModel, copyDesc.getMetaclassName());
					} catch (ModelElementNotFoundException e) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
								"Couldn't find receiving element " 
										+ copyDesc.getMetaclassName() 
										+ " of copy of " 
										+ EObjectUtil.getTypeAndName(copiedElement)));
						continue;
					}
					if (copiedElement instanceof NamedElement) {
						String newName = ((SimpleElementDescription) desc).getKeyValuePairs().get(NAME);
						boolean sameName = false;
						if ((newName == null) || (newName.equals(oldName))) {
							sameName = true;
						}
						if ((originalParent.equals(receivingTarget)) && sameName) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, 
									"Tried to copy element to same namespace without changing the name: " 
									+ EObjectUtil.getTypeAndName(copiedElement)));
							continue;
						}
					}
					CopyElement copyElement = new CopyElement(copiedElement, receivingTarget);
					copyElement.replaceChangedValues(copyDesc.getKeyValuePairs());
					altContent.add(copyElement);
				}
			}
			if (!altContent.isEmpty()) {
				Alternative alt = new Alternative();
				alt.replaceDeltaElements(altContent);
				toAlternatives.add(alt);
			}
		}
		return toAlternatives;
	}

	private static Alternative parseDelAllChange(List<EObject> realMatchingElements) {
		Alternative delAllAlternative = new Alternative();
		if (realMatchingElements != null) {
			for (EObject realMatch : realMatchingElements) {
				DelElement del = new DelElement(realMatch);
				del.getAccompanyingDeletions().addAll((collectDeletedElements(realMatch, UMLchange.DEL)));				
				delAllAlternative.addDeltaElement(del);
			}
		}
		return delAllAlternative;
	}
	
	private List<Alternative> parseAddAllChange(final List<EObject> realMatchingElements, final GrammarBlock newBlock) {
		List<Alternative> newAlternatives = new ArrayList<>();
		for (GrammarAlternative newAlt : newBlock.getAlternatives()) {
			List<DeltaElement> altContent = new ArrayList<>();
			for (EObject realMatch : realMatchingElements) {
				for (ElementDescription desc : newAlt.getDescriptions()) {
					if (desc instanceof SimpleElementDescription) {
						altContent.addAll(parseSimpleElementDescriptions(
							(SimpleElementDescription) desc, realMatch));									
					} else if (desc instanceof NamespaceDescription) {
						NamespaceDescription nsDesc = (NamespaceDescription) desc;
						Namespace complexNamespace = null;
						try {
							complexNamespace = UMLHelper.getElementOfNameAndType(this.theModel, nsDesc.getNamespaceName(), Namespace.class);
						} catch (ModelElementNotFoundException e) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, e.getMessage()));
							continue;
						}
						for (Element oldMarkedElement : UMLchangeUtil.getStereotypedElements(UMLchange.OLD, complexNamespace)) {
							UMLHelper.unapplyStereotype(oldMarkedElement, "UMLchange::old");
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Ignored <<old>> element: "
							+ EObjectUtil.getTypeAndName(oldMarkedElement)));
						}
						altContent.addAll(parseNamespace(complexNamespace, realMatch));
					}
				}
			}
			if (!altContent.isEmpty()) {
				Alternative alt = new Alternative();
				alt.replaceDeltaElements(altContent);
				newAlternatives.add(alt);					
			}
		}
		return newAlternatives;
	}

	private List<Alternative> parseSubstAllChange(final List<EObject> realMatchingElements,
			final GrammarBlock newBlock, final String refValue) {
		List<Alternative> newAlternatives = new ArrayList<>();
		int subAlternativeCount = 0;
		int subAlternativeSize = newBlock.getAlternatives().size();
		for (GrammarAlternative newAlt : newBlock.getAlternatives()) {
			subAlternativeCount++;
			List<DeltaElement> altContent = new ArrayList<>();
			for (EObject realMatch : realMatchingElements) {
				EObject substituteOwner = determineSubstituteOwner(realMatch);
				List<AddElement> substitutes = new ArrayList<>();
				for (ElementDescription desc : newAlt.getDescriptions()) {
					if (desc instanceof SimpleElementDescription) {
						substitutes.addAll(parseSimpleElementDescriptions(
								(SimpleElementDescription) desc, substituteOwner));
						if (!substitutes.isEmpty()) {
							SubstElement substElem = new SubstElement(realMatch, substitutes);
							substElem.getAccompanyingDeletions().addAll(
								((collectDeletedElements(realMatch,UMLchange.SUBST))));								
							altContent.add(substElem);
						}	
					} else if (desc instanceof NamespaceDescription) {
						NamespaceDescription nsDesc = (NamespaceDescription) desc;
						Namespace complexNamespace = null;
						try {
							complexNamespace = UMLHelper.getElementOfNameAndType(this.theModel, nsDesc.getNamespaceName(), Namespace.class);
						} catch (ModelElementNotFoundException e) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, e.getMessage()));
							continue;
						}
						for (Element oldMarkedElement : UMLchangeUtil.getStereotypedElements(UMLchange.OLD, complexNamespace)) {
							UMLHelper.unapplyStereotype(oldMarkedElement, "UMLchange::old");
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Ignored <<old>> element: " + EObjectUtil.getTypeAndName(oldMarkedElement)));
						}
						DelElement del = new DelElement(realMatch);
						del.getAccompanyingDeletions().addAll(((collectDeletedElements(realMatch, UMLchange.SUBST))));
						altContent.add(del);
						altContent.addAll(parseNamespace(complexNamespace, substituteOwner));
						if (realMatch instanceof NamedElement) {
							altContent.addAll(generateEditsOfKeep((NamedElement) realMatch, subAlternativeCount, refValue,
									subAlternativeSize, ((NamespaceDescription) desc).getNamespaceName()));
						}
					}
				}
			}
			if (!altContent.isEmpty()) {
				Alternative alt = new Alternative();
				alt.replaceDeltaElements(altContent);
				newAlternatives.add(alt);				
			}
		}
		return newAlternatives;
	}

	/**
	 * 	
	 * gets all the objects from the given MetaClass that are a child of the given object (direct or indirect)
	 * to witch fits the pattern stored in the description.
	 * @param scope - the parent of all objects
	 * @param patternDescription - the pattern description
	 * @return - all the objects that fits
	 */
	private List<Element> getPatternMatches(final Element scope, final SimpleElementDescription patternDescription) {
		List<Element> matchingElements = new ArrayList<>();
		try {

			if (patternDescription.getMetaclassName().equals("Stereotype")) {
				for (Element element : UMLHelper.getAllElementsOfType(scope, Element.class)) {
					for (StereotypeApplication stereoApp : UMLHelper.getStereotypeApplications(element)) {
						if (checkPatternForStereotype(stereoApp, patternDescription)) {
							matchingElements.add(element);
							break;
						}
					}
				}
				
			} else if (patternDescription.getMetaclassName().equals("TaggedValue")) {
				for (Element element : UMLHelper.getAllElementsOfType(scope, Element.class)) {
					AllStereo:
					for (StereotypeApplication stereo : UMLHelper.getStereotypeApplications(element)) {
						for (TaggedValue tag : stereo.getTaggedValues()) {
							if (checkPatternForTaggedValue(tag, patternDescription)) {
								matchingElements.add(element);
								break AllStereo;
							}
						}
					}
				}
			} else {
				EClass wantedMetaclass = UMLHelper.getMetaClass(patternDescription.getMetaclassName());
				for (Element element : UMLHelper.getAllElementsOfType(scope, wantedMetaclass)) {
					if (matchesPattern(element, patternDescription)) {
						matchingElements.add(element);
					}
				}
			}
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
		}
		return matchingElements;
	}
	
	
	private static EObject determineSubstituteOwner(final EObject changedElement) {
		if (changedElement instanceof Element) {
			return ((Element) changedElement).getOwner();
		} else if (changedElement instanceof StereotypeApplication) {
			return ((StereotypeApplication) changedElement).getExtendedElement();
		} else if (changedElement instanceof TaggedValue) {
			return ((TaggedValue) changedElement).getCorrespondingApplication();
		}
		return null;
	}

//FIXME: Javadoc wrong
	/**
	 * Checks if a given object matches the given pattern.
	 * @param candidate - the candidate object
	 * @param pattern - SimpleElementDescription with keyValuePairs that the candidate has to match
	 * @return - true if the object should be deleted, false otherwise
	 */
//FIXME: look at the method
	private boolean matchesPattern(final Element candidate, final SimpleElementDescription pattern) {
		Map< String, String> patternValues = pattern.getKeyValuePairs();
		for (Entry<String, String> property : patternValues.entrySet()) {
			EStructuralFeature sf = candidate.eClass().getEStructuralFeature(property.getKey());
			
			if (property.getKey().equals("visibility")) {
				VisibilityKind vk = (VisibilityKind) candidate.eGet(sf);
				if (!property.getValue().equals(vk.toString())) {
					return false;
				}
			} else if (property.getKey().equals(NAME)) {
				String value = property.getValue();
				String original = (String) candidate.eGet(sf);

				if (!original.matches(value.replaceAll(ASTERISK, ".*"))) {
					return false;
				}

			} else if (sf != null && candidate.eGet(sf) != null && !property.getValue().equals(candidate.eGet(sf))) {
				return false;
			}
		}
		for (SimpleElementDescription subPattern : pattern.getContainedElements()) {
			String metaClass = subPattern.getMetaclassName();
			if (metaClass.equals("Stereotype")) {
				for (StereotypeApplication stereotypeApplication : UMLHelper.getStereotypeApplications(candidate)) {
					if ((!UMLchange.contains(stereotypeApplication.getAppliedStereotype()))
							&& checkPatternForStereotype(stereotypeApplication, subPattern)) {
						return true;
					}
				}
			} else if (metaClass.equals("TaggedValue")) {
				for (StereotypeApplication stereoApp : UMLHelper.getStereotypeApplications(candidate)) {
					if (!UMLchange.contains(stereoApp.getAppliedStereotype())) {
						for (TaggedValue tag : stereoApp.getTaggedValues()) {
							if (checkPatternForTaggedValue(tag, subPattern)) {
								return true;
							}
						}
					}
				}
			} else {
				try {
					EClass mc = UMLHelper.getMetaClass(metaClass);
					for (Element containedElement : UMLHelper.getAllElementsOfType(candidate, mc)) {
						if (matchesPattern(containedElement, subPattern)) {
							return true;
						}
					}
				} catch (InvalidMetaclassException e) {
					Logger.log(LogLevel.ERROR, e.getMessage(), e);
				}
			}
			return false;
		}
		return true;
	}
	
	
	
	/**
	 * checks if the given StereotypeApplication fits the pattern.
	 * @param stereoApp - the StereotpeApplication that may fits the pattern
	 * @param pattern - pattern to fit
	 * @return - true if the StereotypeApplication fits the pattern. false otherwise
	 */
	private static boolean checkPatternForStereotype(final StereotypeApplication stereoApp,
			final SimpleElementDescription pattern) {
		if (stereoApp.getAppliedStereotype().getProfile().getName().equals(UMLchange.DESCRIPTOR.getProfileName())) {
			return false;
		}
		// pattern: refID=Class(name=NewClass,contents=<Stereotype(name=critical,contents=<Property(name=secrecy,value=sonstwas))
		for (String key : pattern.getKeyValuePairs().keySet()) {
			if (key.equals(NAME)) {
				String stereoName = stereoApp.getStereotypeName();
				if (!(stereoName.matches(pattern.getKeyValuePairs().get(key).replaceAll(ASTERISK, ".*")))) {
					return false;
				}
			}
		}

		for (SimpleElementDescription tagPattern : pattern.getContainedElements()) {
			boolean hasTag = false;
			for (TaggedValue tag : stereoApp.getTaggedValues()) {
				if (checkPatternForTaggedValue(tag, tagPattern)) {
					hasTag = true;
					break;
				}
			}
			if (!hasTag) {
				return false;
			}
		}
		return true;
	}
	
	/** Tests a TaggedValue if it contains the tag described in the SimpleElementDescription.
	 * 
	 * @param tag a TaggedValue.
	 * @param pattern ElementDescription describing a Tag
	 * @return  true if the Tag fits the description in the SimpleElementDescription.
	 * <br>false if the tag belongs to an UMLChange Stereotype
	 */
	private static boolean checkPatternForTaggedValue(final TaggedValue tag, final SimpleElementDescription pattern) {
		if (tag.getStereotype().getProfile().getName().equals(UMLchange.DESCRIPTOR.getProfileName())) {
			return false;
		}
		if (pattern.getKeyValuePairs().containsKey(NAME)) {
			String patternName = pattern.getKeyValuePairs().get(NAME).replaceAll(ASTERISK, ".*");
			if (!tag.getName().matches(patternName)) {
				return false;
			}
		}
		if (pattern.getKeyValuePairs().containsKey(VALUE)) {
			String patternValue = pattern.getKeyValuePairs().get(VALUE).replaceAll(ASTERISK, ".*");
			for (String localValue : tag.getStringValues()) {
				if (localValue.matches(patternValue)) {
					return true;
				}
			}
			return false;
		}
		return true;
	}

	/**
	 * Finds the changed element with the given element and value of the
	 * ext tag. If ext is empty, the element is the changed one. If ext contains
	 * a stereotype name, that is the changed element. If ext contains a stereotype
	 * followed by a tag name, the tag is the changed element.
	 * @param extValue - the value of ext
	 * @param changeParent - the parent of the UMLchange stereotype
	 * @return - the changed element
	 */
	private static EObject determineChangedElement(final String extValue, final Element changeParent) throws ModelElementNotFoundException {
		if (changeParent == null) {
			throw new IllegalArgumentException("Can't determine changed element with a null element.");
		}
		if (extValue.equals("")) {
			return changeParent;
		}
		Pattern namePattern = Pattern.compile("(?<!\\w)[\\w\\ -]+(?!\\w)");
		Matcher nameMatcher = namePattern.matcher(extValue);
		StereotypeApplication foundApp = null;
		TaggedValue changedTag = null;
		String changedStereoName = "";
		String changedTagName = "";
		if (nameMatcher.find()) {
			changedStereoName = nameMatcher.group();
			foundApp = UMLHelper.getStereotypeApplication(changeParent, changedStereoName);
			if (foundApp == null) {
				throw new ModelElementNotFoundException("Couldn't find application of stereotype " + changedStereoName);
			}
			if (nameMatcher.find()) {
				changedTagName = nameMatcher.group();
				changedTag = foundApp.getTaggedValue(changedTagName);
				if (changedTag == null) {
					throw new ModelElementNotFoundException("Couldn't find tag " + changedTagName + " at " + foundApp);										
				}
				return changedTag;
			}
			return foundApp;
		}
		throw new IllegalArgumentException("Couldn't determine changed element. Ext value: " + extValue);
	}
	
	/**
	 * This method collects the elements deleted
	 * when the target element is deleted.
	 * This entails all elements owned directly or indirectly
	 * by the target element, their connections and their
	 * in/directly owned elements.
	 * @param deletedElement - Element that will be deleted
	 * @return - all deleted elements
	 */
	private static Set<EObject> collectDeletedElements(final EObject deletedElement, final UMLchange type) {
		Set<EObject> deletedElements = new HashSet<>();
		if (deletedElement instanceof StereotypeApplication) {
			StereotypeApplication deletedApp = (StereotypeApplication) deletedElement;
			deletedElements.addAll(deletedApp.getTaggedValues());
		} else if (deletedElement instanceof Element) {
			Element topElement = (Element) deletedElement;
// all directly and indirectly contained elements
			List<Element> topOwnedElements = new ArrayList<>(topElement.allOwnedElements());
			
			if (type == UMLchange.SUBST) {
// SUBST: except all owned Elements of an Element which has the <<keep>> stereotype applied to.
				List<Element> keptElements = new ArrayList<>();
				for (Element ele : topOwnedElements) {
					if (UMLchangeUtil.hasStereotype(UMLchange.KEEP, ele)) {
						keptElements.add(ele);
					}
				}
				for (Element keptElement : keptElements) { 
					topOwnedElements.removeAll(keptElement.allOwnedElements());
					topOwnedElements.remove(keptElement);
				}
			}
			
			deletedElements.addAll(topOwnedElements);
			topOwnedElements.add(topElement);
// their dependencies
			List<Dependency> allDependenciesInModel = UMLHelper.getAllElementsOfType(topElement.getModel(), Dependency.class);
			for (Dependency dependency : allDependenciesInModel) {
				if (!topOwnedElements.contains(dependency)) {
					for (NamedElement client : dependency.getClients()) {
						if (topOwnedElements.contains(client)) {
							deletedElements.add(dependency);
							deletedElements.addAll(dependency.allOwnedElements());
						}
					}
					for (NamedElement supplier : dependency.getSuppliers()) {
						if (topOwnedElements.contains(supplier)) {
							deletedElements.add(dependency);
							deletedElements.addAll(dependency.allOwnedElements());
						}
					}
				}
			}
// all associations
			List<Association> allAssociationsInModel = UMLHelper.getAllElementsOfType(topElement.getModel(), Association.class);
			for (Association assoc : allAssociationsInModel) {
				if (!topOwnedElements.contains(assoc)) {
					for (Property memberEnd : assoc.getMemberEnds()) {
						if (topOwnedElements.contains(memberEnd.getType())) {
							deletedElements.add(assoc);
							deletedElements.addAll(assoc.allOwnedElements());
						}
					}
				}				
			}
// all StereotypeApplications and their TaggedValues
			Set<EObject> deletedExtensions =
				new HashSet<>();
			for (EObject deletedElem : deletedElements) {
				if (deletedElem instanceof Element) {
					Element delElem = (Element) deletedElem;
					for (StereotypeApplication deletedApp : UMLHelper.getStereotypeApplications(delElem)) {
						if (!UMLchange.contains(deletedApp.getAppliedStereotype())) {
							deletedExtensions.add(deletedApp);
							for (TaggedValue deletedValue : deletedApp.getTaggedValues()) {
								if (deletedValue.hasValue()) {
									deletedExtensions.add(deletedValue);
								}
							}
						}
					}
				}
			}
			for (StereotypeApplication deletedApp : UMLHelper.getStereotypeApplications(topElement)) {
				if (!UMLchange.contains(deletedApp.getAppliedStereotype())) {
					deletedExtensions.add(deletedApp);
					for (TaggedValue deletedValue : deletedApp.getTaggedValues()) {
						if (deletedValue.hasValue()) {
							deletedExtensions.add(deletedValue);
						}
					}
				}
			}
			deletedElements.addAll(deletedExtensions);
		}
		return deletedElements;
	}
	
	/**
	 * Creates the evolution DeltaElements from ElementDescriptions,
	 * given the ChangeType and changed element.
	 * @param description - description of new elements
	 * @param targetElement - the element to change
	 * @return - list of DeltaElements
	 */
	private List<AddElement> parseSimpleElementDescriptions(
			final SimpleElementDescription description,
			final EObject targetElement) {
		List<AddElement> addElements = new ArrayList<>();
		if (targetElement != null && description != null) {
			String metaclassname = description.getMetaclassName();
			try {
				AddElement addElem = new AddElement(targetElement, UMLHelper.getMetaClass(metaclassname), null);
				if (description.getKeyValuePairs() != null) {
					addElem.replaceValues(description.getKeyValuePairs());
				}
				addElem.addContainedElements(createAddContent(description, addElem));
				addElements.add(addElem);
			} catch (IllegalArgumentException e) {
			    Logger.log(LogLevel.ERROR, e.getMessage(), e);
			} catch (InvalidMetaclassException e) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, e.getMessage()));
			}
		}
		return addElements;
	}
	
	// TODO: Mehrere Namespaces mit gleichem Namen
	private List<DeltaElement> parseNamespace(
			final Namespace complexNamespace,
			final EObject ownerOfNewElements) {
		List<DeltaElement> deltaElements = new ArrayList<>();
// NS muss immer das Mutter-Paket enthalten (geht das immer?)
		if (complexNamespace != null) {
			Set<EObject> outermostNewElements = getOutermostNewElements(complexNamespace);
			for (EObject newElem : outermostNewElements) {
				if (newElem instanceof Element) {
					Element newElement = (Element) newElem;
					if (!UMLHelper.isConnection(newElement)) {
						EditElement editOwner = new EditElement(newElement);
						if (UMLchangeUtil.hasStereotype(UMLchange.OLD, newElement.getOwner())) {
							if (newElement.getOwner() instanceof NamedElement) {
								NamedElement oldMarkedOwner = (NamedElement) newElement.getOwner();
//TODO: We should handle elements that seem to be new (modeled in namespace, no <<old>>), but already exist in the original model
								NamedElement originalElement = this.oldMarkedOriginalElementMapping.get(oldMarkedOwner);
								if (originalElement != null && !originalElement.equals(oldMarkedOwner)) {
									editOwner.addKeyValuePair(OWNER, originalElement);
								}
// FIXME: What if there is no original element?
							}
						} else if (ownerOfNewElements instanceof NamedElement) {
							NamedElement originalElement = (NamedElement) ownerOfNewElements;
							editOwner.addKeyValuePair(OWNER, originalElement);
						}
						if (!editOwner.getValues().isEmpty()) {
							deltaElements.add(editOwner);
						}
					} else {
// FIXME: We don't allow (or support) edits of associations
						EditElement editConnection = createConnectionEdit(newElement);
						if (editConnection != null) {
							deltaElements.add(editConnection);
						}
					}
				} else if (newElem instanceof StereotypeApplication && ownerOfNewElements instanceof NamedElement) {
					deltaElements.addAll(createExtensionAdds((StereotypeApplication) newElem, (NamedElement) ownerOfNewElements));
				}
			}
			if (ownerOfNewElements instanceof NamedElement) {
				for (StereotypeApplication newApplication : getNewApplications(complexNamespace)) {
					deltaElements.addAll(createExtensionAdds(newApplication, (NamedElement) ownerOfNewElements));
				}
			}
		}
		return deltaElements;
	}
	
	private List<AddElement> createExtensionAdds(
			final StereotypeApplication newApplication,
			final NamedElement targetElement) {
		List<AddElement> extensionAdds = new ArrayList<>();
		if (newApplication.getExtendedElement() instanceof NamedElement) {
			NamedElement namedOldElement = (NamedElement) newApplication.getExtendedElement();
			NamedElement originalElement = null;
			if (UMLchangeUtil.hasStereotype(UMLchange.OLD, namedOldElement)) {
				originalElement = this.oldMarkedOriginalElementMapping.get(namedOldElement);
			} else if (targetElement != null) {
				originalElement = targetElement;
			}
			if (originalElement != null && !originalElement.equals(namedOldElement)) {
				Stereotype appliedStereo = newApplication.getAppliedStereotype();
				StereotypeApplication originalApp = null;
				AddElement addStereotype = null;
				if (!originalElement.isStereotypeApplied(appliedStereo)) {
					addStereotype = new AddElement(originalElement, appliedStereo.eClass(), null);
					addStereotype.addKeyValuePair(NAME, appliedStereo.getQualifiedName());
					extensionAdds.add(addStereotype);
					
				} else {
					originalApp = UMLHelper.getStereotypeApplication(originalElement, appliedStereo.getName());
				}
				for (TaggedValue newTagValue : newApplication.getTaggedValues()) {
					if (newTagValue.hasValue()) {
						Property tag = newTagValue.getTag();
						Object tagValue = newTagValue.getValue();
						AddElement addTagValue = null;
						if (originalApp != null) {
							addTagValue = new AddElement(originalApp, tag.eClass(), null);
							extensionAdds.add(addTagValue);													
						} else {
							addTagValue = new AddElement(null, tag.eClass(), addStereotype);
							
							//TODO: Don't use null check of originalApp to check whether addStereotype is initialized
							addStereotype.addContainedElement(addTagValue);
						}
						addTagValue.addKeyValuePair(NAME, tag.getName());
						addTagValue.addKeyValuePair(VALUE, tagValue);
					}
				}
			}
		}		
		return extensionAdds;
	}
	private Set<EObject> getOutermostNewElements(final Namespace ns) {
		Set<EObject> outermostNewElements = new HashSet<>();
		List<Element> processedConnections = new ArrayList<>();
		for (Element nsElem : ns.getOwnedElements()) {
			if (!UMLHelper.isConnection(nsElem)) {
				if (UMLchangeUtil.hasStereotype(UMLchange.OLD, nsElem)) {
					if (nsElem instanceof Namespace) {
						outermostNewElements.addAll(getOutermostNewElements((Namespace) nsElem));
					}
					outermostNewElements.addAll(getNewApplications(nsElem));
				} else {
					outermostNewElements.add(nsElem);
				}
				for (Element connection : UMLHelper.getAllConnections(nsElem)) {
					if (!processedConnections.contains(connection)) {
						if (UMLchangeUtil.hasStereotype(UMLchange.OLD, connection)) {
							outermostNewElements.addAll(getNewApplications(connection));
						} else {
							outermostNewElements.add(connection);
						}
					}
					processedConnections.add(connection);
				}
			}
		}
		return outermostNewElements;
	}
	
	/**
	 * Returns all applications marking the element marked <<old>> which are not UMLchange applications.
	 * These applications are seen as new applications (because they don't have to be modeled in the
	 * complex namespace).
	 * @param oldElement - the element marked with <<old>>
	 * @return - all new stereotype applications
	 */
	private static List<StereotypeApplication> getNewApplications(final Element oldElement) {
// FIXME: What about old applications with new tag values? (getExtensionAdds does something like it)
		List<StereotypeApplication> newApplications = new ArrayList<>();
		for (Stereotype appliedStereo : oldElement.getAppliedStereotypes()) {
			if (!UMLchange.contains(appliedStereo)) {
				StereotypeApplication newApplication = new StereotypeApplication(appliedStereo, oldElement);
				newApplications.add(newApplication);
			}
		}
		return newApplications;
	}

//FIXME: don't allow
	private EditElement createConnectionEdit(final Element newConnection) {
		EditElement connEdit = new EditElement(newConnection);
		if (newConnection instanceof Association) {
			Association newAssoc = (Association) newConnection; 
			int endNr = 1;
			for (Property memberEnd : newAssoc.getMemberEnds()) {
				Type endElement = memberEnd.getType();
				if (UMLchangeUtil.hasStereotype(UMLchange.OLD, endElement)) {
					NamedElement originalElement = this.oldMarkedOriginalElementMapping.get(endElement);
					// Variante: "source" "ALT;;NEU"
					// "sourceKind" "ID;;AggregationKind"
					if (originalElement != null && !originalElement.equals(endElement)) {
						connEdit.addKeyValuePair("end" + endNr, endElement.getQualifiedName() + ";;" + originalElement.getQualifiedName());
						endNr++;
					}
				}
			}
		} else if (newConnection instanceof Dependency) {
			Dependency newDependency = (Dependency) newConnection;
			for (NamedElement client : newDependency.getClients()) {
				if (UMLchangeUtil.hasStereotype(UMLchange.OLD, client)) {
					NamedElement originalElement = this.oldMarkedOriginalElementMapping.get(client);
					if (originalElement != null && originalElement != client) {
						connEdit.addKeyValuePair("client", originalElement);					
					}
				}
			}
			for (NamedElement supplier : newDependency.getSuppliers()) {
				if (UMLchangeUtil.hasStereotype(UMLchange.OLD, supplier)) {
					NamedElement originalElement = this.oldMarkedOriginalElementMapping.get(supplier);
					if (originalElement != null && originalElement != supplier) {
						connEdit.addKeyValuePair("supplier", originalElement);					
					}
				}
			}
		} else if (newConnection instanceof Transition) {
			Transition newTransition = (Transition) newConnection;
			Vertex source = newTransition.getSource();
			if (UMLchangeUtil.hasStereotype(UMLchange.OLD, source)) {
				NamedElement originalElement = this.oldMarkedOriginalElementMapping.get(source);
				if (originalElement != null && !originalElement.equals(source)) {
					connEdit.addKeyValuePair("source", originalElement);
				}
			}
			Vertex target = newTransition.getTarget();
			if (UMLchangeUtil.hasStereotype(UMLchange.OLD, target)) {
				NamedElement originalElement = this.oldMarkedOriginalElementMapping.get(target);
				if (originalElement != null && !originalElement.equals(target)) {
					connEdit.addKeyValuePair("target", originalElement);
				}
			}
		}
		if (!connEdit.getValues().isEmpty()) {
			return connEdit;
		}
		return null;
	}
	
	
	/**
	 * creates an AddElement with a null target and type and values of the given Element.
	 * @param sourceElement - Element to copy
	 * @param keep - boolean if <<keep>> stereotyped elements should be recognized or not
	 * @return - AddElement
	 */
//FIXME: Look at the method
//	private AddElement createAddElementStructure(final EObject sourceElement, final boolean keep) {
//		if (sourceElement == null) {
//			//TODO Exception or something?
//			return null;
//		}
//		AddElement sourceAsAdd = null;
//		if (sourceElement instanceof StereotypeApplication) {
//			StereotypeApplication stapp = (StereotypeApplication) sourceElement;
//			if (!UMLchange.contains(stapp.getAppliedStereotype())) {
//				sourceAsAdd = new AddElement(null, sourceElement.eClass(), null);
//				sourceAsAdd.addKeyValuePair(NAME, stapp.getAppliedStereotype().getQualifiedName());
//				for (TaggedValue taggedValue : ((StereotypeApplication) sourceElement).getTaggedValues()) {
//					sourceAsAdd.addContainedElement(createAddElementStructure(taggedValue, keep));
//				}
//			}
//		} else if (sourceElement instanceof TaggedValue) {
//			sourceAsAdd = new AddElement(null, sourceElement.eClass(), null);
//			TaggedValue tv = (TaggedValue) sourceElement;
//			sourceAsAdd.addKeyValuePair(NAME, tv.getName());
//			sourceAsAdd.addKeyValuePair(VALUE, tv.getValue());
//		} else {
//			sourceAsAdd = new AddElement(null, sourceElement.eClass(), null);
//			for (EStructuralFeature sf : sourceElement.eClass().getEAllStructuralFeatures()) {
//				if ((sourceElement.eIsSet(sf)) && !(sf.isDerived())) {
//					if (sf instanceof EAttribute) {
//						sourceAsAdd.addKeyValuePair(sf.getName(), sourceElement.eGet(sf));
//					}
//					if (sf instanceof EReference
//							&& sf.getName().equals("type")) {
//						sourceAsAdd.addKeyValuePair(sf.getName(), sourceElement.eGet(sf));							
//					}
//				}
//			}
//			Element sourceUMLElement = (Element) sourceElement;
//			for (Element element : (sourceUMLElement.getOwnedElements())) {
//				if (!((keep) && (UMLchangeUtil.hasStereotype(UMLchange.KEEP, element)))) {
//					sourceAsAdd.addContainedElement(createAddElementStructure(element, keep));
//				}
//			}
//			for (StereotypeApplication stapp : UMLHelper.getStereotypeApplications(sourceUMLElement)) {
//				if (!UMLchange.contains(stapp.getAppliedStereotype())) {
//					sourceAsAdd.addContainedElement(createAddElementStructure(stapp, keep));
//				}
//			}
//		}
//		return sourceAsAdd;
//	}

	/**
	 * Translates the UMLchange grammar block
	 * descriptions to delta elements and creates
	 * the content of the delta element.
	 * @param containerSimpleDesc - description of the container delta element
	 * @param containerAdditiveElement - the containing delta element
	 * @return - the content for the delta element
	 */
	private List<AddElement> createAddContent(
			final SimpleElementDescription containerSimpleDesc,
			final AdditiveElement containerAdditiveElement) {
		List<AddElement> content = new ArrayList<>();
			for (SimpleElementDescription contentDesc : containerSimpleDesc.getContainedElements()) {
				try {
					String metaclassname = contentDesc.getMetaclassName();
					AddElement containedAddedElement =
						new AddElement(null,
								UMLHelper.getMetaClass(metaclassname),
								containerAdditiveElement);
					containedAddedElement.replaceValues(contentDesc.getKeyValuePairs());
					content.add(containedAddedElement);
					containedAddedElement.addContainedElements(createAddContent(contentDesc, containedAddedElement));
				} catch (InvalidMetaclassException e) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, e.getMessage()));
				}
			}
		return content;
	}

}
