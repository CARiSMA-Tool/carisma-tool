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
package carisma.check.staticcheck.guardedaccess;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Behavior;
import org.eclipse.uml2.uml.BehavioralFeature;
import org.eclipse.uml2.uml.Constraint;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.OpaqueBehavior;
import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.Transition;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;

/**
 * analyzes a model with respect to guard access.
 * @author Johannes Kowald
 *
 */
public class GuardedAccessCheck implements CarismaCheck {
	
	/**
	 * The AnalysisHost.
	 */
	private AnalysisHost host;
	
	/**
	 * A map for the name (String) of the element with applied stereotype <<guarded>> and
	 * object names (String) given in the value of the tag {guard} belonging to <<guarded>>.
	 * Map<Name of element with stereotype 'guarded', Value of the tag 'guard'>.
	 */
	private Map<String, String> guardingInformations; 
	
	/**
	 * A string, which contains informations about possible violations regarding to the 
	 * rules of the stereotype <<guarded access>>.
	 */
	private String violations;
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost analysisHost) {
		this.host = analysisHost;
		this.violations = "";
		
		host.appendLineToReport("############################################################################");
		host.appendLineToReport("# UMLsec <<guarded access>> check                                          #");
		host.appendLineToReport("# All element names are adequate qualified names                           #");
		host.appendLineToReport("############################################################################");
		host.appendLineToReport("# Step 1: Searching for element with applied stereotype <<guarded access>> #");
		host.appendLineToReport("############################################################################");
		
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Resource is null"));
			host.appendLineToReport("Error: Resource is null");
			return false;
		}
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			host.appendLineToReport("Error: The model is empty");
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			Package pack = (Package) currentModel.getContents().get(0);
			guardingInformations = new HashMap<String, String>();
			return guardedAccess(pack);
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			host.appendLineToReport("Error: The content is not a model");
			return false;
		}
	}
	
	/**
	 * .
	 * @param pack Package
	 * @return boolean
	 */
	private boolean guardedAccess(final Package pack) {
		Stereotype stereotype = pack.getAppliedStereotype("UMLsec::guarded access");
		if (stereotype == null) {
			List<Element> stereotypeList = UMLsecUtil.getStereotypedElements(pack, UMLsec.GUARDED_ACCESS);
			if ((stereotypeList == null) || (stereotypeList.size() < 1)) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "No stereotype <<guarded access>> applied."));
				host.appendLineToReport("Error: No stereotype <<guarded access>> applied.");
			} else {
				for (Element subElement : stereotypeList) {
					GuardedAccessCheck gac = new GuardedAccessCheck();
					gac.guardedAccess((Package) subElement);
				}
			}
		} else {
			host.appendLineToReport("Stereotype <<guarded access>> found!");
			host.appendLineToReport("Stereotype <<guarded access>> is applied to the element with the name '" + pack.getName() + "'");
			host.appendLineToReport("");
			host.appendLineToReport("############################################################################");
			host.appendLineToReport("# Step 2: Searching for elements with applied stereotype <<guarded>>.      #");
			host.appendLineToReport("#         Dumping the qualified name of the element and the value of the   #");
			host.appendLineToReport("#         tag {guard} belonging to the stereotype.                         #");
			host.appendLineToReport("############################################################################");
			
			findElementsWithStereotypeGuarded(pack);
			
			host.appendLineToReport("############################################################################");
			host.appendLineToReport("# Step 3: Searching for transitions with a guard and a effect, to check    #");
			host.appendLineToReport("#         their conformity with the rules of <<guarded access>>. Dumping   #");
			host.appendLineToReport("#         all possible candidates with guard and effect                    #");
			host.appendLineToReport("############################################################################");
			
			boolean stereotypeSatisfied = analyseTransitions(pack);
			
			host.appendLineToReport("############################################################################");
			host.appendLineToReport("# Step 4: Conclusion                                                       #");
			host.appendLineToReport("############################################################################");
			
			if (stereotypeSatisfied) {
				host.appendLineToReport("The model satisfies the rules of the stereotype <<guarded access>>");
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The model satisfies the rules of the stereotype <<guarded access>>"));
			} else {
				host.appendLineToReport("The model violates the rules of the stereotype <<guarded access>>");
				host.appendLineToReport("Violations:");
				host.appendLineToReport(this.violations);
				host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "The model violates the rules of the stereotype <<guarded access>>"));
			}
			
			return stereotypeSatisfied;
		}
		host.appendLineToReport("WARNING: The check is not finished yet!");
		return true;
	}
	
	/**
	 * Searches for all elements with the stereotype <<guarded>> applied to it and saves
	 * the corresponding guard value with the element in a map.
	 * @param pack The package
	 */
	private void findElementsWithStereotypeGuarded(final Package pack) {
		int elementCounter = 0;
		
		List<Element> elementsWithGuarded = UMLsecUtil.getStereotypedElements(pack, UMLsec.GUARDED);
		if ((elementsWithGuarded != null) && (elementsWithGuarded.size() > 0)) {
			for (Element element : elementsWithGuarded) {
				if (element instanceof NamedElement) {
					NamedElement namedElement = (NamedElement) element;
					host.appendLineToReport("[" + (++elementCounter) + "] Name of the element:");
					//host.appendLineToReport("       " + namedElement.getQualifiedName());
					host.appendLineToReport("       " + UMLHelper.getAdequateQualifiedName(pack, namedElement));
					List<String> taggedValues = UMLsecUtil.getStringValues("guard", UMLsec.GUARDED, namedElement);
					if (taggedValues != null) {
						host.appendLineToReport("    Noteworthy Values:");
						if (taggedValues.size() > 0) {
							for (String tagValue : taggedValues) {
								host.appendLineToReport("       guard=" + tagValue);
								guardingInformations.put(namedElement.getName(), tagValue);
							}
						} else {
							host.appendLineToReport("       guard is empty");
							guardingInformations.put(namedElement.getName(), "");
						}
					}
					host.appendLineToReport("");
				}
			}
		} else {
			host.appendLineToReport("No elements found with applied stereotype <<guarded>>");
			host.appendLineToReport("Nothing to check!");
			host.appendLineToReport("");
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "No elements found with applied stereotype <<guarded>>"));
		}
	}
	
	/**
	 * 
	 */
	private boolean analyseTransitions(final Package pack) {
		boolean returnBool = true;
		int transitionCounter = 0;
		
		List<Transition> transitionList = UMLHelper.getAllElementsOfType(pack, Transition.class);
		if ((transitionList != null) && (transitionList.size() > 0)) {
			for (Transition transition : transitionList) {
				if ((transition.getGuard() != null) && (transition.getEffect() != null)) {
					returnBool &= analyseTransitionProperties(pack, transition, ++transitionCounter);
				}
			}
		} else {
			host.appendLineToReport("No transitions found with guard and effect");
			host.appendLineToReport("");
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "No transitions found with guard and effect"));
			return true;
		}
		
		return returnBool;
	}
	
	/**
	 * 
	 * @param transition
	 */
	private boolean analyseTransitionProperties(Package pack, Transition transition, int transitionCounter) {
		boolean returnBool = true;
		String adequateQualifiedName = "";
		
		host.appendLineToReport("[" + transitionCounter + "] Name of the transition: ");
		adequateQualifiedName = UMLHelper.getAdequateQualifiedName(pack, (NamedElement) transition);
		host.appendLineToReport("       " + adequateQualifiedName);
		
		List<String> guardBodies = analyseGuardProperties(transition.getGuard());
		String effectValue = analyseEffectProperties(pack, transition.getEffect());
		
		host.appendLineToReport("    Result:");
		
		if (effectValue != null && guardBodies != null && guardBodies.size() > 0) {
			for (String guardBody : guardBodies) {
				if (guardBody.substring(0, 4).equals("obj=")) {
					String guardBodyCut = guardBody.substring(4);
					if (guardingInformations.get(guardBodyCut) != null) { 
						if (!guardingInformations.get(guardBodyCut).equals(effectValue)) {
							host.appendLineToReport("       This transition violates the rules of the stereotype <<guarded access>>");
							host.appendLineToReport("       because:");
							host.appendLineToReport("          The element, referenced by the guard value 'obj' is stereotyped <<guarded>>");
							host.appendLineToReport("          but the method, referenced by the effect, belongs to a class, which is not");
							host.appendLineToReport("          part of the {guard} tag of the <<guarded>> stereotype.");
							this.violations += "The effect of the transition '" + adequateQualifiedName + "' violates the rules.\n "
									+ "See the result of step 3 entry [" + transitionCounter + "] for more details.\n";
							host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "The effect of the transition '"
									+ adequateQualifiedName + "' violates the rules"));
							returnBool &= false;
						} else {
							host.appendLineToReport("       This transition satisfies the rules of the stereotype <<guarded access>>");
						}
					} else {
						host.appendLineToReport("       No guarding informations found for this transition (guard='" + guardBody 
								+ "', effect='" + effectValue + "')");
					}
				}
			}
		} else {
			host.appendLineToReport("       The transition has no guard or effect, so there is no check necessary");
		}
		host.appendLineToReport("");
		return returnBool;
	}
	
	/**
	 * 
	 * @param constraint
	 */
	private List<String> analyseGuardProperties(Constraint constraint) {
		host.appendLineToReport("    Guards:");
		
		List<String> guardBodies = new ArrayList<String>();
		
		List<Element> guardList = constraint.allOwnedElements();
		if (guardList != null && guardList.size() > 0) {
			for (Element guardElement : guardList) {
				if (guardElement instanceof OpaqueExpression) {
					OpaqueExpression guard = (OpaqueExpression) guardElement;
					guardBodies.addAll(guard.getBodies());
					for (String guardBody : guard.getBodies()) {
						host.appendLineToReport("       " + guardBody);
					}
				}		
			}
		}
		
		return guardBodies;
	}
	
	/**
	 * 
	 * @param behavior
	 */
	private String analyseEffectProperties(Package pack, Behavior behavior){
		if (behavior instanceof OpaqueBehavior) {
			OpaqueBehavior opaqueBehavior = (OpaqueBehavior) behavior;
			BehavioralFeature behavioralFeature = opaqueBehavior.getSpecification();
			if (behavioralFeature != null) {
				host.appendLineToReport("    Effects:");
				host.appendLineToReport("       the following informations are about the element, which is referenced by the effect");
				//host.appendLineToReport("       Name: " + behavioralFeature.getQualifiedName());
				host.appendLineToReport("       Name: " + UMLHelper.getAdequateQualifiedName(pack, (NamedElement) behavioralFeature));
				
				// Search for parent class of the operation
				if (behavioralFeature instanceof Operation) {
					Operation operation = (Operation) behavioralFeature;
					NamedElement parentElement = (NamedElement) operation.getOwner();
					//host.appendLineToReport("       Name of the parent element: " + parentElement.getQualifiedName());
					host.appendLineToReport("       Name of the parent element: " + UMLHelper.getAdequateQualifiedName(pack, parentElement));
					return parentElement.getName();
				}
				
				// If nothing was return yet, return the Name of the BehavioralFeature as a NamedElement
				if (behavioralFeature instanceof NamedElement) {
					NamedElement namedElement = (NamedElement) behavioralFeature;
					namedElement.getName();
				}
			}
		}
		
		return null;
	}
	
}
