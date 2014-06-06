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
package carisma.check.activity2petrinet;


import org.eclipse.uml2.uml.ActivityEdge;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.ControlFlow;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.InitialNode;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.Model;

import carisma.check.activity2petrinet.petriNet.Arc;
import carisma.check.activity2petrinet.petriNet.PetriNet;
import carisma.check.activity2petrinet.petriNet.Place;
import carisma.check.activity2petrinet.petriNet.Transition;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;


/**
 * This Class contains the static method convert, which converts an UML 2 activity diagram to
 * a petri net.
 * @author Kubi Mensah
 *
 */
public class Convert {
	
	/**
	 * The loaded model. 
	 */
	private Model model;
	/**
	 * Manages the activity diagram.
	 */
	private ActivityDiagramManager man;
	
	/**
	 * host for report.
	 */
	private AnalysisHost host = new DummyHost(true);
	
	/**
	 * Constructs a new convert object with the given model.
	 * @param model the model
	 */
	public Convert(final Model model) {
		this.model = model;
		man = new ActivityDiagramManager(model);
	}
	
	/**
	 * Constructs a new convert object with the given model.
	 * @param model the model
	 * @param analysisHost host for report
	 */
	public Convert(final Model model, final AnalysisHost analysisHost) {
		this.model = model;
		man = new ActivityDiagramManager(model);
		if (analysisHost != null) {
			host = analysisHost;
		}
	}
	
	/**
	 * This method converts an uml activity diagram into a petri net.
	 * @return		 returns the converted petri net
	 */
	public final  PetriNet convert() {
		
		//initializing new petri net
        PetriNet petriNet = new PetriNet("activity_convert", "http://www.pnml.org/version-2009/grammar/pnmlcoremodel.rng");
        Element initialNode = null;
    	
        //determining the initial state of the activity diagram
        Element activity = model.getMembers().get(0);
       	while (!(activity instanceof Activity)) {
       		if (activity.getOwnedElements().size() == 0) {
       			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Found no activity"));
       			host.appendLineToReport("Found no activity");
       			return petriNet;
       		} else {
       			activity = activity.getOwnedElements().get(0);
       		}
		}
		for (Element element : activity.getOwnedElements()) {
			if (element instanceof InitialNode) {
				initialNode = (InitialNode) element;
			}
		}
//		TODO KR: ein Aktivitätsdigramm kann mehrere Initialnodes haben
//		das muss hier berücksichtigt werden, entsprechende places mit tokens erstellen
		if (initialNode == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Found no initial node"));
   			host.appendLineToReport("Found no initial node");
			return petriNet;
		}
        // start to traverse the activity diagram from the initial state, converting it into a petri net 
        convertRek(petriNet, initialNode);
		
		return petriNet;
	}
	

	/**
	 * This recursive method traverses over each element of an UML activity diagram according to  a 
	 * triple graph grammar, mapping  UML 2 activities into petri nets.
	 * @param petri The output petri net (elements are added through 'call by reference') 
	 * @param elt 	The node in the activity diagram from which the conversion will start.
	 */
	private void convertRek(final PetriNet petri, final Element elt) {
		if (elt == null) {
			return;
		} else if (man.isDecisionOrMergeNode(elt) || man.isFinalNode(elt) || man.isInitialNode(elt)) {
				String name = " ";
				ActivityNode aN = (ActivityNode) elt;
				if (aN.getName() != null) { 
					name = aN.getName(); 
				}
				boolean exists = false;
				exists = !petri.addPlace(new Place(aN.getQualifiedName(), null, name));
				if (exists) { 
					return; 
				}
				if (!man.isFinalNode(aN)) {
					for (ActivityEdge aE : aN.getOutgoings()) {
						convertRek(petri , aE);
					}
					return;
				}
		//rule 2		
		} else if (man.isAction(elt) || man.isForkNode(elt) || man.isJoinNode(elt)) {
			String name = " ";
			ActivityNode aN = (ActivityNode) elt;
			if (aN.getName() != null) { name = aN.getName(); }
			boolean exists = false;
			exists = !petri.addTransition(new Transition(aN.getQualifiedName(), null, name));
			if (exists) { return; }
			for (ActivityEdge aE : aN.getOutgoings()) {
				convertRek(petri , aE);
			}
			return;
		} else if (man.isEdge(elt)) {
			ControlFlow cf = (ControlFlow) elt;
			ActivityNode source = cf.getSource();
			
			ActivityNode target = cf.getTarget();
			    //rule 3
			if (
					   (man.isAction(source)   && man.isAction(target)) 
					|| (man.isAction(source)   && man.isForkNode(target))  
					|| (man.isAction(source)   && man.isJoinNode(target))  
					|| (man.isForkNode(source) && man.isAction(target))
					|| (man.isForkNode(source) && man.isJoinNode(target))  
					|| (man.isJoinNode(source) && man.isAction(target))
					|| (man.isJoinNode(source) && man.isForkNode(target))
					
			) {	
				String name = " ";
				if (cf.getName() != null) { name = cf.getName(); }
				boolean exists = false;
				exists = !petri.addPlace(new Place(cf.getQualifiedName() + ".1", null, name));
				if (exists) { return; }
				petri.addArc(new Arc(cf.getQualifiedName() + ".0", source.getQualifiedName(), cf.getQualifiedName() + ".1"));
				petri.addArc(new Arc(cf.getQualifiedName() + ".2", cf.getQualifiedName() + ".1", target.getQualifiedName()));
				convertRek(petri, target);
				return;
			}
			    //rule 4
			if (
				   (man.isInitialNode(source) && man.isDecisionOrMergeNode(target))
				|| (man.isDecisionOrMergeNode(source) && man.isDecisionOrMergeNode(target))
				|| (man.isDecisionOrMergeNode(source) && man.isFinalNode(target))
			) {
				String name = " ";
				if (cf.getName() != null) { name = cf.getName(); }
				boolean exists = false;
				exists = !petri.addTransition(new Transition(cf.getQualifiedName() + ".1", null, name));
				if (exists) { return; }
				petri.addArc(new Arc(cf.getQualifiedName() + ".0", source.getQualifiedName(), cf.getQualifiedName() + ".1"));
				petri.addArc(new Arc(cf.getQualifiedName() + ".2", cf.getQualifiedName() + ".1", target.getQualifiedName()));
				convertRek(petri, target);
				return;
			}
				//rule 5 
			if (
				   (man.isAction(source)   && man.isFinalNode(target))
				|| (man.isAction(source)   && man.isDecisionOrMergeNode(target))
				|| (man.isForkNode(source) && man.isFinalNode(target))
				|| (man.isForkNode(source) && man.isDecisionOrMergeNode(target))
				|| (man.isJoinNode(source) && man.isFinalNode(target))
				|| (man.isJoinNode(source) && man.isDecisionOrMergeNode(target))
				||		
				
				//rule 6
				   (man.isInitialNode(source)         && man.isAction(target))
				|| (man.isInitialNode(source)         && man.isForkNode(target))
				|| (man.isDecisionOrMergeNode(source) && man.isAction(target))
				|| (man.isDecisionOrMergeNode(source) && man.isForkNode(target))
				|| (man.isDecisionOrMergeNode(source) && man.isJoinNode(target))
			) {
				boolean exists = false;
				exists = !petri.addArc(new Arc(cf.getQualifiedName(), source.getQualifiedName(), target.getQualifiedName()));
				if (exists) { return; }
				convertRek(petri, target);
				return;
			}
			
		} else {
			return;
		}
		
	}
	
}
