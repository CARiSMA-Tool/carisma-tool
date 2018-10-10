package carisma.check.staticcheck.nodownflow;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Transition;
import org.eclipse.uml2.uml.Trigger;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


/**
 * This class analyzes an UML-diagram with respect to nodownflow.
 * @author Klaus Rudack
 *
 */
public class NoDownFlow {

	/**
	 * AnalysisHost for report.
	 */
	private AnalysisHost analysisHost = null;
	
	/**
	 * variable to save if an error occurred that violates the diagram.
	 */
	private boolean isSuccessful = true;
	
	/**
	 * the given model.
	 */
	private Package model;
	

	/**
	 * list that saves all the high values.
	 */
	private List<String> highValues = new ArrayList<>();
	
	
	/**
	 * Main function to analyze the model with respect to nodownflow.
	 * @param noDownFlowModel the UML-model to analyze
	 * @param host AnalysisHost for report, should be null if you have none. A new host will be created than.
	 * @return true if the diagram is correct with respect to nodownflow, false otherwise
	 */
	public final boolean startCheck(final Package noDownFlowModel, final AnalysisHost host) {
		if (noDownFlowModel == null) {
			throw new IllegalArgumentException("The UML model is not allowed be 'null'.");
		}
		this.model = noDownFlowModel;
		if (host != null) {
			this.analysisHost = host;
		} else {
			this.analysisHost = new DummyHost(true);
			this.analysisHost.appendLineToReport("No AnalysisHost is given, initialize a new AnalysisHost.");
		}
		if (!UMLHelper.isProfileApplied(this.model, UMLsec.DESCRIPTOR)) {
			this.analysisHost.appendLineToReport("No UMLsec profile applied.\nCheck successful with respect to no-down-flow.");
			AnalysisResultMessage message = new AnalysisResultMessage(StatusType.INFO, "No UMLsec profile applied");
			this.analysisHost.addResultMessage(message);
			message = new AnalysisResultMessage(StatusType.INFO, "NoDownFlowCheck successful with respect to no-down-flow");
			this.analysisHost.addResultMessage(message);
			return true;
		}
		List<Element> noDownFlowElements = UMLsecUtil.getStereotypedElements(this.model, UMLsec.NO_DOWN_FLOW);
		if ((noDownFlowElements == null) || (noDownFlowElements.size() == 0)) {
			this.analysisHost.appendLineToReport("No stereotype <<no-down-flow>> applied.\nCheck successful with respect to no-down-flow.");
			AnalysisResultMessage message = new AnalysisResultMessage(StatusType.INFO, "No stereotype <<no-down-flow>> applied");
			this.analysisHost.addResultMessage(message);
			message = new AnalysisResultMessage(StatusType.INFO, "NoDownFlowCheck successful with respect to no-down-flow");
			this.analysisHost.addResultMessage(message);
			return true;
		}
		for (Element noDownFlowElement : noDownFlowElements) {
			if (noDownFlowElement instanceof Package) {
				startTest((Package) noDownFlowElement);
			}
		}
		if (this.isSuccessful) {
			this.analysisHost.appendLineToReport("NoDownFlowCheck successful with respect to no-down-flow.");
			AnalysisResultMessage message = new AnalysisResultMessage(StatusType.INFO, "NoDownFlowCheck successful with respect to noDownFlow");
			this.analysisHost.addResultMessage(message);
		} else {
			AnalysisResultMessage message = new AnalysisResultMessage(StatusType.ERROR, "NoDownFlowCheck failed with respect to noDownFlow");
			this.analysisHost.addResultMessage(message);
		}
		return this.isSuccessful;
	}

	/**
	 * starts the main tests.
	 * @param noDownFlowElement the element where the stereotype no down flow is applied
	 */
	private void startTest(final Package noDownFlowElement) {
		for (Element element : UMLsecUtil.getStereotypedElements(noDownFlowElement, UMLsec.CRITICAL)) {
			for (Object object : UMLsecUtil.getStringValues("high", UMLsec.CRITICAL, element)) {
					this.highValues.add((String) object);
			}
		}
		if (this.highValues.size() == 0) {
			this.analysisHost.appendLineToReport("No sensitive methods and properties.");
			AnalysisResultMessage message = new AnalysisResultMessage(StatusType.INFO,
					"No sensitive methods and properties\nCheck successfull with respect to noDownFlow");
			this.analysisHost.addResultMessage(message);
			return;
		}
		for (Transition transition : UMLHelper.getAllElementsOfType(noDownFlowElement, Transition.class)) {
			List<Trigger> triggerList = transition.getTriggers();
			for (Trigger trigger : triggerList) {
				for (String highValue : this.highValues) {
					if (trigger.getEvent().getName().contains(highValue)) {
						testDataLeak(transition);
					}
				}		
			}
		}
	}
	
	/**
	 * tests if sensitive data leaks out.
	 * @param transition the transition between two states where the data would be leaking
	 */
	private void testDataLeak(final Transition transition) {
		if (transition.getTarget() == transition.getSource()) {
			return;
		}
		Map<String, String> nonHighTriggerEffects = new HashMap<>();
		for (Transition trans : transition.getSource().getOutgoings()) {
			List<Trigger> triggerList = trans.getTriggers();
			boolean highTrigger = false;
			for (Trigger trigger : triggerList) {
				for (String highValue : this.highValues) {
					if (trigger.getEvent().getName().equals(highValue)) {
						highTrigger = true;
					}
				}
				if (!highTrigger) {
					String triggerName = trigger.getName();
					String effectName = trans.getEffect().getName();
					nonHighTriggerEffects.put(triggerName, effectName);
				}
			}
		}
		for (Transition trans : transition.getTarget().getOutgoings()) {
			List<Trigger> triggerList = trans.getTriggers();
			boolean highTrigger = false;
			for (Trigger trigger : triggerList) {
				for (String highValue : this.highValues) {
					if (trigger.getEvent().getName().equals(highValue)) {
						highTrigger = true;
					}
				}
				if (!highTrigger) {
					String triggerName = trigger.getName();
					String effectName = trans.getEffect().getName();
// Ist das die Definition? Im Source State eine Transition mit dem gleichen non-high Trigger,
// aber einen anderen Effekt als im Target State?
// Was ist, wenn es mehrere Transitionen braucht, aber dann erst verschiedene Ausgaben hat?
// z.B. mit einer weiteren non-high Transition
//					FIXME KLausR: wer hat den Kommentar hier geschrieben? Das ist das was ich zu dem
//					Algorythmus angemerkt hatte.
					if (nonHighTriggerEffects.containsKey(triggerName) 
							&& (!nonHighTriggerEffects.get(triggerName).equals(effectName))) {
						this.isSuccessful = false;
						this.analysisHost.appendLineToReport("The trigger " + triggerName + " in states " + transition.getTarget().getName()
								+ " , " + transition.getSource().getName() + " leaks sensitive data");
					}
				}
			}
		}
	}

	
}
