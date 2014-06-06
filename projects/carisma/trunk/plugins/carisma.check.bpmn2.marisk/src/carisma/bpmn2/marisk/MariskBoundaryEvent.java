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

package carisma.bpmn2.marisk;

import java.util.List;
import java.util.Map;

import org.eclipse.bpmn2.Activity;
import org.eclipse.bpmn2.EventDefinition;
import org.eclipse.bpmn2.FlowElementsContainer;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.bpmn2.marisk.util.MariskHelper;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;


/**
 * Checker for the MaRisk(VA).
 * Should substitute the MariskCheckers1-9 (by Sebastian Haronski)
 * @author Marcel Michel
 */
public class MariskBoundaryEvent implements CarismaCheck {

	/**
	 * The analysis host.
	 */
	private AnalysisHost host = null;
	
	/**
	 * Resolved Activity objects.
	 */
	private List<Activity> activityList = null;
	
	/**
	 * Activity Names.
	 */
	private String[] activityNames = null;
	
	/**
	 * Container of the activities.
	 */
	private FlowElementsContainer container = null;
	
	/**
	 * Regular Expression for an Activity.
	 */
	private static final String REG_ACTIVITY = "\\w+(\\s\\w+)*";
	
	/**
	 * Regular Expression for activity repetition.
	 */
	private static final String REG_ACTIVITIES = "(\\s*(" + REG_ACTIVITY + "\\s*(,\\s*" + REG_ACTIVITY + ")*)\\s*)";
	
	@SuppressWarnings("unchecked")
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		this.host = host;
		
		boolean errorAccured = false;
		
		if (parameters.containsKey("carisma.check.bpmn2.marisk.process")
				&& parameters.containsKey("carisma.check.bpmn2.marisk.activities")
				&& parameters.containsKey("carisma.check.bpmn2.marisk.cancelactivity")
				&& parameters.containsKey("carisma.check.bpmn2.marisk.boundaryevent")) {
			String containerParameter = ((StringParameter) parameters.get(
					"carisma.check.bpmn2.marisk.process")).getValue().trim(); 
			String activitiesParameter = ((StringParameter) parameters.get(
					"carisma.check.bpmn2.marisk.activities")).getValue().trim();
			String boundaryEventParameter = ((StringParameter)parameters.get(
					"carisma.check.bpmn2.marisk.boundaryevent")).getValue();
			Boolean cancelActivity = ((BooleanParameter) parameters.get(
					"carisma.check.bpmn2.marisk.cancelactivity")).getValue();
			
			if (MariskHelper.parseActivities(activitiesParameter)) {
				activityNames = MariskHelper.extractActivities(activitiesParameter);
				boolean allItemsMatched = mapActivitiesAndContainer(containerParameter, activityNames);
				if (!allItemsMatched) {
					if (container == null) {
						String msg = "Container element \"" + containerParameter + "\" could not be matched.";
						host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
								msg));
						errorAccured = true;
					} else {
						List<String> unmatchedActivities = MariskHelper.getUnmatchedActivities(activityNames, activityList);
						StringBuffer msg = new StringBuffer("The following Activities could not be matched: ");
						for (int i = 0; i < unmatchedActivities.size(); i++) {
							if ((i + 1) == unmatchedActivities.size()) {
							    msg.append(unmatchedActivities.get(i));
							    msg.append(".");
							} else {
							    msg.append(unmatchedActivities.get(i));
							    msg.append(", ");
							}
							
						}
						host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
								msg.toString()));
						errorAccured = true;
					}
				} else {
					// ################ START: Boundary Event #################
					Class<EventDefinition> c = null;
					
					// ################ START: Map StartEvent #################
					if (!boundaryEventParameter.equalsIgnoreCase("null")) {
						EClass eClass = MariskHelper.findEClass(boundaryEventParameter, host.getAnalyzedModel());
						if (eClass == null) {
							eClass = MariskHelper.findEClass(boundaryEventParameter + "Definition", host.getAnalyzedModel());
						}
						if (eClass != null) {
							if (EventDefinition.class.isAssignableFrom(eClass.getInstanceClass())) {
								c = (Class<EventDefinition>) eClass.getInstanceClass();
							} else {
								String msg = "Class \"" + eClass.getInstanceClassName() + "\" is not an EventDefinition.";
								host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
										msg));
								errorAccured = true;
							}
						} else {
							String msg = "The StartEvent \"" + boundaryEventParameter + "\" could not be mapped.";
							host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
									msg));
							errorAccured = true;
						}
					}
					// ################ END: Map StartEvent #################
					
					for (Activity activity : activityList) {
						if (!MariskHelper.hasBoundaryEventOfType(
								activity, 
								c, 
								cancelActivity)) {
							host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
									"Activity '" + activity.getName() + "' does not fulfill the defined properties."));
							errorAccured = true;
						}
					}
					// ################# END: Boundary Event ##################
				}
			} else {
				host.addResultMessage(new AnalysisResultMessage(
						StatusType.ERROR, "Activity input must meet pattern " + REG_ACTIVITIES));
			}
		} else {
			host.addResultMessage(new AnalysisResultMessage(
					StatusType.ERROR, "Paramerts missing. Create a new Analysis file!"));
			errorAccured = true;
		}
		
		return !errorAccured;
	}
	
	/**
	 * Maps the activity and the container string to model elements. 
	 * @param containerName The name of the container
	 * @param activityNames The names of the activities
	 * @return if successful true otherwise false
	 */
	private boolean mapActivitiesAndContainer(final String containerName, final String[] activityNames) {
		Resource modelRes = host.getAnalyzedModel();
		container = MariskHelper.mapContainer(modelRes, containerName);
		activityList = MariskHelper.mapActivities(modelRes, activityNames);
		return (container != null && (activityNames.length == activityList.size()));
	}


}
