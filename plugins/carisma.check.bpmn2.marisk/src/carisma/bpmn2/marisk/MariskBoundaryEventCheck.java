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

import carisma.bpmn2.marisk.util.IncompleteMappingExeption;
import carisma.bpmn2.marisk.util.MariskHelper;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import java.util.logging.Logger;


/**
 * Checker for the MaRisk(VA).
 * Should substitute the MariskCheckers1-9 (by Sebastian Haronski)
 * @author Marcel Michel
 */
public class MariskBoundaryEventCheck implements CarismaCheckWithID {

	private static final Logger logger = Logger.getLogger(MariskBoundaryEventCheck.class.getName());
	public static final String CHECK_ID = "carisma.check.bpmn2.marisk.boundaryevent";
	public static final String PARAM_PROCESS = "carisma.check.bpmn2.marisk.process";
	public static final String PARAM_ACTIVITIES = "carisma.check.bpmn2.marisk.activities";
	public static final String PARAM_BOUNDARYEVENT= "carisma.check.bpmn2.marisk.boundaryevent";
	public static final String PARAM_CANCELACTIVITY = "carisma.check.bpmn2.marisk.cancelactivityy";
	public static final String CHECK_NAME = "MaRisk(VA) Boundary Event Check";
	
	/**
	 * The analysis host.
	 */
	private AnalysisHost analysisHost = null;
	
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
		this.analysisHost = host;
		
		boolean errorAccured = false;
		
		if (parameters.containsKey(PARAM_PROCESS)
				&& parameters.containsKey(PARAM_ACTIVITIES)
				&& parameters.containsKey(PARAM_CANCELACTIVITY)
				&& parameters.containsKey(CHECK_ID)) {
			String containerParameter = ((StringParameter) parameters.get(
					PARAM_PROCESS)).getValue().trim(); 
			String activitiesParameter = ((StringParameter) parameters.get(
					PARAM_ACTIVITIES)).getValue().trim();
			String boundaryEventParameter = ((StringParameter)parameters.get(
					CHECK_ID)).getValue();
			boolean cancelActivity = ((BooleanParameter) parameters.get(
					PARAM_CANCELACTIVITY)).getValue();
			
			if (MariskHelper.parseActivities(activitiesParameter)) {
				this.activityNames = MariskHelper.extractActivities(activitiesParameter);
				boolean allItemsMatched;
				try {
					allItemsMatched = mapActivitiesAndContainer(containerParameter, this.activityNames);
				} catch (IncompleteMappingExeption e) {
					logger.warning("Error message: " + e.getMessage());
					return false;
				}
				if (!allItemsMatched) {
					if (this.container == null) {
						String msg = "Container element \"" + containerParameter + "\" could not be matched.";
						host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
								msg));
						errorAccured = true;
					} else {
						List<String> unmatchedActivities = MariskHelper.getUnmatchedActivities(this.activityNames, this.activityList);
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
					
					for (Activity activity : this.activityList) {
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
	 * @param activityNameArray The names of the activities
	 * @return if successful true otherwise false
	 * @throws IncompleteMappingExeption 
	 */
	private boolean mapActivitiesAndContainer(final String containerName, final String[] activityNameArray) throws IncompleteMappingExeption {
		Resource modelRes = this.analysisHost.getAnalyzedModel();
		this.container = MariskHelper.mapContainer(modelRes, containerName);
		this.activityList = MariskHelper.mapActivities(modelRes, activityNameArray);
		return (this.container != null && (activityNameArray.length == this.activityList.size()));
	}

	@Override
	public String getCheckID() {
		return CHECK_ID;
	}

	@Override
	public String getName() {
		return CHECK_NAME;
	}


}
