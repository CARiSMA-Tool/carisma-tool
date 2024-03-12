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
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.bpmn2.trace.BPMN2Trace;
import carisma.modeltype.bpmn2.trace.NoTracesCalculatedException;
import java.util.logging.Logger;

/**
 * Checker for the MaRisk(VA).
 * Should substitute the MariskCheckers1-9 (by Sebastian Haronski)
 * @author Marcel Michel
 */
public class MariskActivitySequenceCheck implements CarismaCheckWithID {

	private static final Logger logger = Logger.getLogger(MariskActivitySequenceCheck.class.getName());
	private static final String CHECK_ID = "carisma.check.bpmn2.marisk.actvitysequence";
	public static final String PARAM_STARTEVENT = "carisma.check.bpmn2.marisk.startevent";
	public static final String PARAM_ACTIVITIES = "carisma.check.bpmn2.marisk.activities";
	public static final String PARAM_PROCESS = "carisma.check.bpmn2.marisk.process";
	private static final String CHECK_NAME = "MaRisk(VA) Activity Sequence Check";
	
	/**
	 * The analysis host.
	 */
	private AnalysisHost analysisHost = null;
	
	/**
	 * Helper Class.
	 */
	private BPMN2Trace traceHelper = null;
	
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
	
	@SuppressWarnings("unchecked")
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		this.analysisHost = host;
		
		boolean errorAccured = false;
		
		if (parameters.containsKey(PARAM_PROCESS)
				&& parameters.containsKey(PARAM_ACTIVITIES)
				&& parameters.containsKey(PARAM_STARTEVENT)) {
			String containerParameter = ((StringParameter) parameters.get(
					PARAM_PROCESS)).getValue().trim(); 
			String activitiesParameter = ((StringParameter) parameters.get(
					PARAM_ACTIVITIES)).getValue().trim();
			String startEventParameter = ((StringParameter)parameters.get(
					PARAM_STARTEVENT)).getValue();
			
			this.traceHelper = new BPMN2Trace();
			
			// ################ START: Activity Sequence #################
			if (MariskHelper.parseActivities(activitiesParameter)) {
				this.activityNames = MariskHelper.extractActivities(activitiesParameter);
				boolean allItemsMatched;
				try {
					allItemsMatched = mapActivitiesAndContainer(containerParameter, this.activityNames);
				} catch (IncompleteMappingExeption e1) {
		            logger.warning("Error message: " + e1.getMessage());
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
					// ################ START: Analysis #################
					this.traceHelper.calculateTraces(this.container);
					boolean hasTrace = false;
					Class<EventDefinition> c = null;
					
					// ################ START: Map StartEvent #################
					if (!startEventParameter.equalsIgnoreCase("null")) {
						EClass eClass = MariskHelper.findEClass(startEventParameter, host.getAnalyzedModel());
						if (eClass == null) {
							eClass = MariskHelper.findEClass(startEventParameter + "Definition", host.getAnalyzedModel());
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
							String msg = "The StartEvent \"" + startEventParameter + "\" could not be mapped.";
							host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
									msg));
							errorAccured = true;
						}
					}
					// ################ END: Map StartEvent #################
					
					try {
						hasTrace = this.traceHelper.hasTrace(this.activityList, c);
					} catch (NoTracesCalculatedException e) {
						host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
								"Error during Trace calculation"));
						errorAccured = true;
					}
					// ################ END: Analysis #################
					
					if (!hasTrace) {
						String msg = "The defined Path could not be found in the process!";
						host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
								msg));
						errorAccured = true;
					} else {
						String msg = "All Activities are performed in the correct order.";
						host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, 
								msg));
					}
				}

			} else {
				host.addResultMessage(new AnalysisResultMessage(
						StatusType.ERROR, "Activity input must meet pattern " + "Activity1, Activity2, ..."));
			}
			// ################# END: Activity Sequence ##################
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
	 * @throws IncompleteMappingExeption 
	 */
	private boolean mapActivitiesAndContainer(final String containerName, final String[] activityNames) throws IncompleteMappingExeption {
		Resource modelRes = this.analysisHost.getAnalyzedModel();
		this.container = MariskHelper.mapContainer(modelRes, containerName);
		this.activityList = MariskHelper.mapActivities(modelRes, activityNames);
		return (this.container != null && (activityNames.length == this.activityList.size()));
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
