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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.bpmn2.Activity;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.bpmn2.marisk.util.MariskHelper;
import carisma.bpmn2.marisk.util.SoDEntity;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.util.EObjectUtil;


/**
 * Checker for the MaRisk(VA).
 * Should substitute the MariskCheckers1-9 (by Sebastian Haronski)
 * @author Marcel Michel
 */
public class MariskSoDCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.bpmn2.marisk.sod";

	public static final String PARAM_SOD = "carisma.check.bpmn2.marisk.sod";
	
	public static final String CHECK_NAME = "MaRisk(VA) SoD Check";

	/**
	 * The analysis host.
	 */
	private AnalysisHost analysisHost = null;
	
	/**
	 * Names of the SoD entities.
	 */
	private List<SoDEntity<String>> sodNameList = null;
	
	/**
	 * Resolved SoD entities.
	 */
	private List<SoDEntity<Activity>> sodActivityList = null;

	/**
	 * Regular Expression for an Activity.
	 */
	private static final String REG_ACTIVITY = "\\w+(\\s\\w+)*";
	
	/**
	 * Regular Expression for activity repetition.
	 */
	private static final String REG_ACTIVITIES = "(\\s*(" + REG_ACTIVITY + "\\s*(,\\s*" + REG_ACTIVITY + ")*)\\s*)";
	
	/**
	 * Regular Expression for the SoD Pattern.
	 */
	private static final String REG_SOD_PATTERN = "(" + REG_ACTIVITIES + "\\|" + REG_ACTIVITIES + ";)+";
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		this.analysisHost = host;
		
		boolean errorAccured = false;
		
		if (parameters.containsKey(MariskSoDCheck.CHECK_ID)) {
			String sodParameter = ((StringParameter) parameters.get(
					MariskSoDCheck.CHECK_ID)).getValue().trim();
			
			// ################ START: SEPARATION OF DUTY ################
			if (sodParameter.equals("")) {
				host.addResultMessage(new AnalysisResultMessage(
						StatusType.WARNING, "No Separation of Duty Constraint defined."));
			} else if (parseSoDActivities(sodParameter)) {
				this.sodNameList = extractSoDActivites(sodParameter);
				this.sodActivityList = mapSoDActivities(this.sodNameList);
				List<String> missedMappingNames = checkProperSoDMapping(this.sodActivityList, this.sodNameList);
				if (missedMappingNames.size() != 0) {
					host.addResultMessage(new AnalysisResultMessage(
							StatusType.WARNING, "Not all SoD elements could be found in the process!"));
					StringBuffer msg = new StringBuffer("The following elements could not be mapped: ");
					for (int i = 0; i < missedMappingNames.size(); i++) {
						if ((i + 1) == missedMappingNames.size()) {
							msg.append(missedMappingNames.get(i));
							msg.append(".");
						} else {
							msg.append(missedMappingNames.get(i));
							msg.append(", ");
						}
						
					}
					host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, msg.toString()));
				}
				boolean sodCheck = performSeparationOfDutyTest(this.sodActivityList);
				if (sodCheck) {
					host.addResultMessage(new AnalysisResultMessage(
							StatusType.INFO, "Separation of Duty Constraint passed."));
				} else {
					host.addResultMessage(new AnalysisResultMessage(
							StatusType.ERROR, "Separation of Duty Constraint is VIOLATED!"));
					errorAccured = true;
				}
			} else {
				host.addResultMessage(new AnalysisResultMessage(
						StatusType.ERROR, "SoD input must meet pattern " + REG_SOD_PATTERN));
				errorAccured = true;
			}
			// ################# END: SEPARATION OF DUTY #################
		} else {
			host.addResultMessage(new AnalysisResultMessage(
					StatusType.ERROR, "Paramerts missing. Create a new Analysis file!"));
			errorAccured = true;
		}
		
		return !errorAccured;
	}

	/**
	 * Checks if the sod pattern is applied correctly.
	 * @param sodParameter The string which should be checked
	 * @return If string matches the pattern return true otherwise false
	 */
	private static boolean parseSoDActivities(final String sodParameter) {
		return sodParameter.matches(REG_SOD_PATTERN);
	}
	
	/**
	 * Extracts the SoD activities from the sodParameter.
	 * @param sodParameter The SoD String
	 * @return A List of the extracted SoD Entities
	 */
	private static List<SoDEntity<String>> extractSoDActivites(final String sodParameter) {
		List<SoDEntity<String>> result = new ArrayList<>();
		String[] constraints = sodParameter.split(";");
		for (String con : constraints) {
			String[] sep = con.trim().split("\\|");
			List<String> activities1 = new ArrayList<>();
			List<String> activities2 = new ArrayList<>();
			for (String str : sep[0].split(",")) {
				activities1.add(str.trim());
			}
			for (String str : sep[1].split(",")) {
				activities2.add(str.trim());
			}
			result.add(new SoDEntity<>(activities1, activities2));
		}
		return result;
	}
	
	/**
	 * Maps the SoD elements to model elements. 
	 * @param sodList The SoD elements
	 * @return The List of the mapped SoD Entities
	 */
	private List<SoDEntity<Activity>> mapSoDActivities(final List<SoDEntity<String>> sodList) {
		List<SoDEntity<Activity>> result = new ArrayList<>();
		
		for (SoDEntity<String> obj : sodList) {
			result.add(new SoDEntity<>(
					new ArrayList<Activity>(obj.getSeparatedActivities1().size()), 
					new ArrayList<Activity>(obj.getSeparatedActivities2().size())));
		}
		
		Resource modelRes = this.analysisHost.getAnalyzedModel();
		TreeIterator<EObject> iterator = modelRes.getAllContents();
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			if (obj instanceof Activity) {
				String name = EObjectUtil.getName(obj);
				for (int i = 0; i < sodList.size(); i++) {
					List<String> act1 = sodList.get(i).getSeparatedActivities1();
					List<String> act2 = sodList.get(i).getSeparatedActivities2();
					for (int k = 0; k < act1.size(); k++) {
						if (name.equalsIgnoreCase(act1.get(k))) {
							result.get(i).getSeparatedActivities1().add((Activity) obj);
						}
					}
					for (int k = 0; k < act2.size(); k++) {
						if (name.equalsIgnoreCase(act2.get(k))) {
							result.get(i).getSeparatedActivities2().add((Activity) obj);
						}
					}
				}
			}
		}
		return result; 
	}
	
	/**
	 * Resolves the names of unmatched SoD elements.
	 * @param sodNames The names of the SoD Elements which should be matched
	 * @param sodActivities Actual matched elements
	 * @return The names of unmatched activity elements
	 */
	private static List<String> checkProperSoDMapping(final List<SoDEntity<Activity>> sodActivities, final List<SoDEntity<String>> sodNames) {
		List<String> localActivityNames = new ArrayList<>();
		List<String> stringNames = new ArrayList<>();
		
		for (SoDEntity<Activity> obj : sodActivities) {
			for (Activity a : obj.getSeparatedActivities1()) {
				localActivityNames.add(a.getName());
			}
			for (Activity a : obj.getSeparatedActivities2()) {
				localActivityNames.add(a.getName());
			}
		}
		
		for (SoDEntity<String> obj : sodNames) {
			for (String str : obj.getSeparatedActivities1()) {
				stringNames.add(str);
			}
			for (String str : obj.getSeparatedActivities2()) {
				stringNames.add(str);
			}
		}
		
		for (String name : localActivityNames) {
			stringNames.remove(name);
		}
		
		return stringNames;
	}
	/**
	 * Performs a separation of duty test. 
	 * @param sodList The elements which should be separated
	 * @return If successful true otherwise false
	 */
	private static boolean performSeparationOfDutyTest(final List<SoDEntity<Activity>> sodList) {
		boolean passed = true;
		for (SoDEntity<Activity> obj : sodList) {
			List<Activity> act1 = obj.getSeparatedActivities1();
			List<Activity> act2 = obj.getSeparatedActivities2();
			for (int i = 0; i < act1.size(); i++) {
				for (int k = 0; k < act2.size(); k++) {
					if (!MariskHelper.checkSeparationOfDuty(act1.get(i), act2.get(k))) {
						passed = false;
					}
				}
			}
		}
		return passed;
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
