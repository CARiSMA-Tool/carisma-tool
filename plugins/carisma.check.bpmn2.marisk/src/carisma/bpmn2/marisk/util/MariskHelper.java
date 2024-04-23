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
package carisma.bpmn2.marisk.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;

import org.eclipse.bpmn2.Activity;
import org.eclipse.bpmn2.BoundaryEvent;
import org.eclipse.bpmn2.EventDefinition;
import org.eclipse.bpmn2.FlowElementsContainer;
import org.eclipse.bpmn2.Lane;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.util.EObjectUtil;

/**
 * Marisk Helper.
 * @author Marcel Michel
 *
 */
public class MariskHelper {

	/**
	 * Regular Expression for an Activity.
	 */
	private static final String REG_ACTIVITY = "\\w+(\\s\\w+)*";
	
	/**
	 * Regular Expression for activity repetition.
	 */
	private static final String REG_ACTIVITIES = "(\\s*(" + REG_ACTIVITY + "\\s*(,\\s*" + REG_ACTIVITY + ")*)\\s*)";
	
	/**
	 * Extracts and trims the activityParameter.
	 * @param activitiesParameter The activitiesParameter 
	 * @return The activities represented as String Array 
	 */
	public static String[] extractActivities(final String activitiesParameter) {
		String[] result = activitiesParameter.split(",");
		for (int i = 0; i < result.length; i++) {
			result[i] = result[i].trim();
		}
		return result;
	}

	/**
	 * Checks if the activity pattern is applied correctly.
	 * @param activityParameter The string which should be checked
	 * @return If string matches the pattern return true otherwise false
	 */
	public static boolean parseActivities(final String activityParameter) {
		return activityParameter.matches(REG_ACTIVITIES);
	}
	
	/**
	 * Resolves the names of unmatched activity elements.
	 * @param activityNames The names of the activities which should be matched
	 * @param activityList Actual matched activities
	 * @return The names of unmatched activity elements
	 */
	public static List<String> getUnmatchedActivities(final String[] activityNames, final List<Activity> activityList) {
		List<String> result = new ArrayList<>();
		
		for (String str : activityNames) {
			result.add(str);
		}
		
		for (Activity a : activityList) {
			boolean found = false;
			for (int i = 0; i < result.size() && !found; i++) {
				if (result.get(i).equalsIgnoreCase(a.getName())) {
					found = true;
					result.remove(i);
				}
			}
		}
		
		return result;
	}
	
	/**
	 * Searches an EClass in a given model.
	 * @param eClassName The name of the searched class
	 * @param model The model resource
	 * @return The class represented by the parameter contextString, if 
	 * no class could be found null is returned.
	 */
	public static EClass findEClass(final String eClassName, final Resource model) {
		EObject tmpObject = null;
		
		//list packages of model
		TreeIterator<EObject> modelIterator = model.getAllContents();
		Set<EPackage> pacSet = new HashSet<>();
		
		while (modelIterator.hasNext()) {
			tmpObject = modelIterator.next();
			pacSet.add(tmpObject.eClass().getEPackage());
		}
		
		//list sub- / superpackages
		Set<EPackage> packagesToAdd = new HashSet<>();
		
		for (EPackage epac : pacSet) {
			packagesToAdd.addAll(epac.getESubpackages());
			
			EPackage superPac = epac.getESuperPackage();
			while (superPac != null && packagesToAdd.add(superPac)) {
				superPac = superPac.getESuperPackage(); 
			}
		}
		pacSet.addAll(packagesToAdd);
	
		//search in packages
		for (EPackage epac : pacSet) {
			TreeIterator<EObject> content = epac.eAllContents();
			while (content.hasNext()) {
				tmpObject = content.next();
				if (tmpObject instanceof EClass 
						&& ((EClass) tmpObject).getName().equalsIgnoreCase(eClassName)) {
					return (EClass) tmpObject;
				}
			}
		}
		return null;
	}
	
	/**
	 * Maps the activity to model elements. 
	 * @param model The model resource
	 * @param activityNames The names of the activities
	 * @return if successful a list of model elements
	 * @throws IncompleteMappingExeption if not all nodes could be mapped
	 */
	public static List<Activity> mapActivities(final Resource model, final String[] activityNames) throws IncompleteMappingExeption {
		Hashtable<String, Activity> activityList = new Hashtable<>();

		TreeIterator<EObject> iterator = model.getAllContents();
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			if (obj instanceof Activity) {
				String name = ((Activity) obj).getName();
				boolean found = false;
				for (int i = 0; i < activityNames.length && !found; i++) {
					if (name.equalsIgnoreCase(activityNames[i])) {
						found = true;
						activityList.put(activityNames[i], (Activity) obj);
					}
				}
			}
		}
		
		if(activityList.size() != activityNames.length){
			Collection<String> notfound = new ArrayList<>();
			for (String name : activityNames) {
				if(!activityList.contains(name)){
					notfound.add(name);
				}
			}
			throw new IncompleteMappingExeption(notfound, activityList.values());
		}
		return new ArrayList<>(activityList.values());
	}
	
	/**
	 * Maps the container string to model element. 
	 * @param model The model resource
	 * @param containerName The name of the container
	 * @return if successful the container element otherwise null
	 */
	public static FlowElementsContainer mapContainer(final Resource model, final String containerName) {
		FlowElementsContainer container = null;
		TreeIterator<EObject> iterator = model.getAllContents();
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			if (obj instanceof FlowElementsContainer) {
				String name = EObjectUtil.getName(obj);
				if (name.equalsIgnoreCase(containerName)) {
					container = (FlowElementsContainer) obj;
				}
			}
		}
		return container;
	}
	
	/**
	 * Checks if an activity has a boundary event of a given type.
	 * @param activity The activity which is to be checked.
	 * @param type The required type of the activity.
	 * @param cancelActivity Indicates if the BoundaryEvent should cancel the activity (null for don't care)
	 * @param <T> A Subtype of EventDefinition.
	 * @return True if the activity contains a BoundaryEvent of the given type.
	 */
	public static final <T extends EventDefinition> boolean hasBoundaryEventOfType(
				final Activity activity, final Class<T> type, final boolean cancelActivity) {
		if (type == null) {
			return true;
		}
		for (BoundaryEvent event : activity.getBoundaryEventRefs()) {
			for (EventDefinition def : event.getEventDefinitions()) {
				if (type.isInstance(def)) {
					return (cancelActivity == event.isCancelActivity());
				}
			}
		}
		return false;
	}
	
	/**
	 * Checks if two activities are performed by different actors.
	 * @param activity1 The first activity.
	 * @param activity2 The second activity.
	 * @return Returns true if the activities are performed by different actors. Otherwise false
	 * is returned and a text is printed to the analysisreport. There is also a text output
	 * if any of the activities has an empty LaneSet, in this case true is returned.
	 */
	public static final boolean checkSeparationOfDuty(final Activity activity1, final Activity activity2) {

		if ((activity1.getLanes().size() != 0) && (activity2.getLanes().size() != 0)) {
			for (Lane lane : activity1.getLanes()) {
				if (activity2.getLanes().contains(lane)) {
					return false;
				}
			}
		}
		return true;
	}

}



