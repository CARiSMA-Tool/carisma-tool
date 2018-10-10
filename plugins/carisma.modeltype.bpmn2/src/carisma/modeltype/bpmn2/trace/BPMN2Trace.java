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
package carisma.modeltype.bpmn2.trace;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.bpmn2.Activity;
import org.eclipse.bpmn2.BoundaryEvent;
import org.eclipse.bpmn2.EventDefinition;
import org.eclipse.bpmn2.FlowElement;
import org.eclipse.bpmn2.FlowElementsContainer;
import org.eclipse.bpmn2.FlowNode;
import org.eclipse.bpmn2.SequenceFlow;
import org.eclipse.bpmn2.StartEvent;

/**
 * Trace Calculation in a bpmn2 model.
 * 
 * @author Sebastian Haronski
 * @author Marcel Michel
 *
 */
public class BPMN2Trace {

	/**
	 * All traces inside the FlowElementContainer.
	 */
	private List<List<FlowNode>> allTraces = null;
	
	/**
	 * No Traces Calculated Error Message.
	 */
	private final String NO_TRACES_CALCULATED_ERROR = "No Traces were calculated before";

	/**
	 * Calculates all traces through the process and stores them into allTraces.
	 * @param flowElementsContainer The flowElementsContainer which owns the activities
	 * @return If successful true, otherwise false
	 */
	public final boolean calculateTraces(final FlowElementsContainer flowElementsContainer) {
		this.allTraces = new LinkedList<>();
		
		for (FlowElement elem : flowElementsContainer.getFlowElements()) {
			if (elem instanceof StartEvent) {
				this.allTraces.addAll(getTraces((StartEvent) elem, new LinkedList<FlowNode>()));
			} else if (elem instanceof Activity	&& ((Activity) elem).getIncoming().size() == 0) {
				this.allTraces.addAll(getTraces((Activity) elem, new LinkedList<FlowNode>()));
			}
		}

		//remove traces with endloops
		for (int i = this.allTraces.size() - 1; i >= 0; i--) {
			if (loopInTrace(this.allTraces.get(i))) {
				this.allTraces.remove(i);
			}
		}
		
		return (this.allTraces.size() > 0) ? true : false; 
	}
	
	/**
	 * Checks if an activity is always executed before an other one.
	 * @param act1 The activity which shall be executed first.
	 * @param act2 The activity which shall be executed second.
	 * @return True, if act1 is always executed before act2, otherwise false.
	 * @throws NoTracesCalculatedException if no traces were calculated before.
	 */
	public final boolean allTracesBefore(final Activity act1, final Activity act2) throws NoTracesCalculatedException {
		if (this.allTraces == null) {
			throw new NoTracesCalculatedException(this.NO_TRACES_CALCULATED_ERROR);
		}
		
		int pos1 = 0;
		int pos2 = 0;
		for (List<FlowNode> lfn : this.allTraces) {
			pos1 = lfn.indexOf(act1);
			pos2 = lfn.indexOf(act2);
			if (pos2 != -1 && (pos1 == -1 || pos2 < pos1)) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Checks if all Traces contain a specific activity.
	 * @param activity The activity which all Traces shall contain.
	 * @return True if all Traces in allTraces contain the activity, otherwise false.
	 * @throws NoTracesCalculatedException if no traces were calculated before.
	 */
	public final boolean allTracesInclude(final Activity activity) throws NoTracesCalculatedException {
		if (this.allTraces == null) {
			throw new NoTracesCalculatedException(this.NO_TRACES_CALCULATED_ERROR);
		}
		
		for (List<FlowNode> lfn : this.allTraces) {
			if (!lfn.contains(activity)) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Checks if a trace exists with a specific order of activities and may print out a message.
	 * @param activities The specific order of activities.
	 * @param startEventType of the StartEvent of the trace, null if no Startevent is required.
	 * @param <T> Type extends EventDefinition.
	 * @throws NoTracesCalculatedException if no traces were calculated before.
	 * @return True if a trace with the given order of activities exists in allTraces.
	 */
	public final <T extends EventDefinition> boolean hasTrace(final List<Activity> activities, final Class<T> startEventType) throws NoTracesCalculatedException {
		if (this.allTraces == null) {
			throw new NoTracesCalculatedException(this.NO_TRACES_CALCULATED_ERROR);
		}
		
		List<List<FlowNode>> tmpList = new LinkedList<>();

		if (startEventType == null) {
			tmpList = this.allTraces;
		} else {
			for (List<FlowNode> lfn : this.allTraces) {
				if (lfn.get(0) instanceof StartEvent) {
					for (EventDefinition event : ((StartEvent) lfn.get(0)).getEventDefinitions()) {
						if (startEventType.isInstance(event)) {
							tmpList.add(lfn);
							break;
						}
					}
				}
			}
		}

		boolean passed = false;

		mark:
		for (List<FlowNode> trace : tmpList) {
			int nr = 0;
			for (FlowNode node : trace) {
				while (node == activities.get(nr)) {
					nr++;
					if (nr >= activities.size()) {
						passed = true;
						break mark;
					}
				}
			}
		}
		return passed;
	}
	
	/**
	 * Returns all traces starting at a given FlowNode.
	 * @param start The FlowNode where the traces shall start.
	 * @param trace The trace leading up to the FlowNode. Empty list
	 * @return A list containing all possible traces from the start, loops are traversed only once.
	 */
	private List<List<FlowNode>> getTraces(final FlowNode start, final List<FlowNode> trace) {
		List<List<FlowNode>> ret = new LinkedList<>();

		trace.add(start);

		FlowNode noCall = null;
		if (loopInTrace(trace)) {
			noCall = trace.get((trace.subList(0, trace.size() - 2).lastIndexOf(start) + 1));
		}

		//follow sequence flows
		int outgoing = 0;
		for (SequenceFlow sf : start.getOutgoing()) {
			if (sf.getTargetRef() == noCall) {
				continue;
			}
			outgoing++;
			for (List<FlowNode> lfn : getTraces(sf.getTargetRef(), trace)) {
				List<FlowNode> curList = new LinkedList<>();
				curList.add(start);
				curList.addAll(lfn);
				ret.add(curList);
			}
		}

		//follow boundary events
		if (start instanceof Activity) {
			for (BoundaryEvent event : ((Activity) start).getBoundaryEventRefs()) {
				if (event == noCall) {
					continue;
				}
				outgoing++;
				for (List<FlowNode> lfn : getTraces(event, trace)) {
					List<FlowNode> curList = new LinkedList<>();
					curList.add(start);
					curList.addAll(lfn);
					ret.add(curList);
				}
			}
		}

		if (outgoing == 0) {
			List<FlowNode> curList = new LinkedList<>();
			curList.add(start);
			ret.add(curList);
		}

		trace.remove(trace.size() - 1);
		return ret;
	}
	
	/**
	 * Checks for loops at the end of the given trace.
	 * @param trace The trace which is to be checked.
	 * @return Returns True if a loop at the end was found otherwise false
	 */
	private static boolean loopInTrace(final List<FlowNode> trace) {
		if (trace == null || trace.size() < 2) {
			return false;
		}

		ArrayList<FlowNode> tmpList = new ArrayList<>(trace.size() / 2);

		mark:
		for (int i = 1; i <= trace.size() / 2; i++) {
				tmpList.add(trace.get(trace.size() - i));

				for (int j = 0; j < i; j++) {
					if (!tmpList.get(j).equals(trace.get(trace.size() - tmpList.size() - j - 1))) {
						continue mark;
					}
				}
				return true;
		}
		return false;
	}
}
