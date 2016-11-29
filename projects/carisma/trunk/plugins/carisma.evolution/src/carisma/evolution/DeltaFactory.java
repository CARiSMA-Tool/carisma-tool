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
package carisma.evolution;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;
/**
 * The Delta Factory takes the generated delta element descriptions
 * and resolves the constraints and alternatives to form 
 * permutations of the delta elements.
 * At the moment it just passes the whole set of delta elements.
 * @author Daniel Warzecha (rudimentary implementation)
 * @author Benjamin Berghoff (permutation implementation)
 *
 */
public class DeltaFactory {
	
	/** Changes from which the Deltas are produced. 
	 */
	private List<Change> 	changes = null;
	
	/** Maps which Changes shouldn't be in combined Delta.
	 */
	private Map<Change, Set<Change>> 	notDependencies = null;
	
	/** Defines which Changes depend on each other. Described  by the AND Constraint.
	 */
	private Map<Change, Set<Change>> 	andDependencies = null;

	/** Defines which Changes depend on which. Described by the REQ Constraint.
	 */
	private Map<Change, Set<Change>>	reqDependencies	= null;

	/** 
	 * A regular expression describing the ChangeId appendix used to differ multiple Alternatives. 
	 */
	private static final String CHANGE_ID_PATTERN = " \\(Alt." + "[0-9]*" + "\\)$";
	/**
	 * Initializes or resets the internal structures if necessary.
	 * @param changeList list of Changes used to create list of Deltas
	 */
	private void init(final List<Change> changeList) {
		if (this.changes == null) {
			this.changes = new ArrayList<>();
		}
		this.changes.clear();
		if (this.notDependencies == null) {
			this.notDependencies 	= new HashMap<>();			
		}
		this.notDependencies.clear();

		if (this.andDependencies == null) {
			this.andDependencies 	= new HashMap<>();			
		}
		this.andDependencies.clear();
		if (this.reqDependencies == null) {
			this.reqDependencies 	= new HashMap<>();			
		}
		this.reqDependencies.clear();

		this.changes.clear();
		this.changes.addAll(changeList);
	}
	
	/** Fetches the change structures and builds a list of deltas.
	 * depending on the input it will return a permutation or the Cartesian product
	 * 
	 * 
	 * all the elements of the two example lists are Alternatives
	 * 
	 * List1 {beef, pork, chicken}
	 * List2 {garlic, onion, tomato}
	 * 
	 * output if permutation true
	 * { {beef, garlic}, {pork, garlic}, {chicken, garlic}, 
	 *   {beef, onion},  {pork, onion},  {chicken, onion}, 
	 *   {beef, tomato}, {pork, tomato}, {chicken, tomato},
	 *   {beef}, {pork}, {chicken}, {garlic}, {onion}, {tomato} }
	 *   
	 * @param changeList the list of Changes which are used to create the list of Deltas
	 * @return a Collection of Deltas. returns null if the constraints containing cycles.
	 */
	public final static List<Delta> getDeltas(final List<Change> changeList) {
		
		List<Delta> deltas = new ArrayList<>();
		List<DeltaElement> content = new ArrayList<>();
		
		for (Change change : changeList) {
			content.addAll(change.getAlternatives().get(0).getDeltaElements());
		}
		deltas.add(new Delta(content));
		return deltas;
		
		// wegen performanceTest gruenden auskommentiert .
//		init(changeList);
//		if (changes.isEmpty()) {
//			return new ArrayList<Delta>(); 
//		}
//		
//		parseConstraints();
//		if (hasCycles(reqDependencies)) {
//			return null;
//		}
//		sortChanges();
//
//		List<List<DeltaElement>> deltasContents = new ArrayList<List<DeltaElement>>();
//		List<List<String>> changeIDs = new ArrayList<List<String>>();
//		List<Delta> deltas = new ArrayList<Delta>();
//		int maxNumberOfDeltas = computeMaxNumberOfDeltas(changes);
//		for (int i = 0; i < maxNumberOfDeltas; i++) {
//			deltasContents.add(new ArrayList<DeltaElement>());
//			changeIDs.add(new ArrayList<String>());
//		}
//
//		calculateDeltasContents(deltasContents, changeIDs, maxNumberOfDeltas);
//		for (List<DeltaElement> deltaContent : deltasContents) {
//			deltas.add(new Delta(changeIDs.get(deltasContents.indexOf(deltaContent)), deltaContent));
//		}
//		processConstraints(deltas);
//		
//		System.out.println("Delta count " + deltas.size());
//		return deltas;
	}
	
	
	/** Calculates the content of the Deltas.
	 * 
	 * @param deltasContents Empty List of content.
	 * @param changeIDs Empty List of used Changes.
	 * @param maxNumberOfDeltas number of generally all possible Deltas.
	 */
	private void calculateDeltasContents(final List<List<DeltaElement>> deltasContents, final List<List<String>> changeIDs, final int maxNumberOfDeltas) {
	       /* repetition: how often an alternative is put in following positions
         * i.e. a,1 b,1 c,1 a,2 b,2 c,2   so the repetition for the first alternatives {a,b,c} is one 
         * the repetition for the second alternatives {1,2} would be 3 ( the product over the number of preceding alternatives )
         */
        int repetition = 1;
        for (Change ch : this.changes) {
            int numberOfAlternatives = ch.getAlternatives().size();
            // numberOfChoices: alternatives count + "do not put any alternative in delta"
            int numberOfChoices = numberOfAlternatives + 1;
            if (numberOfAlternatives > 0) {
                for (int i = 0; i <= (maxNumberOfDeltas - (numberOfChoices * repetition));
                        i = i + numberOfChoices * repetition) {
                    for (int altIndex = 0; altIndex < numberOfAlternatives; altIndex++) {
                        Alternative alt = ch.getAlternatives().get(altIndex);
                        for (int j = 0; j < repetition; j++) {
                            int targetDeltaIndex = j + i + altIndex * repetition;
                            deltasContents.get(targetDeltaIndex).addAll(alt.getDeltaElements());
                            StringBuffer descr = new StringBuffer(ch.getRef());
                            if (numberOfAlternatives > 1) {
                                descr.append(" (Alt.");
                                descr.append((altIndex + 1));
                                descr.append(")");
                            }
                            changeIDs.get(targetDeltaIndex).add(descr.toString());
                        }
                    }
                }
                repetition = repetition * numberOfChoices;
            }
        }
	}
	
	/** Computes Number of Deltas before processing Constraints.
	 * @param changes List of Changes.
	 * @return number of deltas
	 */
	private static int computeMaxNumberOfDeltas(final List<Change> changes) {
		int deltaCount = 1;
		if (changes.isEmpty()) { 
			return 0;
		}
		for (Change ch : changes) {
				deltaCount *= (ch.getAlternatives().size() + 1);
		}
		return deltaCount;
	}
	
	
	/** Gets the Constraints into a better form to work with.
	 */
	private void parseConstraints() {
		for (Change ch : this.changes) {
			
			for (ChangeConstraint constraint : ch.getConstraints()) {
				if (constraint.getType() == ConstraintType.AND) {
					if (!(this.andDependencies.containsKey(ch))) {
						this.andDependencies.put(ch, new HashSet<Change>());
					}
					this.andDependencies.get(ch).add(constraint.getReferencedChange());
					if (!(this.andDependencies.containsKey(constraint.getReferencedChange()))) {
						this.andDependencies.put(constraint.getReferencedChange(), new HashSet<Change>());
					}
					this.andDependencies.get(constraint.getReferencedChange()).add(constraint.getConstrainedChange());
				} else if (constraint.getType() == ConstraintType.REQ) {
					if (!this.reqDependencies.containsKey(constraint.getConstrainedChange())) {
						this.reqDependencies.put(constraint.getConstrainedChange(), new HashSet<Change>());
					}
					this.reqDependencies.get(constraint.getConstrainedChange()).add(constraint.getReferencedChange());
				} else if (constraint.getType() == ConstraintType.NOT) {
					if (!this.notDependencies.containsKey(constraint.getConstrainedChange())) { 
						this.notDependencies.put(constraint.getConstrainedChange(), new HashSet<Change>());
					}
					this.notDependencies.get(constraint.getConstrainedChange()).add(constraint.getReferencedChange());
				}
			}
		}
	}
	
	/** Checks if the given Requirements have Cycles.
	 * 
	 * @param changeRequiredChangesMapping all REQ Constraints formated in a HashMap     Key=RefValue  Value=OtherRefValue1,OtherRefValue2
	 *                for RefValue=REQ(OtherRefValue1, OtherRefValue2)
	 * @return true if a Cycle exists, false else; 
	 */
	private boolean hasCycles(final Map<Change, Set<Change>> changeRequiredChangesMapping) {

		if (changeRequiredChangesMapping == null || changeRequiredChangesMapping.isEmpty()) {
			return false;
		}

		// remove entries which don't have a Value which is also a Key. 
		Map<Change, Set<Change>> remainingRequirementsMapping = new HashMap<>();
		for (Entry<Change, Set<Change>> entry : changeRequiredChangesMapping.entrySet()) {
		    for (Change requiredChange : entry.getValue()) {
		        if (changeRequiredChangesMapping.containsKey(requiredChange) 
		                && !remainingRequirementsMapping.containsKey(entry.getKey())) {
		            remainingRequirementsMapping.put(entry.getKey(), entry.getValue());
		        }
		    }
		}
		
		if (changeRequiredChangesMapping.size() != remainingRequirementsMapping.size()) {
				return hasCycles(remainingRequirementsMapping);
		}
		return true;
	}

	
	/** Sorts a List to match the implicit given order in reqDependencies.
	 */
	private void sortChanges() { 
		Compar x = new Compar(this.reqDependencies);
		Collections.sort(this.changes, x);
	}
	
	
	/** Checks if a Delta violates the AND constraints.
	 * 
	 * @param delta A Delta Object which will be tested.
	 * @return returns true if the dependencies are violated. Else false.
	 */
	private boolean violatesAndDependencies(final Delta delta) {
		
		if (this.andDependencies != null && !this.andDependencies.isEmpty()) {
			for (Change neededKey : this.andDependencies.keySet()) {
				if (containsChangeID(delta.getChangeIDs(), neededKey.getRef())) {
					for (Change valueNeed : this.andDependencies.get(neededKey)) {
						if (!containsChangeID(delta.getChangeIDs(), valueNeed.getRef())) {
							return true;
						}
					}
				}
			}
		}
		return false;
	}
	
	/** Checks if a Delta violates the NOT constraints.
	 * 
	 * @param delta A Delta Object which will be tested.
	 * @return returns true if the dependencies are violated. Else false.
	 */
	private boolean violatesNotDependencies(final Delta delta) {
		if (this.notDependencies != null && !this.notDependencies.isEmpty()) {
			for (Change keyNot : this.notDependencies.keySet()) {
				if (containsChangeID(delta.getChangeIDs(), keyNot.getRef())) {
					for (Change valueNot : this.notDependencies.get(keyNot)) {
						if (containsChangeID(delta.getChangeIDs(), valueNot.getRef())) {
							return true;
						}
					}
				}
			}
		}
		return false;
	}
	
	/** Checks if a Delta violates the REQ constraints.
	 * 
	 * @param delta The Delta Object which will be tested.
	 * @return returns true if the dependencies are violated. Else false.
	 */
	private boolean violatesReqDependencies(final Delta delta) {
		if (this.reqDependencies != null && !this.reqDependencies.isEmpty()) {
			for (Change keyRequired : this.reqDependencies.keySet()) {
				if (containsChangeID(delta.getChangeIDs(), keyRequired.getRef())) {
					for (Change valueRequired : this.reqDependencies.get(keyRequired)) {
						if (!containsChangeID(delta.getChangeIDs(), valueRequired.getRef())) {
							return true;
						}
					}
				}
			}
		}
		return false;
	}
	
	/** Checks if a given ChangeID is contained by a list, the appendix of the ChangeIDs, 
	 * used to differ the different Alternatives is neglected.
	 * @param changeIDs A List of Change ID.
	 * @param changeID The ChangeID to be compared ages.
	 * @return returns true if one of the List Elements starts with the String changeID.
	 */
	private static boolean containsChangeID(final List<String> changeIDs, final String changeID) {
		for (String oldChangeID : changeIDs) {
			if (oldChangeID.replaceAll(CHANGE_ID_PATTERN, "").equals(changeID)) {
				return  true;
			}
		}
		return false;
	}
	
	
	/** Checks which combinations don't apply the constraints.
	 * 
	 * @param deltas Receives the combinations
	 */
	private void processConstraints(final List<Delta> deltas) {
		List<Integer> toBeDeleted = new Stack<>();
		for (int i = deltas.size() - 1; i >= 0; i--) {
			if (deltas.get(i).isEmpty()) {
				toBeDeleted.add(Integer.valueOf(i));
				continue;
			}
			if (violatesAndDependencies(deltas.get(i))) {
				toBeDeleted.add(Integer.valueOf(i));
				continue;				
			}
			if (violatesNotDependencies(deltas.get(i))) {
				toBeDeleted.add(Integer.valueOf(i));
				continue;				
			}
			
			if (violatesReqDependencies(deltas.get(i))) {
				toBeDeleted.add(Integer.valueOf(i));
				continue;				
			}
		}
		for (Integer delete : toBeDeleted) {
			deltas.remove(delete.intValue());
		}
	}	
	
	/** Private Class to compare two Changes.
	 * @author bberghoff
	 *
	 */
	private static class Compar implements Comparator<Change>, Serializable { 
		/**
         * 
         */
        private static final long serialVersionUID = -84415735907197509L;
        /** 
		 * A list of Changes which describes the order of the Alternatives used in the Deltas.
		 */
		private LinkedList<Change> order = new LinkedList<>();
				
		/** Public Constructor.
		 * @param require 
		 */
		public Compar(final Map<Change, Set<Change>> require) {
			for (Entry<Change, Set<Change>> entry: require.entrySet()) { 
				call(entry.getKey(), entry.getValue(), require);
			}
		}
		
		/** Creates a List which defines the order of the Changes in the Deltas.
		 * @param key .
		 * @param values .
		 * @param require A Map which reflects the REQ Constraint of all Changes. 
		 */
		private void call(final Change key, final Set<Change> values, final Map<Change, Set<Change>> require) {
			if (values != null) {
				for (Change value : values) {
					if (!this.order.contains(value) && require.keySet().contains(value)) {
						call(value, require.get(value), require);
					} else if (!(this.order.contains(value))) {
							this.order.addFirst(value);
					}
				}
			}
			if (!this.order.contains(key)) {
				this.order.addLast(key);
			}
		}
		
		/** Compares two Changes based on the given order.
		 * @param o1 The first Change
		 * @param o2 The second Change
		 * @return  0 if the two arguments are from the same order. <br>
		 *                  1 if the first argument is greater then the second.<br>
		 *                -1 if the first argument is less then the second
		 */
		@Override
		public int compare(final Change o1, final Change o2) {
			if (this.order.indexOf(o1) < this.order.indexOf(o2)) {
				return -1;
			} else if (this.order.indexOf(o1) == this.order.indexOf(o2)) {
				return 0;
			}
			return 1;
		}
	}
}


