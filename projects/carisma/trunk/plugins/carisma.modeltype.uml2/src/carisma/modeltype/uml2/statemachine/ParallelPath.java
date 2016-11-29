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
package carisma.modeltype.uml2.statemachine;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.FinalState;
import org.eclipse.uml2.uml.Pseudostate;
import org.eclipse.uml2.uml.PseudostateKind;
import org.eclipse.uml2.uml.Vertex;

//TODO Copy HistoryImplentation of StateMachinePaths

/**
 * Class to manage parallel paths.
 *
 * @author Klaus Rudack
 * @version 1.0
 */
public class ParallelPath {

	/**
	 * list with all the possible paths in it.
	 */
	private List<List<Element>> allParallelPaths = new ArrayList<>();
	
	/**
	 * true if transitions should also be in the paths of a StateMachines.
	 */
	private boolean transitions = false;
	
	/**
	 *  list that is needed to merge the single paths in a parallelization.
	 */
	private List<List<Element>> mergeList = new ArrayList<>();
	
	/**
	 * element where the parallelization ends.
	 */
	private Element endsWith;
	
	/**
	 * the State or StateMachine that contains the parallelization.
	 */
	private Element topState;
	
	
	/**
	 * Default constructor.
	 *
	 */
	public ParallelPath() {
		
	}
	
	/**
	 * start function to get all possible path through parallelization.
	 *
	 * @param forkState fork-element where the parallelization starts
	 * @param includeTransitions boolean if transitions should be in the result
	 * @return list with all paths through a StateMachine beginning with the given path
	 */
	public final List<List<Element>> getParallelPath(final Pseudostate forkState, final boolean includeTransitions) {
		this.transitions = includeTransitions;
		Map<Element, Element> map = new HashMap<>();
		
		this.topState = forkState.getOwner().getOwner();
		for (int i = 0; i < forkState.getOutgoings().size(); i++) {
			List<Element> sl = new ArrayList<>();
			sl.add(forkState);
			if (includeTransitions) {
				sl.add(forkState.getOutgoings().get(i));
			}
			sl.add(((Vertex) forkState).getOutgoings().get(i).getTarget());
			start(sl, map);
		}
		deleteDoubles();
		this.endsWith = this.mergeList.get(0).get(this.mergeList.get(0).size() - 1);
		mergeLists();
		for (int i = this.allParallelPaths.size() - 1; i >= 0; i--) {
			if (this.allParallelPaths.get(i).size() < 1) {
				this.allParallelPaths.remove(i);
			}
		}
		return this.allParallelPaths;
	}
	
	/**
	 * gets all the paths between fork and join.
	 * @param newList2 path that already has been determined
	 * @param history Map with starting-states for history
	 *
	 */
	private void start(final List<Element> newList2, final Map<Element, Element> history) {
		Element acc = newList2.get(newList2.size() - 1);
		if (acc == null) {
			return;
		}
		Map<Element, Element> historyCopy = new HashMap<>(history);
		Element parent = acc.getOwner().getOwner();
		boolean gone = false;
		boolean way = false;
		if (historyCopy.containsKey(parent) && !(acc instanceof Pseudostate)) {
			historyCopy.remove(parent);
		}
		if (!(acc instanceof FinalState)) {
			historyCopy.put(parent, acc);
		}
//		if ((parent.equals(topState)) && !(acc instanceof Pseudostate)) {
//			List<Element> newList = new ArrayList<Element>(newList2);
//			Map<Element, Element> nextHash = new HashMap<Element, Element>(historyCopy);
//			if (parent != topState) {
//				findParentWays(newList, nextHash);
//			}
//			way = true;
//		}
//		TODO KR: weg darf die parallelisierung nicht verlassen
		if ((acc instanceof Pseudostate) && !(((Pseudostate) acc).getKind().equals(PseudostateKind.INITIAL_LITERAL)
				|| ((Pseudostate) acc).getKind().equals(PseudostateKind.CHOICE_LITERAL)
				|| ((Pseudostate) acc).getKind().equals(PseudostateKind.JUNCTION_LITERAL))) {
			if (((Pseudostate) acc).getKind().equals(PseudostateKind.JOIN_LITERAL)) {
				this.mergeList.add(newList2);
				gone = true;
			}
			if (((Pseudostate) acc).getKind().equals(PseudostateKind.FORK_LITERAL)) { 
				ParallelPath pWay = new ParallelPath();
				List<List<Element>> t = pWay.getParallelPath((Pseudostate) acc, this.transitions);
				for (int i = 0; i < t.size(); i++) {
					List<Element> ll = new ArrayList<>(newList2);
					ll.addAll(t.get(i));
					for (int j = 0; j < ((Vertex) ll.get(ll.size() - 1)).getOutgoings().size(); j++) {
						List<Element> newStart = new ArrayList<>(ll);
						newStart.add(((Vertex) ll.get(ll.size() - 1)).getOutgoings().get(j).getTarget());
						start(newStart, historyCopy);
					}
				}
				gone = true;
			} else {
				if (((Pseudostate) acc).getKind().equals(PseudostateKind.SHALLOW_HISTORY_LITERAL)) { 
					if (historyCopy.containsKey(acc.getOwner().getOwner())) {
						gone = true;
						List<Element> newList = new ArrayList<>(newList2);
						Element acc1 = historyCopy.get(acc.getOwner().getOwner());
						if (!contains(newList, acc1)) {
							newList.add(acc1);
							start(newList, historyCopy);
						} else {
							this.allParallelPaths.add(newList2);
						}
					} else {
						if (acc.getOwnedElements().size() < 1) {
							gone = true;
							Element first = getFirst(parent);
							if (first != null) {
								List<Element> newList = new ArrayList<>(newList2);
								if (!contains(newList, first)) {
									newList.add(first);
									start(newList, historyCopy);
								} else {
									this.allParallelPaths.add(newList);
								}	
							}
						}
					}
				} else {
					if (((Pseudostate) acc).getKind().equals(PseudostateKind.DEEP_HISTORY_LITERAL)) {
						if (historyCopy.containsKey(parent)) {
							Element top = parent;
							gone = true;
							Element acc1 = historyCopy.get(top);
							if (historyCopy.containsKey(acc1)) {
								top = acc1;
								acc1 = historyCopy.get(top);
							}
							List<Element> newList = new ArrayList<>(newList2);
							if (!contains(newList, acc1)) {
								newList.add(acc1);
								start(newList, historyCopy);
							} else {
								this.allParallelPaths.add(newList2);
							}
						} else {
							if (acc.getOwnedElements().size() < 1) {
								gone = true;
								Element first = getFirst(acc.getOwner().getOwner());
								if (first != null) {
									List<Element> newList = new ArrayList<>(newList2);
									if (!contains(newList, first)) {
										newList.add(first);
										start(newList, historyCopy);
									} else {
										this.allParallelPaths.add(newList);
									}	
								}
							}
						}
					}
				}
			}
		}
		if (!gone) {
			if (acc.getOwnedElements().size() > 0) {
				Element first = getFirst(acc.getOwnedElements().get(0));
				if (first != null) {
					List<Element> newList = new ArrayList<>(newList2);
					newList.add(first);
					start(newList, historyCopy);
				}
			}
			if (((Vertex) acc).getOutgoings().size() > 0) {
				for (int i = 0; i < ((Vertex) acc).getOutgoings().size(); i++) {
					List<Element> list1 = new ArrayList<>(newList2);
					if (this.transitions) {
						list1.add(((Vertex) acc).getOutgoings().get(i));
					}
					Element acc1 = ((Vertex) acc).getOutgoings().get(i).getTarget();
					if (!contains(list1, acc1)) {
						list1.add(acc1);
						start(list1, historyCopy);
						way = true;
					}	
				}
				if (!way) {
					Element newAcc = newList2.get(newList2.size() - 1);
					List<Element> cutted = newList2;
					cutted.remove(cutted.size() - 1);
					cutted = cutList(cutted, newAcc);
					this.allParallelPaths.add(cutted);
				}
			} else {
				this.allParallelPaths.add(newList2);
			}
		}
	}
	
	/**
	 * proofs if an element is already in a list.
	 * @param newList list where the element could be in
	 * @param element element that could be in a list
	 * @return true if there is a loop in the end of the path, false otherwise
	 *
	 */
	private static boolean contains(final List<Element> newList, final Element element) {
		boolean path = false;
		for (int i = newList.size() - 1; i > 0; i--) {
			if (newList.get(i) == element) {
				int l = 1;
				path = true;
				while ((l <= i) && (newList.get(i - l) != element) && (path)) {
					if (newList.get(i - l) != newList.get(newList.size() - l)) {
						path = false;
					}
					l++;
				}
				if (path) {
					return path;
				}
			}
		}
		return path;
	}
	
	
	/**
	 * cuts loops out of a path.
	 * @param cutted path with loop
	 * @param element element where the loop starts/ends
	 * @return list without loop at the end
	 */
	private static List<Element> cutList(final List<Element> cutted, final Element element) {
		List<Element> returnList = new ArrayList<>(cutted);
		int count;
		
		count = returnList.size() - 1;
		while (returnList.get(count) != element) {
			returnList.remove(count);
			count--;
		}
		return returnList;	
	}
	
	
	/**
	 * Gets all possible paths out of parallel paths.
	 * 
	 */
	private void mergeLists() {	
		for (int i = 0; i < this.mergeList.size(); i++) {
			this.mergeList.get(i).remove(0);
			this.mergeList.get(i).remove(this.mergeList.get(i).size() - 1);
		}
		Element acc = this.mergeList.get(this.mergeList.size() - 1).get(0);
		for (int i = this.mergeList.size() - 1; i >= 0; i--) {
			if (this.mergeList.get(i).get(0) == acc) {
				this.allParallelPaths.add(this.mergeList.get(i));
				this.mergeList.remove(i);
			}
		}
		while (this.mergeList.size() > 0) {
			acc = this.mergeList.get(this.mergeList.size() - 1).get(0);
			List<List<Element>> newMerge1 = new ArrayList<>();
			List<List<Element>> newMerge2 = new ArrayList<>(this.allParallelPaths);
			for (int i = this.mergeList.size() - 1; i >= 0; i--) {
				if (this.mergeList.get(i).get(0) == acc) {
					newMerge1.add(this.mergeList.get(i));
					this.mergeList.remove(i);
				}
			}
			this.allParallelPaths.clear();
			for (int i = 0; i < newMerge1.size(); i++) {
				for (int j = 0; j < newMerge2.size(); j++) {
					List<Element> r = new ArrayList<>();
					startMerge(newMerge1.get(i), newMerge2.get(j), r);
				}
			}
		}
		for (int i = 0; i < this.allParallelPaths.size(); i++) {
			this.allParallelPaths.get(i).add(this.endsWith);
		}
	}
	
	
	/**
	 * gets all possible paths through two parallel paths.
	 * @param path1 path 1 to parallelize with path 2
	 * @param path2 path 2 to parallelize with path 1
	 * @param parallelPrefix path that is already parallel
	 */
	private void startMerge(final List<Element> path1, final List<Element> path2, final List<Element> parallelPrefix) {
	
		if ((path1.size() == 0) && (path2.size() == 0)) {
			this.allParallelPaths.add(parallelPrefix);
		}
		if ((path1.size() != 0) && (path2.size() == 0)) {
			List<Element> res = new ArrayList<>(parallelPrefix);
			res.addAll(path1);
			this.allParallelPaths.add(res);
		}
		if ((path1.size() == 0) && (path2.size() != 0)) {
			List<Element> res = new ArrayList<>(parallelPrefix);
			res.addAll(path2);
			this.allParallelPaths.add(res);
		}
		if ((path1.size() != 0) && (path2.size() != 0)) {
			List<Element> newPrefix1 = new ArrayList<>(parallelPrefix);
			List<Element> newPrefix2 = new ArrayList<>(parallelPrefix);
			List<Element> newPath1 = new ArrayList<>(path1);
			List<Element> newPath2 = new ArrayList<>(path2);
			newPrefix1.add(newPath1.get(0));
			newPath1.remove(0);
			newPrefix2.add(newPath2.get(0));
			newPath2.remove(0);
			startMerge(newPath1, path2, newPrefix1);
			startMerge(path1, newPath2, newPrefix2);
		}
	}
	
	/**
	 * Deletes duplicated Paths.
	 *
	 */
	private void deleteDoubles() {
		for (int i = 0; i < this.mergeList.size(); i++) {
			for (int z = 0; z < this.mergeList.size(); z++) {
				if (i != z 
						&& isDouble(this.mergeList.get(z), this.mergeList.get(i))) {
					this.mergeList.remove(z);
				}
			}
		}
	}
	
	
	/**
	 * Tests if the first path is part of the second.
	 * @param list1 list 1 to test
	 * @param list2 list 2 to test
	 * @return boolean if the two paths are the same
	 */
	private static boolean isDouble(final List<Element> list1, final List<Element> list2) {
		boolean returnValue = true;
		
		if (list1.size() > list2.size()) {
			return false;
		}
		for (int i = 0; i < list1.size(); i++) {
			if (list1.get(i) != list2.get(i)) {
				returnValue = false;
			}
		}
		
		return returnValue;
	}
	
	
	/**
	 * additional function to manage composite states.
	 * @param newList2 list with the path in a StateMachine to a composite state child
	 * @param history HashMap with elements for HistoryNodes
	 */
	private void findParentWays(final List<Element> newList2, final Map<Element, Element> history) {
		Element current = newList2.get(newList2.size() - 1);
		Element parentState = current.getOwner().getOwner();
		boolean way = false;
		String className1 = "org.eclipse.uml2.uml.internal.impl.PseudostateImpl";
		String className2 = "org.eclipse.uml2.uml.internal.impl.FinalStateImpl";

		Map<Element, Element> historyClone = new HashMap<>(history);
		if (historyClone.containsKey(parentState) && !(current.getClass().getCanonicalName().equals(className1))) {
			historyClone.remove(parentState);
		}
		if (!(current.getClass().getCanonicalName().equals(className2))) {
			historyClone.put(parentState, current);
		}
		if (!(current.getClass().getCanonicalName().equals(className1)) && !(current.getClass().getCanonicalName().equals(className2))) {
			Element nextParent = parentState;
			while (nextParent != null) {
			for (int i = 0; i < ((Vertex) nextParent).getOutgoings().size(); i++) {
				List<Element> newList = new ArrayList<>(newList2);
				Element acc1 = ((Vertex) nextParent).getOutgoings().get(i).getTarget();
				if (this.transitions) {
					newList.add(((Vertex) nextParent).getOutgoings().get(i));
				}
				if (!contains(newList, acc1)) {
					way = true;
					newList.add(acc1);
					start(newList, historyClone);
				}
			}
			if (nextParent.getOwner().getOwner() != this.topState) {
				nextParent = nextParent.getOwner().getOwner();
			} else {
				nextParent = null;
			}
			}
			if (!way) {
				this.allParallelPaths.add(newList2);
			}
		}
	}
	
	
	/**
	 * gets the starting element in the given region.
	 * @param element Region to get the starting element in
	 * @return the starting element
	 */
	public final Element getFirst(final Element element) {
		Element returnValue = element;
		String className = "org.eclipse.uml2.uml.internal.impl.PseudostateImpl";

		if (returnValue.getClass().getCanonicalName().equals(className)) {
			if (((Pseudostate) returnValue).getKind().equals(PseudostateKind.INITIAL_LITERAL)) {
				return returnValue;
			}
		} else {
			if (element.getOwnedElements().size() > 0) {
				for (int i = 0; i < element.getOwnedElements().size(); i++) {
					Element ret = getFirst(element.getOwnedElements().get(i));
					if (ret != null) {
						return  ret;
					}
				}
			}
		}
		return null;
	}

}
