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

/**
 * 
 */
/**
 * 
 */
package carisma.modeltype.uml2.statemachine;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.FinalState;
import org.eclipse.uml2.uml.Pseudostate;
import org.eclipse.uml2.uml.PseudostateKind;
import org.eclipse.uml2.uml.StateMachine;
import org.eclipse.uml2.uml.Vertex;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.modeltype.uml2.UMLHelper;

//FIXME Does not handle nested states!
//FIXME May run into endless loop in nested state machines
//FIXME search for initial state originally provided performed a DFS, which tends to find the innermost initial state when there is more than one initial state
//FIXME Moreover, the algorithm here doesn't seem to implement a simple graph algorithm, which would be more appropriate.
/**
 * class to get all possible paths out of a state machine.
 * @author Klaus Rudack
 * @version 1.0
 *
 */
//TODO check implementation of deleting double part-paths in cutList, for now deletePartPaths will do so but need change for performance
public class StateMachinePaths {

	/**
	 * list that will be filled with all the possible paths of the state machine.
	 */
	private List<List<Element>> allPaths = new ArrayList<>();
	
	/**
	 * the given StateMachine.
	 */
	private StateMachine sm;
	
	/**
	 * true if transitions should also be in the paths of a StateMachines, false otherwise.
	 */
	private boolean withTransitions = false;
	
	/**
	 * default constructor.
	 */
	public StateMachinePaths() {
		
	}
	
	
	/**
	 * Start-Function.
	 * @param stateMachine StateMachine to get paths out of
	 * @param analysisHost AnalysisHost
	 * @param transitions boolean whether transitions should be in the result or not
	 * @return - all the paths in a StateMachine
	 */
	public final List<List<Element>> getPaths(final StateMachine stateMachine, final AnalysisHost analysisHost, final boolean transitions) {
		if (stateMachine == null) {
			throw new NullPointerException();
		}
		List<Element> startList = new ArrayList<>();
		AnalysisHost host;
		if (analysisHost != null) {
			host = analysisHost;
		} else {
			host = new DummyHost(true);
		}
			
		this.withTransitions = transitions;
		Element first;
		this.sm = stateMachine;
		first = getInitialState(stateMachine.getOwnedElements().get(0));
		if (first != null) {
			Map<Element, Element> lastStateMapping = new HashMap<>();
			startList.add(first);
			findPaths(startList, lastStateMapping);
			List<Pseudostate> entryPoints = getEntryPoints(stateMachine);
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "History in parallelization wont work proper!"));
			if (entryPoints.size() > 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "EntryPoints wont be respectet!"));
			}
			deleteFollows();
			deleteDoubles();
			deletePartPaths();
			deleteHistoryEndings();
			deleteDoubles();
		}
		return this.allPaths;
	}
	
	/**
	 * calculates all the possible paths in a StateMachine.
	 * @param pathPrefix list with a possible path till the last element in the list
	 * @param history HashMap with entryStates for History's
	 */
	private void findPaths(final List<Element> pathPrefix, final Map<Element, Element> history) {
		Vertex current = (Vertex) pathPrefix.get(pathPrefix.size() - 1); Element parentState = current.getOwner().getOwner();
		boolean foundSuccessor = false;
		Map<Element, Element> newHash = new HashMap<>(history);
		if (current instanceof FinalState) {
			newHash.remove(parentState);
		}
		if (!(parentState.equals(this.sm)) && !(current instanceof Pseudostate)) {
			if (!(current instanceof FinalState)) {
				newHash.put(parentState, current);
			}
			List<Element> pathClone = new ArrayList<>(pathPrefix);
			Map<Element, Element> historyClone = new HashMap<>();
			historyClone.putAll(newHash);
				findParentSuccessor(pathClone, historyClone);
			foundSuccessor = true;
		}
		if ((current instanceof Pseudostate) && !(((Pseudostate) current).getKind().equals(PseudostateKind.INITIAL_LITERAL)
				|| ((Pseudostate) current).getKind().equals(PseudostateKind.JOIN_LITERAL)
				|| ((Pseudostate) current).getKind().equals(PseudostateKind.CHOICE_LITERAL)
				|| ((Pseudostate) current).getKind().equals(PseudostateKind.JUNCTION_LITERAL))) {
			if (((Pseudostate) current).getKind().equals(PseudostateKind.FORK_LITERAL)) {
				ParallelPath pPath = new ParallelPath();
				List<List<Element>> parallelPaths = pPath.getParallelPath((Pseudostate) current , this.withTransitions);
				for (int i = 0; i < parallelPaths.size(); i++) {
					List<Element> pathPrefixClone = new ArrayList<>(pathPrefix);
					pathPrefixClone.addAll(parallelPaths.get(i));
					findPaths(pathPrefixClone, newHash);
				}
			} else {
				if (((Pseudostate) current).getKind().equals(PseudostateKind.SHALLOW_HISTORY_LITERAL)) {
					if (newHash.containsKey(parentState)) {
						List<Element> shallowHistoryPath = new ArrayList<>(pathPrefix);
						Element acc1 = newHash.get(parentState);
						if (!hasLoopAtEnd(shallowHistoryPath, acc1)) {
							shallowHistoryPath.add(acc1);
							findPaths(shallowHistoryPath, newHash);
						} else {
							this.allPaths.add(pathPrefix);
						}
					} else {
						if (current.getOwnedElements().size() < 1) {
							Element first = getInitialState(parentState);
							if (first != null) {
								List<Element> newList = new ArrayList<>(pathPrefix);
								if (!hasLoopAtEnd(newList, first)) {
									newList.add(first);
									findPaths(newList, newHash);
								} else {
									this.allPaths.add(newList);
								}	
							}
						}
					}
				} else {
					if (((Pseudostate) current).getKind().equals(PseudostateKind.DEEP_HISTORY_LITERAL)) {
						if (newHash.containsKey(parentState)) {
							Element historyParent = parentState;
							List<Element> deepHistoryPath = new ArrayList<>(pathPrefix);
							Element historizedState = newHash.get(historyParent);
							while (newHash.containsKey(historizedState)) {
								historyParent = historizedState;
								historizedState = newHash.get(historyParent);
							}
							if (!hasLoopAtEnd(deepHistoryPath, historizedState)) {
//								FIXME KR:  hoert einfach auf, warum? evtl weill wenn loop am ende und anderer weg m�glich dieser schon gegangen wird
								deepHistoryPath.add(historizedState);
								findPaths(deepHistoryPath, newHash);
							} else {
								this.allPaths.add(pathPrefix);
							}
						} else {
							if (current.getOwnedElements().size() < 1) {
//								FIXME KR : findet keine Elemente, geht  einfach wieder einen hoch, warum?
								Element first = getInitialState(parentState);
								if (first != null) {
									List<Element> newList = new ArrayList<>(pathPrefix);
									if (!hasLoopAtEnd(newList, first)) {
										newList.add(first);
										findPaths(newList, newHash);
									} else {
										this.allPaths.add(newList);
									}	
								}
							}
						}
					}
				}
			}
		} else { //is either InitialState, JoinState, JunctionState, ChoiceState or no Pseudostate
			if (current.getOwnedElements().size() > 0) {
				Element first = getInitialState(current.getOwnedElements().get(0));
				if (first != null) {
					List<Element> newList = new ArrayList<>(pathPrefix);
					newList.add(first);
					findPaths(newList, newHash);
				}
			}
			if (current.getOutgoings().size() > 0) {
				for (int i = 0; i < current.getOutgoings().size(); i++) {
					List<Element> normalSuccessorPath = new ArrayList<>(pathPrefix);
					if (this.withTransitions) {
						normalSuccessorPath.add(current.getOutgoings().get(i));
					}
					Element successor = current.getOutgoings().get(i).getTarget();
					if (!hasLoopAtEnd(normalSuccessorPath, successor)) {
						normalSuccessorPath.add(successor);
						findPaths(normalSuccessorPath, newHash);
						foundSuccessor = true;
					}
				}
				if (!foundSuccessor) {
					List<Element> cutted = new ArrayList<>(pathPrefix);
					Element newAcc = pathPrefix.get(pathPrefix.size() - 1);
					cutted.remove(cutted.size() - 1);
					cutted = cutListAfterLast(cutted, newAcc);
					this.allPaths.add(cutted);
				}
			} else {
				if (pathPrefix.get(pathPrefix.size() - 1).getOwner().getOwner() != this.sm) {
					Element owner = pathPrefix.get(pathPrefix.size() - 1).getOwner().getOwner();
					if (((Vertex) owner).getOutgoings().size() > 0) {
						for (int i = 0; i < ((Vertex) owner).getOutgoings().size(); i++) {
							List<Element> list1 = new ArrayList<>(pathPrefix);
							if (this.withTransitions) {
								list1.add(((Vertex) owner).getOutgoings().get(i));
							}
							Element acc1 = ((Vertex) owner).getOutgoings().get(i).getTarget();
							if (!hasLoopAtEnd(list1, acc1)) {
								list1.add(acc1);
								findPaths(list1, newHash);
								foundSuccessor = true;
							}	
						}
					}
					if (!foundSuccessor) {
						List<Element> possibleCirclePath = new ArrayList<>(pathPrefix);
						Element newAcc = pathPrefix.get(pathPrefix.size() - 1);
						possibleCirclePath.remove(possibleCirclePath.size() - 1);
						possibleCirclePath = cutListAfterLast(possibleCirclePath, newAcc);
						this.allPaths.add(possibleCirclePath);
					}
				} else {
					this.allPaths.add(pathPrefix);
				}
			}
		}
	}


	/* public final Pseudostate getInitialState(final Element element) {
	 * Old Version of this method performing a DFS
	 * 
	 * 
	 * Element returnValue = element;
	 * 
	 * if (returnValue instanceof Pseudostate && ((Pseudostate)
	 * returnValue).getKind().equals(PseudostateKind.INITIAL_LITERAL)) { return
	 * (Pseudostate) returnValue; } else { for (Element child :
	 * element.getOwnedElements()) { Pseudostate ret = getInitialState(child);
	 * if (ret != null) { return ret; } } } return null; }
	 */
	



	/**
	 * Gets the InitialState in the given region. Implements a BFS.
	 * @param element region to get the starting element within
	 * @return the InitialState
	 */
	public final static Pseudostate getInitialState(final Element element) {
		LinkedList<Pair<Element,Boolean>> q=new LinkedList<>();//boolean visited 
		q.add(new Pair<>(element, Boolean.TRUE));
		Element e=null;
		while (!q.isEmpty()){
			q.peek().setSecond(Boolean.TRUE);
			e=q.pop().getFirst();
			if (e instanceof Pseudostate && ((Pseudostate) e).getKind().equals(PseudostateKind.INITIAL_LITERAL)) {
				return (Pseudostate) e;
				}
			LinkedList<Pair<Element,Boolean>> list=new LinkedList<>();//boolean visited 
			for (Element e2 : e.getOwnedElements()){
				list.add(new Pair<>(e2,Boolean.FALSE));
			}
				for (Pair<Element,Boolean> le : list){
					if (!le.getSecond().booleanValue()) {
						q.push(le);
					}
				}
			}

		return null;
	}
	
	/**
	 * gets all the EntryPoints in a StateMachines.
	 * @param stateMachine StateMachine that could have EntryPoints in it
	 * @return list with all the EntryPoints in stateMachine
	 */
	private static List<Pseudostate> getEntryPoints(final StateMachine stateMachine) {
		List<Pseudostate> entryList = new ArrayList<>();
		for (Pseudostate pseudo : UMLHelper.getAllElementsOfType(stateMachine, Pseudostate.class)) {
			if (pseudo.getKind().equals(PseudostateKind.ENTRY_POINT_LITERAL)) {
				entryList.add(pseudo);
			}
		}
		return entryList;
	}
	
	/**
	 * proofs if there is a loop at the end of the list.
	 * @param path list where the loop could be in
	 * @param element last element of the path
	 * @return true if there is a loop at the end of the path, false otherwise
	 *
	 */
	private static boolean hasLoopAtEnd(final List<Element> path, final Element element) {
		boolean possibleLoop = false;
		for (int i = path.size() - 1; i > 0; i--) {
			if (path.get(i) == element) {
				int elementCounter = 1;
				possibleLoop = true;
				while ((elementCounter <= i) && (path.get(i - elementCounter) != element) && (possibleLoop)) {
					if (path.get(i - elementCounter) != path.get(path.size() - elementCounter)) {
						possibleLoop = false;
					}
					elementCounter++;
				}
				if (possibleLoop) {
					return possibleLoop;
				}
			}
		}
		return possibleLoop;
	}
	
	/**
	 * cuts loops out of a path.
	 * @param list path with loop
	 * @param element element where the loop starts/ends
	 * @return list without loop at the end
	 */
	private static List<Element> cutListAfterLast(final List<Element> list, final Element element) {
		List<Element> returnList = new ArrayList<>(list);
		int count;
		
		count = returnList.size() - 1;
		while (returnList.get(count) != element) {
			returnList.remove(count);
			count--;
		}
		return returnList;
		
	}
	
	/**
	 * deletes duplicated paths.
	 *
	 */
	private void deleteDoubles() {
		List<List<Element>> paths = new ArrayList<>();
		boolean isDouble = false;
		for (int i = 0; i < this.allPaths.size(); i++) {
			for (int j = 0; j < paths.size(); j++) {
				if (this.allPaths.get(i).equals(paths.get(j))) {
					isDouble = true;
				}
			}
			if (!isDouble) {
				paths.add(this.allPaths.get(i));
			}
			isDouble = false;
		}
		this.allPaths = paths;
	}
	
	
	
	/**
	 * additional function to manage Composite States.
	 * @param currentPath list with the path in a StateMachine to a Composite State child
	 * @param history HashMap with elements for HistoryNodes
	 */
	private void findParentSuccessor(final List<Element> currentPath, final Map<Element, Element> history) {
		Map<Element, Element> historyClone = new HashMap<>(history);
		Vertex current = (Vertex) currentPath.get(currentPath.size() - 1);
		Element parent = current.getOwner().getOwner();
		boolean foundSuccessor = false;
		
		if (historyClone.containsKey(parent) && !(current instanceof Pseudostate)) {
			historyClone.remove(parent);
		}
		if (!(current instanceof FinalState)) {
			historyClone.put(parent, current);
		}
		if (!(current instanceof Pseudostate) && !(current instanceof FinalState)) {
			Element nextParent = parent;
			while (nextParent != null) {
			for (int i = 0; i < ((Vertex) nextParent).getOutgoings().size(); i++) {
				List<Element> newList = new ArrayList<>(currentPath);
				Element acc1 = ((Vertex) nextParent).getOutgoings().get(i).getTarget();
				if (this.withTransitions) {
					newList.add(((Vertex) nextParent).getOutgoings().get(i));
				}
				if (!hasLoopAtEnd(newList, acc1)) {
					foundSuccessor = true;
					newList.add(acc1);
					findPaths(newList, historyClone);
				}
			}
			if (nextParent.getOwner().getOwner() != this.sm) {
				nextParent = nextParent.getOwner().getOwner();
			} else {
				nextParent = null;
			}
			}
			if (!foundSuccessor) {
				this.allPaths.add(currentPath);
			}
		}
	}
	
	/**
	 * deletes paths that ends with a history.
	 */
	private void deleteHistoryEndings() {
		
		for (int i = this.allPaths.size() - 1; i >= 0; i--) {
			Element last = this.allPaths.get(i).get(this.allPaths.get(i).size() - 1);
			if (last instanceof Pseudostate 
					&& ((((Pseudostate) last).getKind().equals(PseudostateKind.DEEP_HISTORY_LITERAL))
					|| ((Pseudostate) last).getKind().equals(PseudostateKind.SHALLOW_HISTORY_LITERAL))) {
				this.allPaths.remove(i);
			}
		}
	}
	
	/**
	 * deletes an element if it follows itself.
	 */
	private void deleteFollows() {
		for (int i = this.allPaths.size() - 1; i >= 0; i--) {
			for (int j = this.allPaths.get(i).size() - 1; j > 0; j--) {
				if (this.allPaths.get(i).get(j) == this.allPaths.get(i).get(j - 1)) {
					this.allPaths.get(i).remove(j);
				}
			}
		}
	}
	
	/**
	 * deletes duplicated part-paths at end of a path if there is one.
	 */
	private void deletePartPaths() {
		boolean found = false;
		for (int i = this.allPaths.size() - 1; i > 0; i--) {
			found = false;
			for (int j = i - 1; j >= 0; j--) {
				if (!found) {
					if (this.allPaths.get(i).size() < this.allPaths.get(j).size()) {
					found = compare(i, j);
					} else {
						found = compare(j, i);
					}
				}
			}
		}
	}
	
	/**
	 * Compares two lists whether the first is a part path of the second or not.
	 * @param i the index of the first list in in the resultList ArrayList
	 * @param j the index of the second list in in the resultList ArrayList
	 * @return true if the first list is a part path of the second list, false otherwise
	 */
	private boolean compare(final int i, final int j) {
		boolean possibleResult = false;
		if (this.allPaths.get(i).size() > this.allPaths.get(j).size()) {
			return false;
		}
		
		possibleResult = true;
		for (int l = 0; l <= this.allPaths.get(i).size() - 1; l++) {
		    if (this.allPaths.get(i).get(l) != this.allPaths.get(j).get(l)) {
		    	possibleResult = false;
		    }
		}
		
		if (possibleResult) {
			this.allPaths.remove(i);
		}
		return possibleResult;
	}
		
}