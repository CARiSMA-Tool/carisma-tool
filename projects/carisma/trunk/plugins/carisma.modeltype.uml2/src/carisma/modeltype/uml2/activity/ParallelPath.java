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
package carisma.modeltype.uml2.activity;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.uml2.uml.ActivityEdge;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.ForkNode;
import org.eclipse.uml2.uml.JoinNode;
import org.eclipse.uml2.uml.NamedElement;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;



/**
 * Class to manage parallel paths.
 *
 * @author Klaus Rudack
 * @version 1.0
 */


public class ParallelPath {
	
	/**
	 * list with the parallel paths.
	 */
	private List<List<Element>> results = new ArrayList<List<Element>>();
	
	/**
	 * list with paths that have to be merged.
	 */
	private List<List<Element>> mergeList = new ArrayList<List<Element>>();
		
	/**
	 * the JoinNode where the parallelization ends.
	 */
	private JoinNode endsWith = null;
	
	/**
	 * AnalysisHost for report.
	 */
	private AnalysisHost host = null;
	
	/**
	 * main function to start the parallelization.
	 * @param forkNode the fork Node where the parallelization starts
	 * @param host AnalysisHost for report
	 * @return the parallel paths
	 */
	public final List<List<Element>> getParallelPaths(final ForkNode forkNode, final AnalysisHost host) {
		if (host == null) {
			this.host = new DummyHost(true);
		} else {
			this.host = host;
		}
		for (Element start : forkNode.getOutgoings()) {
			try {
				List<Element> startList = new ArrayList<Element>();
				startList.add(((ActivityEdge) start).getTarget());
				getPaths(startList);
			} catch (NullPointerException exception) {
				host.appendLineToReport("Diagram is not correct!\n" + forkNode.getName() + " has one incorrect outgoing transition");
			}
		}

		try {
			endsWith = (JoinNode) mergeList.get(0).get(mergeList.get(0).size() - 1);
		} catch (Exception exception) {
			host.appendLineToReport("No correct diagram");
		}
		for (int i = mergeList.size() - 1; i >= 0; i--) {
			if (mergeList.get(i).size() < 1) {
				mergeList.remove(i);
			}
		}
		mergePaths();
		for (int i = results.size() - 1; i >= 0; i--) {
			if (results.get(i).size() < 1) {
				results.remove(i);
			}
		}
		deleteDoubles();
		deleteFollows();
		return results;
	}
	
	/**
	 * gets alls the paths between a ForkNode and a JoinNode.
	 * @param list list with the first part-path
	 */
	private void getPaths(final List<Element> list) {
		Element last = list.get(list.size() - 1);
		if (last instanceof ForkNode) {
			ParallelPath parallelPath = new ParallelPath();
			List<List<Element>> parallelList = parallelPath.getParallelPaths((ForkNode) last, host);
			for (List<Element> path : parallelList) {
				List<Element> newList = new ArrayList<Element>();
				newList.addAll(list);
				newList.remove(newList.size() - 1);
				newList.addAll(path);
				getPaths(newList);
			}
		} else {
			if (last instanceof JoinNode) {
				mergeList.add(list);
			} else {
				try {
					if (((ActivityNode) last).getOutgoings().size() > 0) {
						for (Element follower : ((ActivityNode) last).getOutgoings()) {
							if (!(contains(list, ((ActivityEdge) follower).getTarget()))) {
								List<Element> newPath = new ArrayList<Element>();
								newPath.addAll(list);
								newPath.add(((ActivityEdge) follower).getTarget());
								getPaths(newPath);
							} else {
								List<Element> cutted = new ArrayList<Element>();
								Element newAcc = list.get(list.size() - 1);
								cutted.addAll(list);
								cutted.remove(cutted.size() - 1);
								cutted = cutList(cutted, newAcc);
								mergeList.add(cutted);
							}
						}
					} else {
						mergeList.add(list);
					}
				} catch (NullPointerException exception) {
					host.appendLineToReport("Diagram is not correct!\n" + ((NamedElement) last).getName() + " has one incorrect outgoing transition");
				}
			}
		}
	}
	
	/**
	 * Gets all possible paths out of parallel paths.
	 * 
	 */
	private void mergePaths() {
		
		for (int i = 0; i < mergeList.size(); i++) {
			mergeList.get(i).remove(mergeList.get(i).size() - 1);
		}
		Element acc = mergeList.get(mergeList.size() - 1).get(0);
		for (int i = mergeList.size() - 1; i >= 0; i--) {
			if (mergeList.get(i).get(0) == acc) {
				results.add(mergeList.get(i));
				mergeList.remove(i);
			}
		}
		while (mergeList.size() > 0) {
			acc = mergeList.get(mergeList.size() - 1).get(0);
			List<List<Element>> newMerge1 = new ArrayList<List<Element>>();
			List<List<Element>> newMerge2 = new ArrayList<List<Element>>();
			for (int i = mergeList.size() - 1; i >= 0; i--) {
				if (mergeList.get(i).get(0) == acc) {
					newMerge1.add(mergeList.get(i));
					mergeList.remove(i);
				}
			}
			newMerge2.addAll(results);
			results.clear();
			for (int i = 0; i < newMerge1.size(); i++) {
				for (int j = 0; j < newMerge2.size(); j++) {
					ArrayList<Element> r = new ArrayList<Element>();
					startMerge(newMerge1.get(i), newMerge2.get(j), r);
				}
			}
		}
		for (int i = 0; i < results.size(); i++) {
			results.get(i).add(endsWith);
		}
	}
	
	/**
	 * gets all possible paths through two parallel paths.
	 * 
	 * @param l1 path 1 to parallelize with path 2
	 * @param l2 path 2 to parallelize with path 1
	 * @param r path that is already parallel
	 */
	private void startMerge(final List<Element> l1, final List<Element> l2, final List<Element> r) {
	
		if ((l1.size() == 0) && (l2.size() == 0)) {
			results.add(r);
		}
		if ((l1.size() != 0) && (l2.size() == 0)) {
			List<Element> res = new ArrayList<Element>();
			res.addAll(r);
			res.addAll(l1);
			results.add(res);
		}
		if ((l1.size() == 0) && (l2.size() != 0)) {
			ArrayList<Element> res = new ArrayList<Element>();
			res.addAll(r);
			res.addAll(l2);
			results.add(res);
		}
		if ((l1.size() != 0) && (l2.size() != 0)) {
			List<Element> newR1 = new ArrayList<Element>();
			List<Element> newR2 = new ArrayList<Element>();
			List<Element> newL1 = new ArrayList<Element>();
			List<Element> newL2 = new ArrayList<Element>();
			newR1.addAll(r);
			newR2.addAll(r);
			newL1.addAll(l1);
			newL2.addAll(l2);
			newR1.add(newL1.get(0));
			newL1.remove(0);
			newR2.add(newL2.get(0));
			newL2.remove(0);
			startMerge(newL1, l2, newR1);
			startMerge(l1, newL2, newR2);
		}
	}
	
	/**
	 * proofs if an element is already in a list.
	 * @param list list where the element could be in
	 * @param element element that could be in a list
	 * @return boolean if there is a loop in the end of the path
	 *
	 */
	private boolean contains(final List<Element> list, final Element element) {
		boolean path = false;
		for (int i = list.size() - 1; i > 0; i--) {
			if (list.get(i) == element) {
				int l = 1;
				path = true;
				while ((l <= i) && (list.get(i - l) != element) && (path)) {
					if (list.get(i - l) != list.get(list.size() - l)) {
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
	 * @param list path with loop
	 * @param element element where the loop starts/ends
	 * @return list without loop at the end
	 */
	private List<Element> cutList(final List<Element> list, final Element element) {
		List<Element> returnList = new ArrayList<Element>();
		int count;
		
		returnList.addAll(list);
		count = returnList.size() - 1;
		while (returnList.get(count) != element) {
			returnList.remove(count);
			count--;
		}
		return returnList;
	}
	
	/**
	 * deletes an element if it follows itself.
	 */
	private void deleteFollows() {
		for (int i = results.size() - 1; i >= 0; i--) {
			for (int j = results.get(i).size() - 1; j > 0; j--) {
				if (results.get(i).get(j) == results.get(i).get(j - 1)) {
					results.get(i).remove(j);
				}
			}
		}
	}
	
	/**
	 * deletes duplicated Paths.
	 *
	 */
	private void deleteDoubles() {		
		for (int i = results.size() - 1; i >= 0; i--) {
			for (int j = results.size() - 1; j >= 0; j--) {
				if (i != j 
						&& isDouble(results.get(i), results.get(j))) {
					results.remove(i);
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
	private boolean isDouble(final List<Element> list1, final List<Element> list2) {
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
}
	
//	/**
//	 *  data structure which saves all the paths in one single diagram.
//	 */
//	private List<List<ActivityNode>> paths = new ArrayList<List<ActivityNode>>();
//	
//	/**
//	 * the list of nodes of the diagram.
//	 */
//	private List<ActivityNode> nodes = new ArrayList<ActivityNode>();
//
//	
//	/**
//	 * starts the parallelization.
//	 * @param list list with the path till the parallelization
//	 * @return list with all possible paths till the end of the parallelization
//	 */
//	public final List<List<ActivityNode>> getParallelPaths(final List<Element> list) {
//		nodes.addAll(list);
//		getAndFillPaths(nodes);
//		eliminateDuplicate(paths);
//	//	paths = mergePaths(paths);
//		return paths;
//	}
//	
//	
//	/**
//	 * 
//	 * @param nodeList List with the path till the beginning of the parallelization.
//	 */
//	private void getAndFillPaths(final List<ActivityNode> nodeList) {
//		ActivityNode lastElement = null;
//		// ActivityNode lastElement = nodeList.get(nodeList.size() - 1);
//		lastElement = nodeList.get(nodeList.size() - 1);
//		List<ActivityEdge> edges = lastElement.getOutgoings();
//		if (nodeList.get(nodeList.size() - 1) instanceof JoinNode) {
//			paths.add(nodeList);
//		} else {
//		if (edges.size() > 0) {
//			for (ActivityEdge edge : edges) {
//				ActivityNode target = edge.getTarget();
//				@SuppressWarnings("unchecked")
//				List<ActivityNode> cloneList = (List<ActivityNode>) ((ArrayList<ActivityNode>) nodeList)
//						.clone();
//
//				if (containsManyTimes(cloneList, target, 1)) {
//					cloneList.add(target);
//					if (target instanceof ForkNode) {
//						ParallelPath p = new ParallelPath();
//						List<List<ActivityNode>> result = p.getParallelPaths(cloneList);
//						for (List<ActivityNode> resultList : result) {
//							List<ActivityNode> newList = new ArrayList<ActivityNode>();
//							newList.addAll((Collection<? extends ActivityNode>) resultList);
//							getAndFillPaths(newList);
//						}
//					} else {
//						getAndFillPaths(cloneList);
//					}
//				}
//			}
//		} else {
//			paths.add(nodeList);
//		}
//		}
//	}
//	
//	
//		/**
//		 * how many times does the given list contain the given element. this method
//		 * helps us break the looping when the given element appears 'numberOfTimes'
//		 * times in the given list. With this method, it can be set how many times a
//		 * loop can be passed through in an activity diagram resulting in different
//		 * paths cycles.
//		 * 
//		 * @param list of nodes
//		 * @param node 
//		 * @param numberOfTimes 
//		 * @return true if the given list contains the given node at most
//		 *         'numberOfTimes' times otherwise false.
//		 */
//		private boolean containsManyTimes(final List<ActivityNode> list,
//				final ActivityNode node, final int numberOfTimes) {
//			int count = 0;
//			for (ActivityNode node1 : list) {
//				if (node1.equals(node)) {
//					count++;
//				}
//			}
//			return (count <= numberOfTimes);
//		}
//	
//	/**
//	 * Eliminates duplicated paths.
//	 * @param list list with paths
//	 */
//	private void eliminateDuplicate(final List<List<ActivityNode>> list) {
//		for (int i = 0; i < list.size(); i++) {
//			List<ActivityNode> nodeList = list.get(i);
//			for (int j = i + 1; j < list.size(); j++) {
//				if (nodeList.equals(list.get(j))) {
//					list.remove(j);
//				}
//			}
//		}
//	}
//	
//	/**
//	 * merges the different paths that will be executed parallel.
//	 * @param list the different paths that will be executed parallel
//	 */
//	/*private mergePaths(final List<List<ActivityNode>> list) {
//		List<List<ActivityNode>> results = new ArrayList<List<ActivityNode>>();
//		ForkNode forkNode = (ForkNode) list.get(0).get(0);
//		JoinNode joinNode = (JoinNode) list.get(0).get(list.get(0).size() - 1);
//		for (List<ActivityNode> l : list) {
//			l.remove(l.size() - 1);
//			l.remove(0);
//		}
//		while (list.size() > 0) {
//			Element element = list.get(0).get(0);
//			List<List<ActivityNode>> newList = new ArrayList<List<ActivityNode>>();
//			for (int i = list.size() - 1; i >= 0; i++) {
//				if (list.get(i).get(0) == element) {
//					newList.add(list.get(i));
//					list.remove(i);
//				}
//			}
//			results = mergePaths(results, newList);
//		}
//		for (List<ActivityNode> l : list) {
//			l.add(joinNode);
//			l.add(0, forkNode);
//		}
//	}*/
//
//	/**
//	 * merges the lists together.
//	 * @param results the old results                     
//	 * @param newList new paths to be merged in the results
//	 * @return the new results
//	 */
//	/*private List<List<ActivityNode>> mergePaths(final List<List<ActivityNode>> results, final List<List<ActivityNode>> newList) {
//		List<List<ActivityNode>> returnList = new ArrayList<List<ActivityNode>>();
//		if (results.size() < 1) {
//			return newList;
//		}
//		for (List<ActivityNode> l1 : newList) {
//			for(List<ActivityNode> l2 : newList) {
//				List<ActivityNode> rl = new ArrayList<ActivityNode>();
//				returnList.addAll(mergeParts(rl,l1, l2));
//			}
//		}
//		
//		return returnList;
//	}
//	
//	*/
///**
// * merges 2 lists.
// * @param first list that need to be merged with the second one
// * @param second list that need to be merged with the first one
// * @return the merges list
// */
///*	private List<List<ActivityNode>> mergeParts(List<ActivityNode> rl, List<ActivityNode> l1, List<ActivityNode> l2) {
//		if (l1.size() < 1) {
//			return (rl.addAll(l2));
//		}
//	}*/
//	
//}
