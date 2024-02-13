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
import org.eclipse.uml2.uml.InitialNode;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.modeltype.uml2.UMLHelper;


/**
 *  @author Klaus Rudack
 *  @version 2.0
 */

public class ActivityDiagramManager {
	
	/**
	 * list with all possible paths in it.
	 */
	private List<List<Element>> resultList = new ArrayList<>();
	
	/**
	 * the given model.
	 */
	private Package model = null;
	
	/**
	 * AnalysisHost for report.
	 */
	private AnalysisHost host;

	/**
	 * constructor.
	 * @param model the model to be analyzed
	 * @param host AnalysisHost for report
	 */
	public ActivityDiagramManager(final Package model, final AnalysisHost host) {
		if (model == null) {
			AnalysisResultMessage detail = new AnalysisResultMessage(StatusType.WARNING, "internal error");
			host.addResultMessage(detail);
			throw new IllegalArgumentException("the given Model is null.");
		}
		this.model = model;
		if (host == null) {
			this.host = new DummyHost(true);
		} else {
			this.host = host;
		}
	}
	
	/**
	 * constructor without host.
	 * @param model the given model
	 */
	public ActivityDiagramManager(final Package model) {
		this(model, null);
	}
	
	/**
	 * main function that starts the analysis.
	 * @return all possible paths
	 */
	public final List<List<Element>> getAllPaths() {
		for (Element element : getInitalNodes()) {
			List<Element> l = new ArrayList<>();
			l.add(element);
			getPaths(l);
		}
		deleteDoubles();
		deleteFollows();
		return this.resultList;
	}
	
	/**
	 * gets all the InitialNodes where a path could start.
	 * this function returns only those nodes that lie directly in the diagram an not in a sub-diagram
	 * @return the InitialNodes
	 */
	private List<Element> getInitalNodes() {
		List<Element> returnList = new ArrayList<>();
		for (Element element : UMLHelper.getAllElementsOfType(this.model, InitialNode.class)) {
			if ((element.getOwner().getOwner() == this.model) || (element.getOwner().getOwner().getOwner() == this.model)) {
				returnList.add(element);
			}
		}
		return returnList;
	}
	
	/**
	 * gets all possible paths, beginning with the given path.
	 * @param list with the first part-path
	 */
	private void getPaths(final List<Element> list) {
		Element last = list.get(list.size() - 1);
		if (last instanceof ForkNode) {
			ParallelPath parallelPath = new ParallelPath();
			List<List<Element>> parallelList = parallelPath.getParallelPaths((ForkNode) last, this.host);
			for (List<Element> path : parallelList) {
				List<Element> newList = new ArrayList<>();
				newList.addAll(list);
				newList.remove(newList.size() - 1);
				newList.addAll(path);
				getPaths(newList);
			}
		} else {
			try {
    			if (!((ActivityNode) last).getOutgoings().isEmpty()) {
    				for (Element follower : ((ActivityNode) last).getOutgoings()) {
    						if (!(contains(list, ((ActivityEdge) follower).getTarget()))) {
    							List<Element> newPath = new ArrayList<>();
    							newPath.addAll(list);
    							newPath.add(((ActivityEdge) follower).getTarget());
    							getPaths(newPath);
    						} else {
    							List<Element> cutted = new ArrayList<>();
    							Element newAcc = list.get(list.size() - 1);
    							cutted.addAll(list);
    							cutted.remove(cutted.size() - 1);
    							cutted = cutList(cutted, newAcc);
    							this.resultList.add(cutted);
    						}
    				}
    			} else {
    				this.resultList.add(list);
    			}
			} catch (NullPointerException exception) {
				this.host.appendLineToReport("Diagram is not correct!\n" + ((NamedElement) last).getName() + " has one incorrect outgoing transition");
			}
			
		}
	}
	
	/**
	 * proofs if an element is already in a list.
	 * @param list list where the element could be in
	 * @param element element that could be in a list
	 * @return boolean if there is a loop in the end of the path
	 *
	 */
	private static boolean contains(final List<Element> list, final Element element) {
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
	private static List<Element> cutList(final List<Element> list, final Element element) {
		List<Element> returnList = new ArrayList<>();
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
	 * creates an String with the results.
	 * @return the reultsString
	 */
	public final String debugOutput() {
		StringBuffer returnString = new StringBuffer();
		ActivityDiagramManager manager = new ActivityDiagramManager(this.model, this.host);
		List<List<Element>> paths = manager.getAllPaths();
		for (List<Element> list : paths) {
			for (Element element : list) {
				returnString.append("--> ");
				returnString.append(((NamedElement) element).getName());	
			}
			returnString.append("\n");
		}
		return returnString.toString();
	}
	
	/**
	 * deletes an element if it follows itself.
	 */
	private void deleteFollows() {
		for (int i = this.resultList.size() - 1; i >= 0; i--) {
			for (int j = this.resultList.get(i).size() - 1; j > 0; j--) {
				if (this.resultList.get(i).get(j) == this.resultList.get(i).get(j - 1)) {
					this.resultList.get(i).remove(j);
				}
			}
		}
	}
	
	/**
	 * deletes duplicated Paths.
	 *
	 */
	private void deleteDoubles() {		
		for (int i = this.resultList.size() - 1; i >= 0; i--) {
			for (int j = this.resultList.size() - 1; j >= 0; j--) {
				if (i != j 
						&& isDouble(this.resultList.get(i), this.resultList.get(j))) {
					this.resultList.remove(i);
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
	 * Checks if the given UML  element is of type action.
	 * @param elt the UML element
	 * @return truth value
	 */
	public final static boolean isAction(final Element elt) {
		if (elt instanceof org.eclipse.uml2.uml.OpaqueAction) {
			return true;
		}
		return false;
	}
	
	/**
	 * Checks if the given UML  element is of type decision node or merge node.
	 * @param elt the UML element
	 * @return truth value
	 */
	public final static boolean isDecisionOrMergeNode(final Element elt) {
		return isDecisionNode(elt) || isMergeNode(elt);
	}
	
	/**
	 * Checks if the given UML  element is of type decision node.
	 * @param elt the UML element
	 * @return truth value
	 */
	public final static boolean isDecisionNode(final Element elt) {
		if (elt instanceof org.eclipse.uml2.uml.DecisionNode) {
			return true;
		}
		return false;
	}
	
	/**
	 * Checks if the given UML  element is of type merge node.
	 * @param elt the UML element
	 * @return truth value
	 */
	public final static boolean isMergeNode(final Element elt) {
		if (elt instanceof org.eclipse.uml2.uml.MergeNode) {
			return true;
		}
		return false;
	}
	
	/**
	 * Checks if the given UML  element is of type initial node.
	 * @param elt the UML element
	 * @return truth value
	 */
	public final static boolean isInitialNode(final Element elt) {
		if (elt instanceof org.eclipse.uml2.uml.InitialNode) {
			return true;
		}
		return false;
	}
	
	
	/**
	 * Checks if the given UML  element is of type final node.
	 * @param elt the UML element
	 * @return truth value
	 */
	public final static boolean isFinalNode(final Element elt) {
		if (elt instanceof org.eclipse.uml2.uml.ActivityFinalNode) {
			return true;
		}
		return false;
	}
	
	
	/**
	 * Checks if the given UML  element is of type Edge.
	 * @param elt the UML element
	 * @return truth value
	 */
	public final static boolean isEdge(final Element elt) {
		if (elt instanceof org.eclipse.uml2.uml.ControlFlow) {
			return true;
		}
		return false;
	}
	
	/**
	 * Checks if the given UML  element is of type join node.
	 * @param elt the UML element
	 * @return truth value
	 */
	public final static boolean isJoinNode(final Element elt) {
		if (elt instanceof org.eclipse.uml2.uml.JoinNode) {
			return true;
		}
		return false;
	}
	
	/**
	 * Checks if the given UML  element is of type fork node.
	 * @param elt the UML element
	 * @return truth value
	 */
	public final static boolean isForkNode(final Element elt) {
		if (elt instanceof org.eclipse.uml2.uml.ForkNode) {
			return true;
		}
		return false;
	}
	
}

///**
// * @author Moussa ----------------------------------
// *         <code>this class helps us manage our diagram
// *         it takes a UML model and iterates through all activity diagrams. it gives 
// *         for each diagram all the possible paths. We can have it in a data
// *         structure or get it printed.
// *         </code>
// * 
// */
//public class ActivityDiagramManager {
//
//	/** the UML Model on which the user works */
//	private Package model;
//	/**
//	 * the initial node when there is only one diagram. in case of several
//	 * diagrams, it is no longer useful
//	 */
//	private InitialNode initialNode;
//	/** data structure which saves all the paths in one single diagram */
//	List<List<ActivityNode>> paths = new ArrayList<List<ActivityNode>>();
//	/** the list of nodes of the diagram */
//	List<ActivityNode> nodes = new ArrayList<ActivityNode>();
//
//	/**
//	 * constructor
//	 * 
//	 * @param model
//	 */
//	public ActivityDiagramManager(Package model) {
//		this.model = model;
//	}
//
//	/**
//	 * Getter method for the model
//	 * 
//	 * @return the model
//	 */
//	public Package getModel() {
//		return model;
//	}
//
//	/**
//	 * gets the initial node of the activity diagram. only useful in case of one
//	 * single diagram in a model in case of several diagrams, each diagram has
//	 * its own initial node
//	 * 
//	 * @return the initial node
//	 */
//	public InitialNode getInitialNode() {
//		if (initialNode == null) {
//			Element activity = getFirstElement();
//			for (Element element : activity.getOwnedElements()) {
//				if (element instanceof InitialNode) {
//					initialNode = (InitialNode) element;
//				}
//			}
//		}
//		return initialNode;
//	}
//
//	/**
//	 * this method gets the initial of one single diagram
//	 * 
//	 * @param diagram
//	 * @return the initial node of the given diagram
//	 */
//	public InitialNode getInitialNodeOfDiagram(Activity diagram) {
//		for (Element element : diagram.getOwnedElements()) {
//			if (element instanceof InitialNode) {
//				return (InitialNode) element;
//			}
//		}
//		return null;
//	}
//
//	/**
//	 * gets all the possible paths in an activity diagram !!! Only useful in
//	 * case of one single activity diagram in the model !!!
//	 * 
//	 * @return a list of list of activity nodes
//	 */
//	public List<List<ActivityNode>> getAllPaths() {
//		getAndFillPaths(nodes);
//		eliminateDuplicate(paths);
//		return paths;
//	}
//
//	/**
//	 * helps us get the activity diagram in case of one single activity diagram
//	 * in the model
//	 * 
//	 * @return the activity diagram as first element
//	 */
//	private Element getFirstElement() {
//		Element son = model.getMembers().get(0);
//		while (!(son instanceof Activity)) {
//			son = son.getOwnedElements().get(0);
//		}
//		return son;
//	}
//
//	
//	@SuppressWarnings("unchecked")
//	/**<code>
//	 * the method that does all the job. it
//	 * iterates through the diagram and get
//	 * all possible paths and store it in the 
//	 * given list. at the end the list is added
//	 * to the biggest list of paths. a path is
//	 * a list of nodes.
//	 * </code>
//	 */
//	private void getAndFillPaths(List<ActivityNode> nodeList) {
//		ActivityNode lastElement = null;
//		// ActivityNode lastElement = nodeList.get(nodeList.size() - 1);
//		if (nodeList.size() == 0) {
//			nodeList.add(getInitialNode());
//			lastElement = getInitialNode();
//		} else {
//			lastElement = nodeList.get(nodeList.size() - 1);
//		}
//		List<ActivityEdge> edges = lastElement.getOutgoings();
//		if (edges.size() > 0) {
//			for (ActivityEdge edge : edges) {
//				ActivityNode target = edge.getTarget();
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
//	}
//
//	@SuppressWarnings("unchecked")
//	/**
//	 * <code>
//	 * this method iterates through the given diagram and get
//	 * one single path and store it in the 
//	 * given list. at the end the list is added
//	 * to the biggest list of paths. a path is
//	 * a list of nodes. this method will be used in case
//	 * of several activity diagrams in our model
//	 * </code>
//	 */
//	private void getDiagramPaths(List<ActivityNode> nodeList, Activity diagram) {
//		ActivityNode lastElement = null;
//		// ActivityNode lastElement = nodeList.get(nodeList.size() - 1);
//		if (nodeList.size() == 0) {
//			nodeList.add(getInitialNodeOfDiagram(diagram));
//			lastElement = getInitialNodeOfDiagram(diagram);
//		} else {
//			lastElement = nodeList.get(nodeList.size() - 1);
//		}
//
//		if (lastElement != null) {
//			List<ActivityEdge> edges = lastElement.getOutgoings();
//
//			if (edges.size() > 0) {
//				for (ActivityEdge edge : edges) {
//					ActivityNode target = edge.getTarget();
//					List<ActivityNode> cloneList = (List<ActivityNode>) ((ArrayList<ActivityNode>) nodeList)
//							.clone();
//
//					if (containsManyTimes(cloneList, target, 1)) {
//						cloneList.add(target);
//						//if (cloneList.get(cloneList.size() - 1) instanceof ForkNode)
//						//{System.out.println("Found Forknode");}
//						System.out.println(cloneList.get(cloneList.size() - 1).getClass());
//						getAndFillPaths(cloneList);
//					}
//				}
//			} else {
//				paths.add(nodeList);
//			}
//		}
//	}
//
//	/**
//	 * how many times does the given list contain the given element. this method
//	 * helps us break the looping when the given element appears 'numberOfTimes'
//	 * times in the given list. With this method, it can be set how many times a
//	 * loop can be passed through in an activity diagram resulting in different
//	 * paths cycles.
//	 * 
//	 * @param list
//	 *            of nodes
//	 * @param node
//	 * @param numberOfTimes
//	 * @return true if the given list contains the given node at most
//	 *         'numberOfTimes' times otherwise false.
//	 */
//	private boolean containsManyTimes(List<ActivityNode> list,
//			ActivityNode node, int numberOfTimes) {
//		int count = 0;
//		for (ActivityNode node1 : list) {
//			if (node1.equals(node)) {
//				count++;
//			}
//		}
//		return (count <= numberOfTimes);
//	}
//
//	/**
//	 * eliminates duplicates in the list of list of activity nodes because all
//	 * paths in the activity diagram occurs two times in the list.
//	 * 
//	 * @param list
//	 *            of list of nodes
//	 */
//	private void eliminateDuplicate(List<List<ActivityNode>> list) {
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
//	 * Returns all activity diagrams of the given model
//	 * @return the list of activity diagrams
//	 */
//	public List <Activity> getActivitydiagrams(){
//		return getActivityDiagrams(model);
//	}
//	/**
//	 * Gets all the activity diagrams in the given model
//	 * 
//	 * @param element
//	 *            which represents here the model
//	 * @return the list of activity diagrams
//	 */
//	private List<Activity> getActivityDiagrams(Element element) {
//		List<Activity> activityList = new ArrayList<Activity>();
//		for (Element e : element.getOwnedElements()) {
//			if (e instanceof Activity) {
//				activityList.add((Activity) e);
//			}
//			activityList.addAll(getActivityDiagrams(e));
//		}
//		return activityList;
//	}
//
//	/**
//	 * Gets all the paths of the given diagram
//	 * 
//	 * @param the diagram for which the paths will be returned
//	 * @return the list of paths
//	 */
//	public List<List<ActivityNode>> getAllPathsOfDiagram(Activity diagram) {
//		List<List<ActivityNode>> list = new ArrayList<List<ActivityNode>>();
//		getDiagramPaths(nodes, diagram);
//		eliminateDuplicate(paths);
//		list.addAll(paths);
//		nodes.clear();
//		paths.clear();
//		return list;
//	}
//
//	/**
//	 * output of all the activity diagrams of the model
//	 * 
//	 * @return the string that represents all possible paths in the activity
//	 *         diagram.
//	 */
//	public String debugOutput() {
//		String result = "";
//		int diagramCount = 1;
//		for (Activity diagram : getActivityDiagrams(getModel())) {
//			result += "Activity Diagram No " + (diagramCount++) + "\n";   // unmappable character for encoding UTF8
//			List<List<ActivityNode>> listOfPaths = getAllPathsOfDiagram(diagram);
//			if (listOfPaths.size() == 0) {
//				result += "Diagram empty\n";
//			} else {
//				int pathCount = 1;
//				for (List<ActivityNode> actionsList : listOfPaths) {
//					result += "Path No " + (pathCount++) + "\n";   // unmappable character for encoding UTF8
//					for (ActivityNode action : actionsList) {
//						result += action.getName();
//						if (!(action instanceof FinalNode)) {
//							result += "--->";
//						}
//					}
//					result += "\n";
//				}
//			}
//			result += "\n";
//		}
//		return result;
//	}
//	
//	/**
//	 * Checks if the given UML  element is of type action.
//	 * @param elt the UML element
//	 * @return truth value
//	 */
//	public boolean isAction(final Element elt) {
//		if (elt instanceof org.eclipse.uml2.uml.OpaqueAction) {
//			return true;
//		}
//		return false;
//	}
//	
//	/**
//	 * Checks if the given UML  element is of type decision node or merge node.
//	 * @param elt the UML element
//	 * @return truth value
//	 */
//	public boolean isDecisionOrMergeNode(final Element elt) {
//		return isDecisionNode(elt) || isMergeNode(elt);
//	}
//	
//	/**
//	 * Checks if the given UML  element is of type decision node.
//	 * @param elt the UML element
//	 * @return truth value
//	 */
//	public boolean isDecisionNode(final Element elt) {
//		if (elt instanceof org.eclipse.uml2.uml.DecisionNode) {
//			return true;
//		}
//		return false;
//	}
//	
//	/**
//	 * Checks if the given UML  element is of type merge node.
//	 * @param elt the UML element
//	 * @return truth value
//	 */
//	public boolean isMergeNode(final Element elt) {
//		if (elt instanceof org.eclipse.uml2.uml.MergeNode) {
//			return true;
//		}
//		return false;
//	}
//	
//	/**
//	 * Checks if the given UML  element is of type join node.
//	 * @param elt the UML element
//	 * @return truth value
//	 */
//	public boolean isJoinNode(final Element elt) {
//		if (elt instanceof org.eclipse.uml2.uml.JoinNode) {
//			return true;
//		}
//		return false;
//	}
//	
//	/**
//	 * Checks if the given UML  element is of type fork node.
//	 * @param elt the UML element
//	 * @return truth value
//	 */
//	public boolean isForkNode(final Element elt) {
//		if (elt instanceof org.eclipse.uml2.uml.ForkNode) {
//			return true;
//		}
//		return false;
//	}
//	
//	/**
//	 * Checks if the given UML  element is of type initial node.
//	 * @param elt the UML element
//	 * @return truth value
//	 */
//	public boolean isInitialNode(final Element elt) {
//		if (elt instanceof org.eclipse.uml2.uml.InitialNode) {
//			return true;
//		}
//		return false;
//	}
//	
//	
//	/**
//	 * Checks if the given UML  element is of type final node.
//	 * @param elt the UML element
//	 * @return truth value
//	 */
//	public boolean isFinalNode(final Element elt) {
//		if (elt instanceof org.eclipse.uml2.uml.ActivityFinalNode) {
//			return true;
//		}
//		return false;
//	}
//	
//	
//	/**
//	 * Checks if the given UML  element is of type Edge.
//	 * @param elt the UML element
//	 * @return truth value
//	 */
//	public boolean isEdge(final Element elt) {
//		if (elt instanceof org.eclipse.uml2.uml.ControlFlow) {
//			return true;
//		}
//		return false;
//	}
//	
//}
