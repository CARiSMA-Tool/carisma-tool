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
package carisma.modeltype.uml2;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.DeployedArtifact;
import org.eclipse.uml2.uml.Deployment;
import org.eclipse.uml2.uml.DeploymentTarget;
import org.eclipse.uml2.uml.Manifestation;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Relationship;
import org.eclipse.uml2.uml.Usage;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;


public final class UMLDeploymentHelper {
	
	/**
	 * Hiding the constructor.
	 */
	private UMLDeploymentHelper() {
		
	}
	
	
	/**
	 * Filters the special dependencies and returns the set of "normal"
	 * dependencies in the package.
	 * @param pkg - the package to check
	 * @return - set of "normal" dependencies
	 */
	public static Set<Dependency> getAllDependencies(final Package pkg) {
		Set<Dependency> allDependencies = new HashSet<>();
		for (Dependency d : UMLHelper.getAllElementsOfType(pkg, Dependency.class)) {
			if (!(d instanceof Deployment || d instanceof Usage || d instanceof Manifestation)) {
				allDependencies.add(d);
			}
		}
		return allDependencies;
	}
	/**
	 * Returns all dependencies the artifact is client or supplier of.
	 * @param artifact - the artifact to check
	 * @return - list of dependencies
	 */
	public static List<Dependency> getAllDependencies(final Artifact artifact) {
		List<Dependency> allDependencies = new ArrayList<>();
		for (Dependency dep : getAllDependencies(artifact.getModel())) {
			if (dep.getClients().contains(artifact) || dep.getSuppliers().contains(artifact)) {
				allDependencies.add(dep);
			}
		}
		return allDependencies;
	}
	
	public static Set<Artifact> getAllArtifacts(final Collection<Dependency> dependencies) {
		Set<Artifact> allArtifacts = new HashSet<>();
		for (Dependency dep : dependencies) {
			allArtifacts.addAll(getAllArtifacts(dep));
		}
		return allArtifacts;
	}
	
	/**
	 * Given a collection of dependencies, this returns all artifacts that are supplier or client of
	 * a dependency in the set.
	 * @param dependencies - the dependencies to check
	 * @return - the set of artifacts
	 */
	public static Set<Artifact> getAllArtifacts(final Dependency dep) {
		Set<Artifact> allArtifacts = new HashSet<>();
		for (NamedElement ne : dep.getClients()) {
			if (ne instanceof Artifact) {
				allArtifacts.add((Artifact) ne);
			}
		}
		for (NamedElement ne : dep.getSuppliers()) {
			if (ne instanceof Artifact) {
				allArtifacts.add((Artifact) ne);
			}
		}		
		return allArtifacts;
	}
	/**
	 * Returns the nodes which have no communication path between them even
	 * though the presence of the dependency would mandate that.
	 * @param dep - the dependency to check
	 * @return - the map of nodes with no communication path between them
	 */
	public static Map<Node, Node> getUnconnectedNodes(final Dependency dep) {
		Map<Node, Node> unconnectedNodes = new HashMap<>();
		Set<Artifact> clientArtifacts = new HashSet<>();
		Set<Artifact> supplierArtifacts = new HashSet<>();
		for (NamedElement ne : dep.getClients()) {
			if (ne instanceof Artifact) {
				clientArtifacts.add((Artifact) ne);
			}
		}
		for (NamedElement ne : dep.getSuppliers()) {
			if (ne instanceof Artifact) {
				supplierArtifacts.add((Artifact) ne);
			}
		}
		Set<Node> clientNodes = null;
		for (Artifact client : clientArtifacts) {
			clientNodes = getDeploymentLocations(client);
		}
		if(clientNodes==null){
			System.err.println("carisma.modeltype.uml2.UMLDeploymentHelper: clientnodes is null.");
			return Collections.emptyMap();
		}
		Set<Node> supplierNodes = null;
		for (Artifact supplier : supplierArtifacts) {
			supplierNodes = getDeploymentLocations(supplier);
		}
		for (Node clientNode : clientNodes) {
			for (Node supplierNode : supplierNodes) {
				if (getCommunicationPath(clientNode, supplierNode) == null) {
					unconnectedNodes.put(clientNode, supplierNode);
				}
			}
		}
		return unconnectedNodes;
	}
	
	/**
	 * Given the client and supplier artifact of a dependency,
	 * this method returns the corresponding communication paths
	 * between the various deployment locations in the deployment diagram.
	 * @param client - client artifact of the dependency
	 * @param supplier - supplier artifact of the dependency
	 * @return - the set of corresponding communication paths
	 */
	public static Set<CommunicationPath> getCommunicationPaths(final Artifact client, final Artifact supplier) {
		Set<CommunicationPath> communicationPaths = new HashSet<>();
		Set<Node> sourceNodes = getDeploymentLocations(client);
		Set<Node> targetNodes = getDeploymentLocations(supplier);
		for (Node sourceNode : sourceNodes) {
			for (Node targetNode : targetNodes) {
				CommunicationPath path = getCommunicationPath(sourceNode, targetNode);
				if(path!=null){
					communicationPaths.add(path);
				}
			}
		}
		return communicationPaths;
	}
	/**
	 * Given the source and target node, the communication path between them
	 * (if it exists) is returned.
	 * @param sourceNode - the source node
	 * @param targetNode - the target node
	 * @return - communication path between source and target node
	 */
	public static CommunicationPath getCommunicationPath(final Node sourceNode, final Node targetNode) {
		if (sourceNode == null || targetNode == null) {
			return null;
		}
		for (CommunicationPath commPath : sourceNode.getCommunicationPaths()) {
			boolean sourceFound = false;
			boolean targetFound = false;
			for (Property memberEnd : commPath.getMemberEnds()) {
				if (memberEnd.getType().equals(sourceNode)) {
					sourceFound = true;
				} else if (memberEnd.getType().equals(targetNode)) {
					targetFound = true;
				}
				if (sourceFound && targetFound) {
					return commPath;
				}
			}
		}
		return null;
	}
	/**
	 * Given a dependency, the corresponding communication paths between
	 * all clients and suppliers, or rather between the nodes these are
	 * deployed on are returned.
	 * @param dep - the dependency to search communication paths for
	 * @return - the set of communication paths, if present
	 */
	public static Set<CommunicationPath> getCommunicationPaths(final Dependency dep) {
		Set<CommunicationPath> communicationPaths = new HashSet<>();
		for (NamedElement aClient : dep.getClients()) {
			if (aClient instanceof Artifact) {
				for (NamedElement aSupplier : dep.getSuppliers()) {
					if (aSupplier instanceof Artifact) {
						communicationPaths.addAll(getCommunicationPaths((Artifact) aClient, (Artifact) aSupplier));						
					}
				}
			}
		}
		return communicationPaths;
	}
	
	/**
	 * Returns the source and target nodes of the communication
	 * path.
	 * @param commPath - the path to find the nodes for
	 * @return - the set of nodes
	 */
	public static List<Node> getNodes(final CommunicationPath commPath) {
		List<Node> nodes = new ArrayList<>();
		if (commPath.getMemberEnds().size() > 2) {
			Logger.log(LogLevel.WARNING, "Warning! Only 1:1 associations are supported.");
		}
		if (commPath.getMemberEnds().size() >= 2) { 
			Property oneEnd = commPath.getMemberEnds().get(0);
			Property otherEnd = commPath.getMemberEnds().get(1);
			if (oneEnd != null && otherEnd != null) {
				nodes.add((Node) oneEnd.getType());
				nodes.add((Node) otherEnd.getType());
			}
		}
		return nodes;
	}
	
	/**
	 * Returns all dependencies going in both directions
	 * from and to the source node.
	 * @param node1 - first node
	 * @param node2 - second node
	 * @return - all dependencies between the nodes
	 */
	public static Set<Dependency> getAllDependencies(
			final Node node1, final Node node2) {
		Set<Dependency> allDependencies = new HashSet<>();
		allDependencies.addAll(getClientDependencies(node1, node2));
		allDependencies.addAll(getClientDependencies(node2, node1));
		return allDependencies;
	}
	
	/**
	 * Returns all dependencies corresponding to the given communication path.
	 * @param commPath - the given communication path
	 * @return - the set of dependencies corresponding to the path
	 */
	public static Set<Dependency> getAllDependencies(
			final CommunicationPath commPath) {
		Set<Dependency> allDependencies = new HashSet<>();
		List<Node> nodes = getNodes(commPath);
		if (nodes.size() == 2) {
			allDependencies.addAll(
					getAllDependencies(nodes.get(0), nodes.get(1)));			
		}
		return allDependencies;
	}
	/**
	 * Returns all dependencies whose client is deployed in the source node
	 * and target is deployed in the target node.
	 * @param sourceNode - source node (client node) for the dependencies 
	 * @param targetNode - target node (supplier node) for the dependencies
	 * @return - list of client dependencies
	 */
	public static Set<Dependency> getClientDependencies(
			final Node sourceNode, final Node targetNode) {
		Set<Dependency> clientDependencies = new HashSet<>();
		Set<DeployedArtifact> sourceArtifacts = getDeployedArtifacts(sourceNode);
		Set<DeployedArtifact> targetArtifacts = getDeployedArtifacts(targetNode);
		for (DeployedArtifact sourceArtifact : sourceArtifacts) {
			for (Dependency clientDep : sourceArtifact.getClientDependencies()) {
				if (!clientDependencies.contains(clientDep)) {
					for (NamedElement supplier : clientDep.getSuppliers()) {
						if (targetArtifacts.contains(supplier)) {
							clientDependencies.add(clientDep);
						}
					}
				}
			}
		}
		return clientDependencies;
	}
	
	/**
	 * Retrieves a list of deployed artifacts on the given node.
	 * @param deploymentLocation - the node to check
	 * @return - list of deployed artifacts
	 */
	public static Set<DeployedArtifact> getDeployedArtifacts(final Node deploymentLocation) {
		Set<DeployedArtifact> deployedArtifacts = new HashSet<>();
		for (Deployment deploy : deploymentLocation.getDeployments()) {
			for (DeployedArtifact deplArti : deploy.getDeployedArtifacts()) {
				deployedArtifacts.add(deplArti);
			}
		}
		return deployedArtifacts;
	}
	
	public static Set<Node> getDeploymentLocations(final Collection<Artifact> artifacts) {
		Set<Node> deploymentLocations = new HashSet<>();
		for (Artifact arti : artifacts) {
			deploymentLocations.addAll(getDeploymentLocations(arti));
		}
		return deploymentLocations;
	}
	
	/**
	 * Retrieves all nodes where the artifact is deployed.
	 * @param deployedArtifact - the artifact to check
	 * @return - the deployment locations
	 */
	public static Set<Node> getDeploymentLocations(final Artifact deployedArtifact) {
		Set<Node> deploymentLocations = new HashSet<>();
		for (Deployment deploy : getDeployments(deployedArtifact)) {
			DeploymentTarget target = deploy.getLocation();
			if (target instanceof Node) {
				deploymentLocations.add((Node) target);
			}
		}
		return deploymentLocations;
	}
	/**
	 * Retrieves all Deployment dependencies for the given artifact.
	 * @param deployedArtifact - the deployed artifact
	 * @return - set of deployment relations
	 */
	public static Set<Deployment> getDeployments(final Artifact deployedArtifact) {
		Set<Deployment> deployments = new HashSet<>();
		for (Relationship relation : deployedArtifact.getRelationships()) {
			if (relation instanceof Deployment) {
				deployments.add((Deployment) relation);
			}
		}
		return deployments;
	}
}
