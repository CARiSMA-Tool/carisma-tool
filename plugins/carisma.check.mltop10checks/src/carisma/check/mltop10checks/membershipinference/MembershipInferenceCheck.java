package carisma.check.mltop10checks.membershipinference;

import java.util.Arrays;
import java.util.Set;

import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Deployment;
import org.eclipse.uml2.uml.Node;

import carisma.check.mltop10checks.common.AbstractMLTop10Check;
import carisma.core.checks.CarismaCheckWithID;
import carisma.profile.umlsec.mltop10.MLTop10;
import carisma.profile.umlsec.mltop10.MLTop10Util;

/**
 * analyzes a deployment diagram with respect to membership inference rules.
 * 
 * @author Alexander Peikert
 * @author Julian Flake
 *
 */

public class MembershipInferenceCheck extends AbstractMLTop10Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.membershipinference";
	public static final String CHECK_NAME = "MLTop10 Membership Inference Attack Check";

	@Override
	public boolean runCheck() {

		// ---------------------------------------------------------
		// populate collections
		Set<Artifact> mlmodels = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.MLModel);
		Set<Artifact> aialgorithms = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.AIAlgorithm);
		Set<Artifact> trainingdatas = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.TrainingData);
		Set<CommunicationPath> commPaths = MLTop10Util.getAllElementsOfType(modelEl, CommunicationPath.class);
		Set<Dependency> dependencies = MLTop10Util.getAllElementsOfType(modelEl, Dependency.class);
		Set<Deployment> deployments = MLTop10Util.getAllElementsOfType(modelEl, Deployment.class);

		// ---------------------------------------------------------
		// Check numbers of elements relevant for this check
		this.addInfo("Your model contains " + aialgorithms.size() + " AI Algorithm(s), " + mlmodels.size()
				+ " ML Model(s) and " + trainingdatas.size() + " Training Data(s).");
		if (mlmodels.isEmpty()) {
			this.addWarning("Your model does not contain any information about the ML Model."
					+ " Therefore, it cannot be checked whether the system implements mitigations against 'Membership Inference Attacks'.");
		}
		if (aialgorithms.isEmpty()) {
			this.addWarning("Your model does not contain any information about the AI Algorithms."
					+ " Therefore, it cannot be checked whether the system implements mitigations against 'Membership Inference Attacks'.");
		}
		if (trainingdatas.isEmpty()) {
			this.addWarning("Your model does not contain any information about the Training Data."
					+ " Therefore, it cannot be checked whether the system implements mitigations against 'Membership Inference Attacks'.");
		}

		// ---------------------------------------------------------
		// Check the specific prevention mechanisms
		// ---------------------------------------------------------

		// ---------------------------------------------------------
		// Pre Checks. Public training data and secret communication channels
		for (Artifact el : trainingdatas) {
			if (MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "Public")) {
				this.addWarning("Pre a. Your Training Data '" + el.getName()
						+ "' is public. Membership Inference does not need to be checked.");
				return true;
			}
		}

		for (Dependency dep : dependencies) {
			if (!(dep.getClients().get(0) instanceof Artifact) || !(dep.getSuppliers().get(0) instanceof Artifact)) {
				continue;
			}
			Artifact from = (Artifact) dep.getClients().get(0);
			Artifact to = (Artifact) dep.getSuppliers().get(0);
			// dependency from ai app to ml model or vice versa
			if (trainingdatas.contains(from) || (trainingdatas.contains(to))) {
				// get relevant nodes
				Node fromNode = MLTop10Util.getDeploymentNode(from, deployments);
				Node toNode = MLTop10Util.getDeploymentNode(to, deployments);
				// check paths between nodes
				for (CommunicationPath path : commPaths) {
					if (MLTop10Util.getMemberNodes(path).containsAll(Arrays.asList(fromNode, toNode))) {
						// secrecy on relevant paths?
						if (!MLTop10Util.hasStereotype(path, MLTop10.Secrecy)) {
							this.addError("Pre b. There is a dependency between '" + from.getName() + "' and '"
									+ to.getName()
									+ "', but the communication path between the nodes these artifacts are deployed to, does not fulfill 'secrecy'.");
						}
					}
				}
			}
		}

		// ---------------------------------------------------------
		// 1. Model training on randomized or shuffled data
		for (Artifact el : aialgorithms) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.AIAlgorithm, "Randomize")) {
				this.addError("1. AI Algorithm '" + el.getName() + "' does not randomize during training.");
			}
		}

		// ---------------------------------------------------------
		// 2. Model Obfuscation
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "Obfuscation")) {
				this.addError("2. ML Model '" + el.getName() + "' does not obfuscate its predictions.");
			}
		}

		// ---------------------------------------------------------
		// 3. Regularisation
		for (Artifact el : aialgorithms) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.AIAlgorithm, "Regularisation")) {
				this.addError("3. AI Algorithm '" + el.getName() + "' does not use regularizaiton techniques.");
			}
		}

		// ---------------------------------------------------------
		// 4. Reducing the training data
		for (Artifact el : trainingdatas) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "Reduced")) {
				this.addError("4. Training Data '" + el.getName() + "' is not reduced.");
			}
		}

		// ---------------------------------------------------------
		// 5. Testing and monitoring
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "RegularTestingAndMonitoring")) {
				this.addError("5. ML Model '" + el.getName() + "' is not regularly tested and monitored.");
			}
		}

		// ---------------------------------------------------------
		// Overall result
		if (this.errorDetected) {
			this.addError("=> A Membership Inference Attack is potentially possible!");
			return false;
		}
		this.addInfo("=> No vulnerabilities for Membership Inference Attacks detected.");
		return true;
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