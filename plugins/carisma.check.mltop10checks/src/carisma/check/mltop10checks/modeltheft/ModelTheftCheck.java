package carisma.check.mltop10checks.modeltheft;

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
 * analyzes an deployment diagram with respect to model theft rules.
 * 
 * @author Alexander Peikert
 * @author Julian Flake
 *
 */

public class ModelTheftCheck extends AbstractMLTop10Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.modeltheft";
	public static final String CHECK_NAME = "MLTop10 Model Theft Attack Check";
	public static final String THREAT_COMMENT_ATTRIBUTE = "ModelTheft";
	public static final String THREAT_NAME = "MLTop10 Model Theft Attack";

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
		this.addInfo("Your model contains " + trainingdatas.size() + " Training Data(s), " + mlmodels.size()
				+ " ML Model(s) and " + aialgorithms.size() + " AI Algorithm(s).");
		if (mlmodels.isEmpty()) {
			this.addInfo(
					"Your model does not contain any information on the ML Model. Therefore, it cannot be ensured that your scenario contains mitigations against the threat 'Model Theft Attack'.");
		}
		if (aialgorithms.isEmpty()) {
			this.addInfo(
					"Your model does not contain any information on the AI Algorithm. Therefore, it cannot be ensured that your scenario contains mitigations against 'Model Theft Attack'.");
		}
		if (trainingdatas.isEmpty()) {
			this.addInfo(
					"Your model does not contain any information on the Training Data. Therefore, it cannot be ensured that your scenario contains mitigations against 'Model Theft Attack'.");
		}

		// ---------------------------------------------------------
		// Check the specific prevention mechanisms
		// ---------------------------------------------------------

		// ---------------------------------------------------------
		// Pre Check. Public model, training data and algorithm
		for (Artifact el : mlmodels) {
			if (MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "Public")) {
				this.addWarning("Pre Check. ML Model '" + el.getName() + "' is public.");
			}
		}
		for (Artifact el : aialgorithms) {
			if (MLTop10Util.isTaggedValueTrue(el, MLTop10.AIAlgorithm, "Public")) {
				this.addWarning("Pre Check. AI Algorithm '" + el.getName() + "' is public.");
			}
		}
		for (Artifact el : trainingdatas) {
			if (MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "Public")) {
				this.addWarning("Pre Check. Training Data '" + el.getName() + "' is public.");
			}
		}

		// ---------------------------------------------------------
		// 1. Encryption
		for (Dependency dep : dependencies) {
			if (!(dep.getClients().get(0) instanceof Artifact) || !(dep.getSuppliers().get(0) instanceof Artifact)) {
				continue;
			}
			Artifact from = (Artifact) dep.getClients().get(0);
			Artifact to = (Artifact) dep.getSuppliers().get(0);
			// dependency from ai algo to ml model or vice versa
			if ((aialgorithms.contains(from) && mlmodels.contains(to))
					|| (mlmodels.contains(from) && aialgorithms.contains(to))) {
				// get relevant nodes
				Node fromNode = MLTop10Util.getDeploymentNode(from, deployments);
				Node toNode = MLTop10Util.getDeploymentNode(to, deployments);
				// check paths between nodes
				for (CommunicationPath path : commPaths) {
					if (MLTop10Util.getMemberNodes(path).containsAll(Arrays.asList(fromNode, toNode))) {
						// secrecy on relevant paths?
						if (!MLTop10Util.isTaggedValueTrue(path, MLTop10.SecureCommPath, "ConfidelityPreserving")) {
							this.addWarning("1. There is a dependency between " + from.getName() + " and " + to.getName()
									+ ", but the communication path between the nodes these artifacts are deployed to, does not fulfill 'secrecy'.");
						}
					}
				}
			}
		}

		// ---------------------------------------------------------
		// 2. Access Control
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "AccessControl")) {
				this.addWarning("2. ML Model '" + el.getName() + "' does not implement access control.");
			}
		}
		for (Artifact el : aialgorithms) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.AIAlgorithm, "AccessControl")) {
				this.addWarning("2. AI Algorithm '" + el.getName() + "' does not implement access control.");
			}
		}
		for (Artifact el : trainingdatas) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "AccessControl")) {
				this.addWarning("2. Training Data '" + el.getName() + "' does not implement access control.");
			}
		}

		// ---------------------------------------------------------
		// 3. Regular backups
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "RegularBackup")) {
				this.addWarning("3. ML Model '" + el.getName() + "' does not do regular backups.");
			}
		}
		for (Artifact el : trainingdatas) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "RegularBackup")) {
				this.addWarning("3. Training Data '" + el.getName() + "' does not do regular backups.");
			}
		}

		// ---------------------------------------------------------
		// 4. Model Obfuscation
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "Obfuscation")) {
				this.addWarning("4. ML Model '" + el.getName() + "' is not obfuscated.");
			}
		}

		// ---------------------------------------------------------
		// 5. Watermarking
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "Watermarking")) {
				this.addWarning("5. ML Model '" + el.getName() + "' is not watermarked.");
			}
		}

		// ---------------------------------------------------------
		// 6. Legal protection
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "LegalProtection")) {
				this.addWarning("6. ML Model '" + el.getName() + "' is not legally protected.");
			}
		}

		// ---------------------------------------------------------
		// 7. Monitoring and auditing
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "RegularAuditAndMonitoring")) {
				this.addWarning("7. ML Model '" + el.getName() + "' is not regulary audited and monitored.");
			}
		}

		// ---------------------------------------------------------
		// Overall result

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

	@Override
	public String getThreatCommentAttribute() {
		return THREAT_COMMENT_ATTRIBUTE;
	}
	
	@Override
	public String getThreatName() {
		return THREAT_NAME;
	}

}
