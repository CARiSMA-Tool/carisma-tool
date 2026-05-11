package carisma.check.mltop10checks.outputintegrity;

import java.util.Arrays;
import java.util.Set;

import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Deployment;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Node;

import carisma.check.mltop10checks.common.AbstractMLTop10Check;
import carisma.core.checks.CarismaCheckWithID;
import carisma.profile.umlsec.mltop10.MLTop10;
import carisma.profile.umlsec.mltop10.MLTop10Util;

/**
 * This check analyzes a deployment diagram with respect to output integrity
 * rules.
 * 
 * @author Alexander Peikert
 * @author Julian Flake
 *
 */

public class OutputIntegrityCheck extends AbstractMLTop10Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.outputintegrity";
	public static final String CHECK_NAME = "MLTop10 Output Integrity Attack Check";

	@Override
	public boolean runCheck() {

		// ---------------------------------------------------------
		// populate collections
		Set<NamedElement> aiscenarios = MLTop10Util.getStereotypedElements(modelEl, NamedElement.class,
				MLTop10.SecureAIScenario);
		Set<Artifact> mlmodels = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.MLModel);
		Set<Artifact> aiapplications = MLTop10Util.getStereotypedElements(modelEl, Artifact.class,
				MLTop10.AIApplication);
		Set<CommunicationPath> commPaths = MLTop10Util.getAllElementsOfType(modelEl, CommunicationPath.class);
		Set<Dependency> dependencies = MLTop10Util.getAllElementsOfType(modelEl, Dependency.class);
		Set<Deployment> deployments = MLTop10Util.getAllElementsOfType(modelEl, Deployment.class);

		// ---------------------------------------------------------
		// Check numbers of elements relevant for this check
		this.addInfo("Your model contains " + aiscenarios.size() + " Secure AI Scenario(s), " + mlmodels.size()
				+ " ML Model(s) and " + aiapplications.size() + " AI Application(s).");
		if (mlmodels.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the ML Model. Therefore, it cannot be ensured that your scenario contains mitigations against the threat 'Output Integrity Attack'.");
		}
		if (aiapplications.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the AI Application. Therefore, it cannot be ensured that your scenario contains mitigations against 'Output Integrity Attack'.");
		}
		if (aiscenarios.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the Secure AI Scenario. Therefore, it cannot be ensured that your scenario contains mitigations against 'Output Integrity Attack'.");
		}
		if (aiscenarios.size() > 1) {
			this.addWarning(
					"Your model contains more than one Secure AI Scenario Elements. Check, whether this is intended.");
		}

		// ---------------------------------------------------------
		// Check the specific prevention mechanisms
		// ---------------------------------------------------------

		// ---------------------------------------------------------
		// 1. Using Cryptographic Methods
		for (Artifact el : aiapplications) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.AIApplication, "CheckModelResultAuthenticity")) {
				this.addError("1: AI Application '" + el.getName() + "' does not check for model result authenticity.");
			}
		}

		// ---------------------------------------------------------
		// 2. Secure Communication Channels
		for (Dependency dep : dependencies) {
			if (!(dep.getClients().get(0) instanceof Artifact) || !(dep.getSuppliers().get(0) instanceof Artifact)) {
				continue;
			}
			Artifact from = (Artifact) dep.getClients().get(0);
			Artifact to = (Artifact) dep.getSuppliers().get(0);
			// dependency from ai app to ml model or vice versa
			if ((aiapplications.contains(from) && mlmodels.contains(to))
					|| (mlmodels.contains(from) && aiapplications.contains(to))) {
				// get relevant nodes
				Node fromNode = MLTop10Util.getDeploymentNode(from, deployments);
				Node toNode = MLTop10Util.getDeploymentNode(to, deployments);
				// check paths between nodes
				for (CommunicationPath path : commPaths) {
					if (MLTop10Util.getMemberNodes(path).containsAll(Arrays.asList(fromNode, toNode))) {
						// integrity on relevant paths?
						if (!MLTop10Util.hasStereotype(path, MLTop10.Integrity)) {
							this.addError("2: There is a dependency between '" + from.getName() + "' and '"
									+ to.getName()
									+ "', but the communication path between the nodes these artifacts are deployed to, does not fulfill 'integrity'.");
						}
						// secrecy on relevant paths?
						if (!MLTop10Util.hasStereotype(path, MLTop10.Secrecy)) {
							this.addError("2: There is a dependency between '" + from.getName() + "' and '"
									+ to.getName()
									+ "', but the communication path between the nodes these artifacts are deployed to, does not fulfill 'secrecy'.");
						}
					}
				}
			}
		}

		// ---------------------------------------------------------
		// 3. Input Validation
		for (Artifact el : aiapplications) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.AIApplication, "InputValidation")) {
				this.addError("3: AI Application '" + el.getName() + "' does not perform input validation.");
			}
		}

		// ---------------------------------------------------------
		// 4. Tamper-evident Logs
		for (Artifact el : aiapplications) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.AIApplication, "TamperEvidentLogging")) {
				this.addError("4: AI Application '" + el.getName() + "' does not perform tamper evident logging.");
			}
		}

		// ---------------------------------------------------------
		// 5. Regular Software Updates
		for (NamedElement el : aiscenarios) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.SecureAIScenario, "RegularPackageUpdates")) {
				this.addError("5: Packages in Secure AI Scenario '" + el.getName() + "' are not kept up to date.");
			}
		}

		// ---------------------------------------------------------
		// 6. Monitoring and Auditing
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "RegularAuditAndMonitoring")) {
				this.addError("6: ML Model '" + el.getName() + "' is not regularly audited and monitored.");
			}
		}

		// ---------------------------------------------------------
		// Overall result
		if (this.errorDetected) {
			this.addError("Result: An Output Integrity Attack is potentially possible!");
			return false;
		}
		this.addInfo("Result: No vulnerabilities for Output Integrity Attacks detected.");
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
