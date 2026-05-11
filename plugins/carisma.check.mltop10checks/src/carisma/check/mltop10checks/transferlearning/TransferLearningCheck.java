package carisma.check.mltop10checks.transferlearning;

import java.util.Set;

import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.NamedElement;

import carisma.check.mltop10checks.common.AbstractMLTop10Check;
import carisma.core.checks.CarismaCheckWithID;
import carisma.profile.umlsec.mltop10.MLTop10;
import carisma.profile.umlsec.mltop10.MLTop10Util;

/**
 * analyzes an deployment diagram with respect to transfer learning rules.
 * 
 * @author Alexander Peikert
 * @author Julian Flake
 *
 */

public class TransferLearningCheck extends AbstractMLTop10Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.transferlearning";
	public static final String CHECK_NAME = "MLTop10 Transfer Learning Attack Check";

	@Override
	public boolean runCheck() {

		// ---------------------------------------------------------
		// populate collections
		Set<Artifact> allArtifacts = MLTop10Util.getAllElementsOfType(modelEl, Artifact.class);
		Set<Artifact> mlmodels = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.MLModel);
		Set<Artifact> aialgorithms = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.AIAlgorithm);
		Set<Artifact> trainingdatas = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.TrainingData);
		Set<NamedElement> aiscenarios = MLTop10Util.getStereotypedElements(modelEl, NamedElement.class,
				MLTop10.SecureAIScenario);

		// Check numbers of elements relevant for this check
		this.addInfo("Your model contains " + trainingdatas.size() + " Training Data(s), " + mlmodels.size()
				+ " ML Model(s) and " + aialgorithms.size() + " AI Algorithm(s).");
		if (mlmodels.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the ML Model. Therefore, it cannot be ensured that your scenario contains mitigations against the threat 'Transfer Learning Attack'.");
		}
		if (aialgorithms.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the AI Algorithm. Therefore, it cannot be ensured that your scenario contains mitigations against 'Transfer Learning Attack'.");
		}
		if (trainingdatas.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the Training Data. Therefore, it cannot be ensured that your scenario contains mitigations against 'Transfer Learning Attack'.");
		}
		if (aiscenarios.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the Secure AI Scenario. Therefore, it cannot be ensured that your scenario contains mitigations against 'Transfer Learning Attack'.");
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
		for (Artifact el : trainingdatas) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "RegularUpdatesAndTraining")) {
				this.addError("1. Training Data '" + el.getName() + "' is not regularly monitored and updated.");
			}
		}

		// ---------------------------------------------------------
		// 2. Use secure and trusted training datasets
		for (Artifact el : trainingdatas) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "Trusted")) {
				this.addError("2. Training Data '" + el.getName() + "' is not trusted.");
			}
		}

		// ---------------------------------------------------------
		// 3. Implement model isolation
		for (Artifact el : allArtifacts) {
			boolean isTrainingData = MLTop10Util.hasStereotype(el, MLTop10.TrainingData);
			boolean isMLModel = MLTop10Util.hasStereotype(el, MLTop10.MLModel);
			boolean isAIAlgorithm = MLTop10Util.hasStereotype(el, MLTop10.AIAlgorithm);

			if (isTrainingData && isMLModel && isAIAlgorithm) {
				this.addError("3. Artifact '" + el.getName()
						+ "' does not isolate AI Algorithm, Training Data and ML Model.");
			}
		}

		// ---------------------------------------------------------
		// 4. Use differential privacy
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "DifferentialPrivacy")) {
				this.addError("4. ML Model '" + el.getName() + "' does not implement differential privacy.");
			}
		}

		// ---------------------------------------------------------
		// 5. Perform regular security audits
		for (NamedElement el : aiscenarios) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.SecureAIScenario, "RegularSecurityAudits")) {
				this.addError(
						"5. Secure AI Scenario '" + el.getName() + "' does not implement regular security audits.");
			}
		}

		// ---------------------------------------------------------
		// Overall result
		if (this.errorDetected) {
			this.addError("=> A Transfer Learning Attack is potentially possible!");
			return false;
		}
		this.addInfo("=> No vulnerabilities for Transfer Learning Attacks detected.");
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
