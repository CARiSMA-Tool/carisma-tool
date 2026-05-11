package carisma.check.mltop10checks.modelinversion;

import java.util.Set;

import org.eclipse.uml2.uml.Artifact;

import carisma.check.mltop10checks.common.AbstractMLTop10Check;
import carisma.core.checks.CarismaCheckWithID;
import carisma.profile.umlsec.mltop10.MLTop10;
import carisma.profile.umlsec.mltop10.MLTop10Util;

/**
 * analyzes an deployment diagram with respect to model inversion rules.
 * 
 * @author Alexander Peikert
 * @author Julian Flake
 *
 */

public class ModelInversionCheck extends AbstractMLTop10Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.modelinversion";
	public static final String CHECK_NAME = "MLTop10 Model Inversion Attack Check";

	@Override
	public boolean runCheck() {

		// ---------------------------------------------------------
		// populate collections
		Set<Artifact> mlmodels = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.MLModel);
		Set<Artifact> aiapplications = MLTop10Util.getStereotypedElements(modelEl, Artifact.class,
				MLTop10.AIApplication);
		Set<Artifact> aialgorithms = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.AIAlgorithm);
		Set<Artifact> trainingdatas = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.TrainingData);
		// ---------------------------------------------------------
		// Check numbers of elements relevant for this check
		this.addInfo("Your model contains " + aialgorithms.size() + " AI Algorithm(s), " + trainingdatas.size()
				+ " Training Data(s), " + mlmodels.size() + " ML Model(s) and " + aiapplications.size()
				+ " AI Application(s).");
		if (mlmodels.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the ML Model. Therefore, it cannot be ensured that your scenario contains mitigations against the threat 'Model Inversion Attack'.");
		}
		if (aiapplications.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the AI Application. Therefore, it cannot be ensured that your scenario contains mitigations against 'Model Inversion Attack'.");
		}
		if (aialgorithms.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the AI Algorithm. Therefore, it cannot be ensured that your scenario contains mitigations against 'Model Inversion Attack'.");
		}
		if (trainingdatas.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the Training Data. Therefore, it cannot be ensured that your scenario contains mitigations against 'Model Inversion Attack'.");
		}

		// ---------------------------------------------------------
		// Check the specific prevention mechanisms
		// ---------------------------------------------------------

		// ---------------------------------------------------------
		// 0. Public model, training data and algorithm
		boolean hasPublicMLModel = false;
		boolean hasPublicTrainingData = false;
		boolean hasPublicAlgorithm = false;

		for (Artifact el : mlmodels) {
			if (MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "Public")) {
				hasPublicMLModel = true;
				break;
			}
		}

		for (Artifact el : trainingdatas) {
			if (MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "Public")) {
				hasPublicTrainingData = true;
				break;
			}
		}

		for (Artifact el : aialgorithms) {
			if (MLTop10Util.isTaggedValueTrue(el, MLTop10.AIAlgorithm, "Public")) {
				hasPublicAlgorithm = true;
				break;
			}
		}

		if (hasPublicMLModel && hasPublicTrainingData && hasPublicAlgorithm) {
			this.addInfo(
					"Pre Check. ML Model, Training Data, and AI Algorithm are all publicly accessible. No check for a Model Inversion Attack necessary.");
			return true;
		}

		// ---------------------------------------------------------
		// 1. Access control TODO: Check whether they all belong to the same scenario
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "AccessControl")) {
				this.addError("1. ML Model '" + el.getName() + "' does not have any access control implemented.");
			}
		}

		// ---------------------------------------------------------
		// 2. Input validation
		for (Artifact el : aiapplications) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.AIApplication, "InputValidation")) {
				this.addError(
						"2. AI Application '" + el.getName() + "' does not have any input validation implemented.");
			}
		}

		// ---------------------------------------------------------
		// 3. Model transparency
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "Transparency")) {
				this.addError("3. ML Model '" + el.getName() + "' is not transparent.");
			}
		}

		// ---------------------------------------------------------
		// 4. Regular monitoring
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "AnomalyDetection")) {
				this.addError("4. ML Model '" + el.getName() + "' does not have any anomaly detection implemented.");
			}
		}

		// ---------------------------------------------------------
		// 5. Model retraining
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "RegularRetraining")) {
				this.addError("5. ML Model '" + el.getName() + "' does not perform regular retraining.");
			}
		}

		// ---------------------------------------------------------
		// Overall result
		if (this.errorDetected) {
			this.addError("=> A Model Inversion Attack is potentially possible!");
			return false;
		}
		this.addInfo("=> No vulnerabilities for Model Inversion Attacks detected.");
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
