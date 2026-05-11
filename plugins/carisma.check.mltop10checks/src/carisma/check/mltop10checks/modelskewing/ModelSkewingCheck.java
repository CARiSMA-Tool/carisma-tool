package carisma.check.mltop10checks.modelskewing;

import java.util.Set;

import org.eclipse.uml2.uml.Artifact;

import carisma.check.mltop10checks.common.AbstractMLTop10Check;
import carisma.core.checks.CarismaCheckWithID;
import carisma.profile.umlsec.mltop10.MLTop10;
import carisma.profile.umlsec.mltop10.MLTop10Util;

/**
 * analyzes an deployment diagram with respect to model skewing rules.
 * 
 * @author Alexander Peikert
 * @author Julian Flake
 *
 */

public class ModelSkewingCheck extends AbstractMLTop10Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.modelskewing";
	public static final String CHECK_NAME = "MLTop10 Model Skewing Attack Check";

	@Override
	public boolean runCheck() {

		// ---------------------------------------------------------
		// populate collections
		Set<Artifact> mlmodels = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.MLModel);
		Set<Artifact> feedbackdatas = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.FeedbackData);

		// ---------------------------------------------------------
		// Check numbers of elements relevant for this check
		this.addInfo("Your model contains " + mlmodels.size() + " ML Model(s) and " + feedbackdatas.size()
				+ " Feedback Data(s).");
		if (feedbackdatas.isEmpty()) {
			this.addWarning(
					"Pre Check. Your model does not contain any Feedback Data. Therefore, Model Skewing is not possible.");
			return true;
		}
		if (mlmodels.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the ML Model. Therefore, it cannot be ensured that your scenario contains mitigations against 'Model Skewing Attack'.");
		}

		// ---------------------------------------------------------
		// Check the specific prevention mechanisms
		// ---------------------------------------------------------

		// ---------------------------------------------------------
		// 1. Implement robust access controls
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "AccessControl")) {
				this.addError("1. ML Model '" + el.getName() + "' does not implement access control.");
			}
		}
		for (Artifact el : feedbackdatas) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.FeedbackData, "AccessControl")) {
				this.addError("1. Feedback Data '" + el.getName() + "' does not implement access control.");
			}
		}

		// ---------------------------------------------------------
		// 2. Verify the authenticity of feedback data
		for (Artifact el : feedbackdatas) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.FeedbackData, "AuthenticityVerified")) {
				this.addError("2. Feedback Data '" + el.getName() + "'s' authenticity is not verified.");
			}
		}

		// ---------------------------------------------------------
		// 3. Verify the authenticity of feedback data
		for (Artifact el : feedbackdatas) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.FeedbackData, "Validation")) {
				this.addError("3. Feedback Data '" + el.getName() + "' is not validated.");
			}
		}
		for (Artifact el : feedbackdatas) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.FeedbackData, "Cleaning")) {
				this.addError("3. Feedback Data '" + el.getName() + "' is not cleaned.");
			}
		}

		// ---------------------------------------------------------
		// 4. Implement anomaly detection
		for (Artifact el : feedbackdatas) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.FeedbackData, "AnomalyDetection")) {
				this.addError("4. Feedback Data '" + el.getName() + "' does not implement anomaly detection.");
			}
		}

		// ---------------------------------------------------------
		// 5. Regularly monitor the model’s performance
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "RegularPerformanceMonitoring")) {
				this.addError("5. ML Model '" + el.getName() + "' does not perform a regular performance monitoring.");
			}
		}

		// ---------------------------------------------------------
		// 6. Continuously train the model
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "RegularRetraining")) {
				this.addError("6. ML Model '" + el.getName() + "' is not regularly retrained.");
			}
		}

		// ---------------------------------------------------------
		// Overall result
		if (this.errorDetected) {
			this.addError("=> A Model Skewing Attack is potentially possible!");
			return false;
		}
		this.addInfo("=> No vulnerabilities for Model Skewing Attacks detected.");
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
