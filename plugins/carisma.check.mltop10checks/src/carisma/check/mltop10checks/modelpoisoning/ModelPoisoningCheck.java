package carisma.check.mltop10checks.modelpoisoning;

import java.util.Set;

import org.eclipse.uml2.uml.Artifact;

import carisma.check.mltop10checks.common.AbstractMLTop10Check;
import carisma.core.checks.CarismaCheckWithID;
import carisma.profile.umlsec.mltop10.MLTop10;
import carisma.profile.umlsec.mltop10.MLTop10Util;

/**
 * analyzes an deployment diagram with respect to model poisoning rules.
 * 
 * @author Alexander Peikert
 *
 */

public class ModelPoisoningCheck extends AbstractMLTop10Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.modelpoisoning";
	public static final String CHECK_NAME = "MLTop10 Model Poisoning Attack Check";

	@Override
	public boolean runCheck() {

		// ---------------------------------------------------------
		// populate collections
		Set<Artifact> mlmodels = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.MLModel);
		Set<Artifact> aialgorithms = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.AIAlgorithm);

		// ---------------------------------------------------------
		// Check numbers of elements relevant for this check
		this.addInfo("Your model contains " + mlmodels.size() + " ML Model(s) and " + aialgorithms.size()
				+ " AI Algorithms(s).");
		if (mlmodels.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the ML Model. Therefore, it cannot be ensured that your scenario contains mitigations against the threat 'Model Poisoning Attack'.");
		}
		if (aialgorithms.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the AI Application. Therefore, it cannot be ensured that your scenario contains mitigations against 'Model Poisoning Attack'.");
		}

		// ---------------------------------------------------------
		// Check the specific prevention mechanisms
		// ---------------------------------------------------------

		// ---------------------------------------------------------
		// 1. Regularisation
		for (Artifact el : aialgorithms) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.AIAlgorithm, "Regularisation")) {
				this.addError("1. AI Algorithm '" + el.getName() + "' does not use any regularisation techniques.");
			}
		}

		// ---------------------------------------------------------
		// 2. Robust Model Design
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "RobustArchitecture")) {
				this.addError("2. ML Model '" + el.getName() + "' does not have a robust architecture.");
			}
		}

		// ---------------------------------------------------------
		// 3. Cryptographic Techniques
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "CryptographicallySecured")) {
				this.addError("3. ML Model '" + el.getName() + "' does not use any cryptographic techniques.");
			}
		}

		// ---------------------------------------------------------
		// Overall result
		if (this.errorDetected) {
			this.addError("=> A Model Poisoning Attack is potentially possible!");
			return false;
		}
		this.addInfo("=> No vulnerabilities for Model Poisoning Attacks detected.");
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
