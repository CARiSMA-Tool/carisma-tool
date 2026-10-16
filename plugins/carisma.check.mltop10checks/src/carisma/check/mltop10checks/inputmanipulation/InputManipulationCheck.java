package carisma.check.mltop10checks.inputmanipulation;

import java.util.Set;

import org.eclipse.uml2.uml.Artifact;

import carisma.check.mltop10checks.common.AbstractMLTop10Check;
import carisma.core.checks.CarismaCheckWithID;
import carisma.profile.umlsec.mltop10.MLTop10;
import carisma.profile.umlsec.mltop10.MLTop10Util;

/**
 * analyzes an deployment diagram with respect to input manipulation rules.
 * 
 * @author Alexander Peikert
 * @author Julian Flake
 *
 */

public class InputManipulationCheck extends AbstractMLTop10Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.inputmanipulation";
	public static final String CHECK_NAME = "MLTop10 Input Manipulation Check";
	public static final String THREAT_COMMENT_ATTRIBUTE = "InputManipulation";
	public static final String THREAT_NAME = "MLTop10 Input Manipulation Attack";

	@Override
	public boolean runCheck() {

		// ---------------------------------------------------------
		// populate collections
		Set<Artifact> mlmodels = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.MLModel);
		Set<Artifact> aiapplications = MLTop10Util.getStereotypedElements(modelEl, Artifact.class,
				MLTop10.AIApplication);

		// ---------------------------------------------------------
		// Check numbers of elements relevant for this check
		this.addInfo("Your model contains " + mlmodels.size() + " ML Model(s) and " + aiapplications.size()
				+ " AI Application(s).");
		if (mlmodels.isEmpty()) {
			this.addInfo(
					"Your model does not contain any information on the ML Model. Therefore, it cannot be ensured that your scenario contains mitigations against the threat 'Input Manipulation Attack'.");
		}
		if (aiapplications.isEmpty()) {
			this.addInfo(
					"Your model does not contain any information on the AI Application. Therefore, it cannot be ensured that your scenario contains mitigations against 'Input Manipulation Attack'.");
		}

		// ---------------------------------------------------------
		// Check the specific prevention mechanisms
		// ---------------------------------------------------------

		// ---------------------------------------------------------
		// 1.Adversarial training
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "AdversarialTraining")) {
				this.addWarning("1. ML Model '" + el.getName() + "' does not do adversarial training.");
			}
		}

		// ---------------------------------------------------------
		// 2.Robust models
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "DefenseMechanism")) {
				this.addWarning("2. ML Model '" + el.getName() + "' does not implement defense mechanisms.");
			}
		}

		// ---------------------------------------------------------
		// 3. Input validation
		for (Artifact el : aiapplications) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.AIApplication, "InputValidation")) {
				this.addWarning("3. AI Application '" + el.getName() + "' does not implement input validation.");
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
