package carisma.check.mltop10checks.aisupplychain;

import java.util.Set;

import org.eclipse.uml2.uml.NamedElement;

import carisma.check.mltop10checks.common.AbstractMLTop10Check;
import carisma.core.checks.CarismaCheckWithID;
import carisma.profile.umlsec.mltop10.MLTop10;
import carisma.profile.umlsec.mltop10.MLTop10Util;

/**
 * analyzes an deployment diagram with respect to ai supply chain rules.
 * 
 * @author Alexander Peikert
 * @author Julian Flake
 *
 */

public class AISupplyChainCheck extends AbstractMLTop10Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.aisupplychain";
	public static final String CHECK_NAME = "MLTop10 AI Supply Chain Attack Check";

	@Override
	public boolean runCheck() {

		// ---------------------------------------------------------
		// populate collections
		Set<NamedElement> aiscenarios = MLTop10Util.getStereotypedElements(modelEl, NamedElement.class,
				MLTop10.SecureAIScenario);

		// ---------------------------------------------------------
		// Check numbers of elements relevant for this check
		this.addInfo("Your model contains " + aiscenarios.size() + " Secure AI Scenario(s).");

		if (aiscenarios.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the Secure AI Scenario. Therefore, it cannot be ensured that your scenario contains mitigations against 'AI Supply Chain Attack'.");
		}
		if (aiscenarios.size() > 1) {
			this.addWarning(
					"Your model contains more than one Secure AI Scenario Elements. Check, whether this is intended.");
		}

		// ---------------------------------------------------------
		// Check the specific prevention mechanisms
		// ---------------------------------------------------------

		// ---------------------------------------------------------
		// 1. Verify packages integrity
		for (NamedElement el : aiscenarios) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.SecureAIScenario, "PackageIntegrityVerified")) {
				this.addError("1. Secure AI Scenario '" + el.getName() + "' does not verify the package integrity.");
			}
		}

		// ---------------------------------------------------------
		// 2. Keep packages versions up-to-date
		for (NamedElement el : aiscenarios) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.SecureAIScenario, "RegularPackageUpdates")) {
				this.addError("2. Secure AI Scenario '" + el.getName() + "' does not keep the packages up-to-date.");
			}
		}

		// ---------------------------------------------------------
		// 3. Install packages from secure sources
		for (NamedElement el : aiscenarios) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.SecureAIScenario, "PackagesFromSecureSources")) {
				this.addError(
						"3. Secure AI Scenario '" + el.getName() + "' does not use packages from secure sources.");
			}
		}

		// ---------------------------------------------------------
		// 4. Deploy ML infrastructure securely
		for (NamedElement el : aiscenarios) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.SecureAIScenario, "SecureDeployment")) {
				this.addError("4. Secure AI Scenario '" + el.getName() + "' does not perform a secure deployment.");
			}
		}

		// ---------------------------------------------------------
		// Overall result
		if (this.errorDetected) {
			this.addError("=> An AI Supply Chain Attack is potentially possible!");
			return false;
		}
		this.addInfo("=> No vulnerabilities for AI Supply Chain Attacks detected.");
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
