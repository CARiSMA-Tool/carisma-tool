package carisma.check.mltop10checks.datapoisoning;

import java.util.Set;

import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.Node;

import carisma.check.mltop10checks.common.AbstractMLTop10Check;
import carisma.core.checks.CarismaCheckWithID;
import carisma.profile.umlsec.mltop10.MLTop10;
import carisma.profile.umlsec.mltop10.MLTop10Util;

/**
 * analyzes an deployment diagram with respect to data poisoning rules.
 * 
 * @author Alexander Peikert
 * @author Julian Flake
 */

public class DataPoisoningCheck extends AbstractMLTop10Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.datapoisoning";
	public static final String CHECK_NAME = "MLTop10 Data Poisoning Attack Check";

	@Override
	public boolean runCheck() {
		// ---------------------------------------------------------
		// populate collections
		Set<Artifact> allArtifacts = MLTop10Util.getAllElementsOfType(modelEl, Artifact.class);
		Set<Artifact> mlmodels = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.MLModel);
		Set<Artifact> trainingdata = MLTop10Util.getStereotypedElements(modelEl, Artifact.class, MLTop10.TrainingData);
		Set<Node> trainingdataserver = MLTop10Util.getStereotypedElements(modelEl, Node.class,
				MLTop10.TrainingDataServer);

		// ---------------------------------------------------------
		// Check numbers of elements relevant for this check
		this.addInfo("Your model contains " + trainingdata.size() + " Training Data(s), " + mlmodels.size()
				+ " ML Model(s) and " + trainingdataserver.size() + " Training Data Server(s).");
		if (mlmodels.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the ML Model. Therefore, it cannot be ensured that your scenario contains mitigations against the threat 'Data Poisoning Attack'.");
		}
		if (trainingdata.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the Training Data. Therefore, it cannot be ensured that your scenario contains mitigations against the threat 'Data Poisoning Attack'.");
		}
		if (trainingdataserver.isEmpty()) {
			this.addWarning(
					"Your model does not contain any information on the Training Data Server. Therefore, it cannot be ensured that your scenario contains mitigations against the threat 'Data Poisoning Attack'.");
		}

		// ---------------------------------------------------------
		// Check the specific prevention mechanisms
		// ---------------------------------------------------------

		// ---------------------------------------------------------
		// 1. Data validation and verification
		for (Artifact el : trainingdata) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "Validation")) {
				this.addError("1. Training Data '" + el.getName() + "' does not check for validation.");
			}
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "Verification")) {
				this.addError("2. Training Data '" + el.getName() + "' does not check for verification.");
			}
		}

		// ---------------------------------------------------------
		// 2. Secure data storage
		for (Node el : trainingdataserver) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingDataServer, "SecureDataStorage")) {
				this.addError("3. Training Data Server '" + el.getName() + "' does not have a secure data storage.");
			}
		}

		// ---------------------------------------------------------
		// 3. Data separation
		for (Artifact el : allArtifacts) {
			boolean isTrainingData = MLTop10Util.hasStereotype(el, MLTop10.TrainingData);
			boolean isMLModel = MLTop10Util.hasStereotype(el, MLTop10.MLModel);

			if (isTrainingData && isMLModel) {
				this.addError("4. Artifact '" + el.getName() + "' does not isolate Training Data and ML Model.");
			}
		}

		// ---------------------------------------------------------
		// 4. Access Control
		for (Artifact el : trainingdata) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "AccessControl")) {
				this.addError("5. Training Data  '" + el.getName() + "' does not have access control.");
			}
		}

		// ---------------------------------------------------------
		// 5. Monitoring and auditing
		for (Artifact el : trainingdata) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.TrainingData, "RegularAuditAndMonitoring")) {
				this.addError("6. Training Data  '" + el.getName() + "' does not do regular auditing and monitoring.");
			}
		}

		// ---------------------------------------------------------
		// 6. Model validation
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "Validation")) {
				this.addError("7. ML Model  '" + el.getName() + "' does not check for validation.");
			}
		}

		// ---------------------------------------------------------
		// 7. Model ensembles
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "EnsembleModel")) {
				this.addError("8. ML Model  '" + el.getName() + "' is not an ensemble model.");
			}
		}

		// ---------------------------------------------------------
		// 8. Anomaly detection
		for (Artifact el : mlmodels) {
			if (!MLTop10Util.isTaggedValueTrue(el, MLTop10.MLModel, "AnomalyDetection")) {
				this.addError("9. ML Model  '" + el.getName() + "' does not have an anomaly detection.");
			}
		}

		// ---------------------------------------------------------
		// Overall result
		if (this.errorDetected) {
			this.addError("=> A Data Poisoning Attack is potentially possible!");
			return false;
		}
		this.addInfo("=> No vulnerabilities for Data Poisoning Attacks detected.");
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
