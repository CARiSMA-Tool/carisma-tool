package carisma.check.mltop10checks.aggregator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Package;

import carisma.check.mltop10checks.aisupplychain.AISupplyChainCheck;
import carisma.check.mltop10checks.datapoisoning.DataPoisoningCheck;
import carisma.check.mltop10checks.inputmanipulation.InputManipulationCheck;
import carisma.check.mltop10checks.membershipinference.MembershipInferenceCheck;
import carisma.check.mltop10checks.modelinversion.ModelInversionCheck;
import carisma.check.mltop10checks.modelpoisoning.ModelPoisoningCheck;
import carisma.check.mltop10checks.modelskewing.ModelSkewingCheck;
import carisma.check.mltop10checks.modeltheft.ModelTheftCheck;
import carisma.check.mltop10checks.outputintegrity.OutputIntegrityCheck;
import carisma.check.mltop10checks.transferlearning.TransferLearningCheck;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;

/**
 * This check aggregates the OWASP ML Security Top 10 checks into an aggregated
 * Check and Report.
 * 
 * @author Julian Flake
 *
 */
public class MLSecTop10Aggregator implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.mltop10checks.aggregator";
	public static final String CHECK_NAME = "ML Security Top10 Aggregator Check";
	public static final String MLTOP10_VERSION = "2023";

	Map<String, CheckParameter> parameters;
	private AnalysisHost host;

	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		List<CarismaCheckWithID> checks = initializeChecks();
		Map<String, Boolean> results = new HashMap<>();

		// Set parameters and set host or initialize and set dummy host.
		this.parameters = parameters;
		this.host = host;
		if (this.host == null) {
			this.host = new DummyHost(true);
		}
		// Load model and check model (class) and emptiness.
		Resource currentModel = host.getAnalyzedModel();
		if (!(currentModel.getContents().get(0) instanceof Package)) {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			this.host.appendLineToReport("Content is not a model!");
			return false;
		}
		if (currentModel.getContents().isEmpty()) {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			this.host.appendLineToReport("Empty model");
			return false;
		}

		// Perform each check and collect results
		for (int i = 0; i < checks.size(); i++) {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, new String("---- ").repeat(20)));
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, String.format("ML%1$02d:%2$s %3$s:",
					i + 1, MLTOP10_VERSION, checks.get(i).getName().replace("MLTop10 ", ""))));
			results.put(checks.get(i).getCheckID(), checks.get(i).perform(this.parameters, this.host));
		}

		// Overall result is successful, if there is no check that returned false.
		return !results.containsValue(Boolean.FALSE);
	}

	/**
	 * Return the list of all checks that should be included in this aggregator
	 * check. Order matters, since the order determines the labels (ML01, ML02, ...,
	 * ML10).
	 * 
	 * @return List of all checks that should be included in the aggregate report.
	 */
	private List<CarismaCheckWithID> initializeChecks() {
		List<CarismaCheckWithID> checks = new ArrayList<>();
		checks.add(new InputManipulationCheck());
		checks.add(new DataPoisoningCheck());
		checks.add(new ModelInversionCheck());
		checks.add(new MembershipInferenceCheck());
		checks.add(new ModelTheftCheck());
		checks.add(new AISupplyChainCheck());
		checks.add(new TransferLearningCheck());
		checks.add(new ModelSkewingCheck());
		checks.add(new OutputIntegrityCheck());
		checks.add(new ModelPoisoningCheck());
		return checks;
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
