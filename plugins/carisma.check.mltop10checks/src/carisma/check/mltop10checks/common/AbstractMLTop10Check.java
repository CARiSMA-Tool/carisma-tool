package carisma.check.mltop10checks.common;

import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Package;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;

/**
 * Abstract super class for the ML Security Top 10 checks.
 * 
 * @author Julian Flake
 * @author Alexaner Peikert
 */
public abstract class AbstractMLTop10Check implements CarismaCheck {

	/**
	 * AnalysisHost for report.
	 */
	protected AnalysisHost analysisHost;

	/**
	 * The model to check.
	 */
	protected Model modelEl = null;

	/**
	 * A flag to store, whether at least one error has been detected.
	 */
	protected boolean errorDetected = false;

	protected void addInfo(String message) {
		this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, message));
	}

	protected void addWarning(String message) {
		this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, message));
	}

	protected void addError(String message) {
		this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, message));
		this.errorDetected = true;
	}

	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost newHost) {
		if (newHost != null) {
			this.analysisHost = newHost;
		} else {
			this.analysisHost = new DummyHost(true);
		}
		Resource currentModel = this.analysisHost.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			this.addError("Empty model");
			this.analysisHost.appendLineToReport("Empty model");
			return false;
		}
		if (!(currentModel.getContents().get(0) instanceof Package)) {
			this.addError("Content is not a model!");
			this.analysisHost.appendLineToReport("Content is not a model!");
			return false;
		}
		this.modelEl = (Model) currentModel.getContents().get(0);
		return runCheck();
	}

	public abstract boolean runCheck();

}
