package carisma.check.bpmn2.dummy;

import java.util.Map;

import org.eclipse.bpmn2.DocumentRoot;
import org.eclipse.bpmn2.di.BPMNDiagram;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.util.EObjectUtil;


/**
 * Dummy BPMN2DummyCheck for BPMN2 models.
 * @author Marcel Michel
 *
 */
public class BPMN2DummyCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.bpmn2dummy";
	public static final String CHECK_NAME = "BPMN2 Dummy Check";

	/**
	 * The analysis host.
	 */
	private AnalysisHost analysisHost;
	
	/**
	 * Number of elements in the model.
	 */
	private int numOfElements;
	
	/**
	 * Performs an dummy analysis.
	 * @param parameters The check parameters
	 * @param host The AnalysisHost
	 * @return If check is successfully performed true otherwise false
	 */
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		this.analysisHost = host;
		this.numOfElements = 0;
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (currentModel.getContents().get(0) instanceof DocumentRoot) {
			DocumentRoot modelRoot = (DocumentRoot) currentModel.getContents().get(0);
			printContent(modelRoot.getDefinitions(), "");
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "number of elements counted: " + this.numOfElements));
			return true;
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		return false;
	}
	
	/**
	 * This method prints recursively the content of a model to the report. 
	 * @param element The current model element
	 * @param indent The indent illustrates the containment relationship
	 */
	private void printContent(final EObject element, final String indent) {
		this.numOfElements++;
		this.analysisHost.appendLineToReport(indent + element.eClass().getName() + ": " 
				+ EObjectUtil.getName(element));
		for (EObject child : element.eContents()) {
			if (!(child instanceof BPMNDiagram)) {
				printContent(child, indent + "\t");
			}
		}
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
