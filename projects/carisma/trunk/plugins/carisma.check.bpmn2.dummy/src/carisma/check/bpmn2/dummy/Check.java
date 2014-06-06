package carisma.check.bpmn2.dummy;

import java.util.Map;

import org.eclipse.bpmn2.DocumentRoot;
import org.eclipse.bpmn2.di.BPMNDiagram;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.core.util.EObjectUtil;


/**
 * Dummy Check for BPMN2 models.
 * @author Marcel Michel
 *
 */
public class Check implements CarismaCheck {

	/**
	 * The analysis host.
	 */
	private AnalysisHost host;
	
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
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		this.host = host;
		this.numOfElements = 0;
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (currentModel.getContents().get(0) instanceof DocumentRoot) {
			DocumentRoot modelRoot = (DocumentRoot) currentModel.getContents().get(0);
			printContent(modelRoot.getDefinitions(), "");
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "number of elements counted: " + numOfElements));
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
		numOfElements++;
		host.appendLineToReport(indent + element.eClass().getName() + ": " 
				+ EObjectUtil.getName(element));
		for (EObject child : element.eContents()) {
			if (!(child instanceof BPMNDiagram)) {
				printContent(child, indent + "\t");
			}
		}
	}
}
