package carisma.check.owl2.dummy;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.core.util.EObjectUtil;
import carisma.modeltype.owl2.OWL2ModelSaver;
import carisma.modeltype.owl2.change.ChangeElement;
import carisma.modeltype.owl2.change.ChangeHelper;
import carisma.modeltype.owl2.hide.HideHelper;
import carisma.modeltype.owl2.model.owl.Ontology;
import carisma.modeltype.owl2.type.OWL2FileType;

/**
 * Dummy Check for OWL2 models.
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

	private Ontology ontology = null;
	
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
		} else {
			ontology = (Ontology) currentModel.getContents().get(0);
			printContent(currentModel.getContents().get(0), "");
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Model contains " 
					+ numOfElements + " elements"));
			
			boolean changeParameterValue = false;
			if (parameters.containsKey("carisma.check.owl2.dummy.change")) {
				BooleanParameter parameter = (BooleanParameter) parameters.get("carisma.check.owl2.dummy.change");
				changeParameterValue = parameter.getValue();
			}
			
			boolean hidingParameterValue = false;
			if (parameters.containsKey("carisma.check.owl2.dummy.hiding")) {
				BooleanParameter parameter = (BooleanParameter) parameters.get("carisma.check.owl2.dummy.hiding");
				hidingParameterValue = parameter.getValue();
			}
			
			if (hidingParameterValue) {
				HideHelper.applyHidings(ontology, true);
			}
			
			if (changeParameterValue) {
				List<ChangeElement> list = ChangeHelper.getAllChangeElements(ontology, true, true);
				ChangeHelper.applyChanges(list);
				
			}
			
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Hiding " + hidingParameterValue + ", Change " + changeParameterValue));
			
			OWL2ModelSaver saver = new OWL2ModelSaver(ontology);
			saver.saveOntology(new File(currentModel.getURI().toFileString() + ".output"), OWL2FileType.OWL_XML);
			saver.exportOntology(new File(currentModel.getURI().toFileString() + ".output2"), OWL2FileType.OWL_XML);
			
			return true;
		}
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
			printContent(child, indent + "\t");
		}
	}
}
