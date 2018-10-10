package carisma.check.processanalysis.loader.bpmn;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.bpmn2.DocumentRoot;
import org.eclipse.bpmn2.Lane;
import org.eclipse.bpmn2.Task;
import org.eclipse.bpmn2.TextAnnotation;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.bpmn2.BPMN2Helper;
import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;

/**
 * Importer for models in BPMN2 format.
 * At this time, Lanes, tasks and text annotations are supported.
 */
public class BPMN2ImportCheck implements CarismaCheck {

	public BPMN2ImportCheck() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		
		//The incoming model.
		Resource res = host.getAnalyzedModel();
		
		//The outgoing process description
		ProcessDescription processDescription = new ProcessDescription();

		//TODO KR: res.getContents().get(0) anstatt nur res, muss man noch Sachen abfangen ob das model �berhaupt content hat etc?
		//TODO: Sind das schon alle relevanten Diagrammtypen?
		
		ArrayList<Lane> lanes = (ArrayList<Lane>) BPMN2Helper.getAllElementsOfType((DocumentRoot) res.getContents().get(0), Lane.class);
		ArrayList<Task> tasks = (ArrayList<Task>) BPMN2Helper.getAllElementsOfType((DocumentRoot) res.getContents().get(0), Task.class);
		ArrayList<TextAnnotation> annotations = (ArrayList<TextAnnotation>) BPMN2Helper.getAllElementsOfType((DocumentRoot) res.getContents().get(0), TextAnnotation.class);

		//TODO: typ und id klären
		
		for (Lane l : lanes) {
			//processDescription.addEntity(new ProcessEntity(l.getName(), TextKind.NAME, (EObject)l));
			processDescription.addEntity(new ProcessEntity("", "", l, l.getName()));
		}

		for (Task t : tasks) {
			//processDescription.addEntity(new ProcessEntity(t.getName(), TextKind.NAME));
			processDescription.addEntity(new ProcessEntity("", "", t, t.getName()));
		}

		for (TextAnnotation a : annotations) {
			//processDescription.addEntity(new ProcessEntity(a.getText(), TextKind.NAME));
			processDescription.addEntity(new ProcessEntity("", "", a, a.getText()));
		}
		
		try {
			host.putToRegister(ProcessDescription.CARISMA_REGISTRY_KEY, processDescription);
		} catch (RegisterInUseException e) {
			host.displayError("There is already a model to be analysed. Have you used two model importer plugins?");
			return false;
		}
		
		return true;
	}
}