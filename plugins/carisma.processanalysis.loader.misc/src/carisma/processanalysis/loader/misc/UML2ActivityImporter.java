package carisma.processanalysis.loader.misc;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.Comment;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;
import carisma.processanalysis.textmodel.TextKind;

public class UML2ActivityImporter implements CarismaCheck {
	
	Resource res = null;
	
	public UML2ActivityImporter(Resource res) {
		this.res = res;
	}
	
	/**
	 * Creates a new DataModel from a given resource.
	 * This model is then stored in the central registry
	 * if no model with the same name exists.
	 * 
	 * @param res The resource that contains the model.
	 * @return DataModel
	 * @throws ImportException 
	 */

	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		
		Resource res = host.getAnalyzedModel();
		Model model = (Model) res.getContents().get(0);
		HashMap<String, String> activities = new HashMap<String, String>();
		HashMap<String, String> comments = new HashMap<String, String>();
		ProcessDescription processDescription = new ProcessDescription();

		//First find all activities and their corresponding labels
		for (Action action : UMLHelper.getAllElementsOfType(model, Action.class)) {
			activities.put(action.getName(), action.getLabel());
		}

		// Finde alle Kommentare. Pruefe fuer jeden Kommentar, ob er einer
		// Aktivitaet zugeordnet ist. Falls das der Fall ist, speichere
		// dieses Paar als ein gemeinsames ModelEntitiy, ansonsten
		// als eigenstaendige Entity.
		for (Comment comment : UMLHelper.getAllElementsOfType(model, Comment.class)) {
			boolean foundActivity = false;
			String actionName = "";

			for (Element element : comment.getAnnotatedElements()) {
				if (element instanceof Action) {
					foundActivity = true;
					Action annoAction = (Action) element;
					actionName = annoAction.getName();
					break;
				}
			}

			// Entweder es gab zu diesem Kommentar eine Aktivitaet, oder
			// es handelt sich um einen isolierten Kommentar. Den isolierten
			// Kommentar speichern wir sofort im Modell, den anderen ordnen
			// wir spaeter einer Aktivitaet zu.
			if (foundActivity) {
				comments.put(actionName, comment.getBody());
			} else {
				//TODO: type und id klären!
				//processDescription.addEntity(new ProcessEntity(comment.getBody(), TextKind.Comment));
				processDescription.addEntity(new ProcessEntity("", "", comment, comment.getBody()));
			}
		}

		// Ueber alle Aktivitaeten iterieren und die zugehoerigen Kommentare
		// finden, falls welche vorhanden sind. Ansonsten wird der Text der
		// Aktivitaet ohne Kommentar im Datenmodell gespeichert.
		for (Map.Entry<String, String> entry : activities.entrySet()) {
			String key = entry.getKey();

			//TODO: type und id klären!
			//ProcessEntity entity = new ProcessEntity(entry.getValue(), TextKind.Label);
			ProcessEntity entity = new ProcessEntity("", "", null, entry.getValue());

			if (comments.containsKey(key)) {
				entity.addText(comments.get(key), TextKind.PROCESSCOMMENT);
			}

			processDescription.addEntity(entity);
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
