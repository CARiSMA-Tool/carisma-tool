package carisma.check.situationfinder;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.bpmn2.DocumentRoot;

import com.thoughtworks.xstream.persistence.FilePersistenceStrategy;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.bpmn2.BPMN2Helper;
import carisma.regulatory.ontology.Activity;
import carisma.regulatory.ontology.Constraint;
import carisma.regulatory.ontology.Ontology;
import carisma.regulatory.ontology.RuleElement;
import carisma.regulatory.ontology.Situation;
import carisma.regulatory.ontology.owl.OWL2Ontology;

public class SituationFinderCheck implements CarismaCheck {
	
	private AnalysisHost host = null;
	
	private Ontology regulatoryOntology = null;
	
	private DocumentRoot modelRoot = null;
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters,
			AnalysisHost newHost) {
		host = newHost;
		
		modelRoot = (DocumentRoot) newHost.getAnalyzedModel().getContents().get(0);
		List<org.eclipse.bpmn2.Activity> modelActivities = BPMN2Helper.getAllElementsOfType(modelRoot, org.eclipse.bpmn2.Activity.class);
		
		InputFileParameter ontologyFileParameter = (InputFileParameter) parameters.get("carisma.check.situationfinder.ontologyFile");
		File ontologyFile = ontologyFileParameter.getValue();
		regulatoryOntology = OWL2Ontology.loadFromFile(ontologyFile);
		
		
		Collection<Situation> situations = regulatoryOntology.getSituations();
		for (Situation situation : situations) {
			Collection<RuleElement> involvedRuleElements = situation.getInvolvedRuleElements();
			List<RuleElement> foundRuleElements = new ArrayList<RuleElement>();
			for (RuleElement involvedRuleElement : involvedRuleElements) {
				if (involvedRuleElement instanceof Activity) {
					String activityName = involvedRuleElement.getName();
					for (org.eclipse.bpmn2.Activity modelActivity : modelActivities) {
						if (modelActivity.getName().equals(activityName)) {
							foundRuleElements.add(involvedRuleElement);
						}
					}
				}
			} 
			//TODO: TH: Ist die folgende Abfrage so wirklich korrekt? Wahrscheinlich false positive, wenn gleiches RE mehrmals in Modell vorkommt 
			if (foundRuleElements.size() == involvedRuleElements.size()) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Found situation: " + situation.getName()));
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "It is advised to analyze the model with the following checks:"));
				for (Constraint constraint : situation.getConstraintsToCheck()) {
					host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, constraint.getType()));
				}
			}
		}
		return true;
	}

}
