package carisma.xutils.regulatory.importer.superior;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;

import carisma.regulatory.ontology.utils.Annotation;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class DemoConstraintCreator {
	
	public static final String CLS_CONSTRAINT_SOD = "SeparationOfDuty";

	public boolean createDemoStructure(final RegulatoryOntologyHelper ontologyContainer) {
		boolean createRuleElementRepresentations = true;
		OWLNamedIndividual sodRule = ontologyContainer.getGoh().getIndividualByIdAndClass(
				"MARiskVA_7.2.1_B1", RegulatoryOntologyHelper.CLS_MARISKBINDING, true);
		if (sodRule == null) {
			System.out.println("MaRisk SOD Rule not found!");
			return false;
		}
		OWLClass separationOfDutyClass = createSODConstraintClass("Activity,Activity",ontologyContainer);
		if (separationOfDutyClass == null) {
			System.out.println("Couldn't create the SOD Constraint class!");
			return false;
		}

		List<OWLNamedIndividual> activities = createDemoActivities(ontologyContainer);
		if (activities.size() != 2) {
			System.out.println("Couldn't create all necessary activities for SOD!");
			return false;
		}
		if (createRuleElementRepresentations) {
			for (OWLNamedIndividual activity : activities) {
				ontologyContainer.createContainedRuleElementsRelation(sodRule, activity, "" , -1, -1);
			}
		}
		OWLNamedIndividual sodIndividual = createSODIndividual(activities, ontologyContainer);
		if (sodIndividual == null) {
			System.out.println("Couldn't create the SOD individual for the MaRisk rule!");
			return false;
		}
		OWLNamedIndividual vierAugenPrinzip = createVierAugenPrinzip(sodRule, activities, sodIndividual, ontologyContainer);
		if (vierAugenPrinzip == null) {
			System.out.println("Couldn't create the situation for the constraint!");
			return false;
		}

		
		
		List<OWLNamedIndividual> sigmaActivities = createSigmaActivities(ontologyContainer);
		if (sigmaActivities.size() != 2) {
			System.out.println("Couldn't create all necessary SIGMA activities for SOD!");
			return false;
		}	
		if (createRuleElementRepresentations) {
			for (OWLNamedIndividual activity : sigmaActivities) {
				ontologyContainer.createContainedRuleElementsRelation(sodRule, activity, "" , -1, -1);
			}
		}
		
		OWLNamedIndividual sigmaSodIndividual = createSigmaSODIndividual(sigmaActivities, ontologyContainer);
		if (sigmaSodIndividual == null) {
			System.out.println("Couldn't create the SIGMA SOD individual for the TBD rule!");
			return false;
		}
		OWLNamedIndividual sigmaVierAugenPrinzip = createSigmaVierAugenPrinzip(sodRule, sigmaActivities, sigmaSodIndividual, ontologyContainer);
		if (sigmaVierAugenPrinzip == null) {
			System.out.println("Couldn't create the situation for the constraint!");
			return false;
		}
		return true;
	}
	
	private OWLClass createSODConstraintClass(final String specification, final RegulatoryOntologyHelper ontologyContainer) {
		List<String> ruleElementClasses = new ArrayList<String>();
		ruleElementClasses.add(RegulatoryOntologyHelper.CLS_ACTIVITY);
		ruleElementClasses.add(RegulatoryOntologyHelper.CLS_ACTIVITY);
		return ontologyContainer.createConstraintClass(CLS_CONSTRAINT_SOD, ruleElementClasses);
	}
	
// FIXME: add the representation in the rule
	private List<OWLNamedIndividual> createDemoActivities(final RegulatoryOntologyHelper ontologyContainer) {
		List<OWLNamedIndividual> activities = new ArrayList<OWLNamedIndividual>();
		OWLNamedIndividual risikoAufbau = null;
		risikoAufbau = ontologyContainer.createREActivity("Aufbau Risikopositionen");
		if (risikoAufbau != null) {
			activities.add(risikoAufbau);
		}
		OWLNamedIndividual risikoKontrolle = null;
		risikoKontrolle = ontologyContainer.createREActivity("Kontrolle Risikopositionen");
		if (risikoKontrolle != null) {
			activities.add(risikoKontrolle);
		}
		return activities;
	}
	
	private OWLNamedIndividual createSODIndividual(final List<OWLNamedIndividual> activities, final RegulatoryOntologyHelper ontologyContainer) {
		return ontologyContainer.createConstraintIndividual("VierAugenPrinzipConstraint", CLS_CONSTRAINT_SOD, activities);
	}
	
	private OWLNamedIndividual createVierAugenPrinzip(final OWLNamedIndividual sodRule, final List<OWLNamedIndividual> activities, final OWLNamedIndividual sodIndividual, RegulatoryOntologyHelper ontologyContainer) {
		return ontologyContainer.createSituation("VierAugenPrinzip", Collections.singletonList(sodRule), activities, Collections.singletonList(sodIndividual));
	}
	
	private List<OWLNamedIndividual> createSigmaActivities(final RegulatoryOntologyHelper ontologyContainer) {
		List<OWLNamedIndividual> activities = new ArrayList<OWLNamedIndividual>();
		OWLNamedIndividual projektAnlegen = ontologyContainer.createREActivity("Projekt anlegen");
		if (projektAnlegen != null) {
			activities.add(projektAnlegen);
		}
		OWLNamedIndividual datenPruefen = ontologyContainer.createREActivity("Projektdaten pruefen");
		if (datenPruefen != null) {
			activities.add(datenPruefen);
		}
		return activities;
	}
	
	
	private OWLNamedIndividual createSigmaSODIndividual(final List<OWLNamedIndividual> activities, final RegulatoryOntologyHelper ontologyContainer) {
		return ontologyContainer.createConstraintIndividual("VierAugenPrinzipSigma", CLS_CONSTRAINT_SOD, activities);
	}
	private OWLNamedIndividual createSigmaVierAugenPrinzip(final OWLNamedIndividual sodRule, final List<OWLNamedIndividual> activities, final OWLNamedIndividual sodIndividual, RegulatoryOntologyHelper ontologyContainer) {
		return ontologyContainer.createSituation("SigmaVAP", Collections.singletonList(sodRule), activities, Collections.singletonList(sodIndividual));
	}
}
