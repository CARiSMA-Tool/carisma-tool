package carisma.regulatory.ontology.utils;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.coode.owlapi.owlxmlparser.OWLAnnotationPropertyElementHandler;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;

import carisma.processanalysis.texttools.KeywordCollection;
import carisma.processanalysis.texttools.KeywordSet;
import carisma.regulatory.ontology.model.RuleElementModel;
import carisma.regulatory.ontology.model.RuleElementRelationModel;
import carisma.regulatory.ontology.owl.OWL2AbstractAxiom;
import carisma.regulatory.ontology.owl.OWL2AbstractElement;

/**
 * Helper class to work conveniently with regulatory ontologies.
 * @author wenzel
 *
 */
public class RegulatoryOntologyHelper {
	
    private GenericOntologyHelper goh = null;
	// Ontology Classes
    // root
    public static final String CLS_RULE = "Rule";
    
    // RuleElements
    public static final String CLS_RULEELEMENT = "RuleElement";
    public static final String CLS_PROCESS = "Process";
    public static final String CLS_ROLE = "Role";
    public static final String CLS_ACTIVITY = "Activity";
    public static final String CLS_ARTIFACT = "Artifact";
    public static final String CLS_PROPERTY = "Property";
    
    // Checkable Situations, their associated constraints and their parameters
    public static final String CLS_SITUATION = "Situation";
    public static final String CLS_CONSTRAINT = "Constraint";
    
    // BDSG, BGB laws (juris)
    public static final String CLS_LAWRULE = "LawRule";
    public static final String CLS_LAW = "Law";
    public static final String CLS_SECTION = "Section";
    public static final String CLS_PARAGRAPH = "Paragraph";
    
    // BSI
    public static final String CLS_BSIRULE = "BSIRule";
    public static final String CLS_BSIELEMENT = "BSIElement";
    public static final String CLS_BSITHREAT = "BSIThreat";
	public static final String CLS_BSIMEASURE = "BSIMeasure";
	// MARisk VA
	public static final String CLS_MARISKRULE = "MARiskRule";
	public static final String CLS_MARISKCLAUSE = "MARiskClause";
	public static final String CLS_MARISKENTRY = "MARiskEntry";
	public static final String CLS_MARISKBINDING = "MARiskBinding";
	public static final String CLS_MARISKCOMMENT = "MARiskComment";
	// GoBS 
	public static final String CLS_GUIDELINES = "Guidelines";
	public static final String CLS_GUIDELINESENTRY = "GuidelinesEntry";
	
	// Properties
	// root
    public static final String PROP_RULE_TITLE = CLS_RULE + "/Title";
    public static final String PROP_RULE_UNPROVABLE = CLS_RULE + "/Unprovable";
         // TH: fuer die Erweiterung des Riskfinders im Zusammenhang mit der Demo
    public static final String PROP_RULE_KEYWORDS = CLS_RULE + "/Keywords"; // CSV-Zeile mit Keywords der jew. Regel 

    // RuleElements
    public static final String PROP_RULEELEMENT_TYPE = CLS_RULEELEMENT + "/ElementType";
    public static final String PROP_RULEELEMENT_NAME = CLS_RULEELEMENT + "/Name";
    // Checkable Situations, their associated constraints and their parameters
    public static final String PROP_SITUATION_NAME = CLS_SITUATION + "/Name";
    public static final String PROP_CONSTRAINT_SPECIFICATION = CLS_CONSTRAINT + "/Specification";
    
    // BDSG, BGB laws (juris)
    public static final String PROP_PARAGRAPH_NUMBER = CLS_PARAGRAPH + "/Number";
    public static final String PROP_SECTION_CONTENT = CLS_SECTION + "/Content";
	// BSI
	public static final String PROP_BSIRULE_CONTENT = CLS_BSIRULE + "/Content";
	// MARisk
	public static final String PROP_MARISKCLAUSE_NUMBER = CLS_MARISKCLAUSE + "/Number";
	public static final String PROP_MARISKCLAUSE_NAME= CLS_MARISKCLAUSE + "/Name";
	public static final String PROP_MARISKENTRY_NUMBER = CLS_MARISKENTRY + "/Number";
	public static final String PROP_MARISKENTRY_TEXT= CLS_MARISKENTRY + "/Text";
	//GoBS
	public static final String PROP_GUIDELINESENTRY_TEXT = CLS_GUIDELINESENTRY + "/Text";
	public static final String PROP_GUIDELINESENTRY_TITLE = CLS_GUIDELINESENTRY + "/Title";
	public static final String PROP_GUIDELINESENTRY_NAME = CLS_GUIDELINESENTRY + "/Name";

	// Associations
	// root
    public static final String REL_RULE_REFFEREDRULES = CLS_RULE + "/ReferredRules";
    // RuleElements
    public static final String REL_ACTIVITY_USEDARTIFACTS = CLS_ACTIVITY + "/UsedArtifacts";
    public static final String REL_ACTIVITY_PROPERTIES = CLS_ACTIVITY + "/Properties";
    public static final String REL_ARTIFACT_PROPERTIES = CLS_ARTIFACT + "/Properties";
    public static final String REL_PROCESS_ACTIVITIES = CLS_PROCESS + "/Activities";
    public static final String REL_PROCESS_PARTICIPANTS = CLS_PROCESS + "/Participants";
    public static final String REL_PROCESS_USEDARTIFACTS = CLS_PROCESS + "/UsedArtifacts";
    public static final String REL_PROCESS_PROPERTIES = CLS_PROCESS + "/Properties";
    public static final String REL_ROLE_PERFORMABLEACTIVITIES = CLS_ROLE + "/PerformableActivities";
    public static final String REL_ROLE_MANAGEDARTIFACTS = CLS_ROLE + "/ManagedArtifacts";
    public static final String REL_ROLE_PROPERTIES = CLS_ROLE + "/Properties";
    public static final String REL_RULEELEMENTS_RULEELEMENTS = CLS_RULEELEMENT + "/RelatedRuleElements";
// TODO: Check if these relations are okay or too specific
    public static final String REL_RULE_RULEELEMENTS = CLS_RULE + "/ContainedRuleElements";
    public static final String REL_RULE_CONTAINEDACTIVITIES = CLS_RULE + "/ContainedActivities";
    public static final String REL_RULE_CONTAINEDARTIFACTS = CLS_RULE + "/ContainedArtifacts";
    public static final String REL_RULE_CONTAINEDROLES = CLS_RULE + "/ContainedRoles";
    public static final String REL_RULE_CONTAINEDPROPERTIES = CLS_RULE + "/ContainedProperties";
    public static final String REL_RULE_CONTAINEDPROCESSES = CLS_RULE + "/ContainedProcesses";
    
    // Checkable Situations, their associated constraints and their parameters
    public static final String REL_SITUATION_INVOLVEDRULES = CLS_SITUATION + "/InvolvedRules";
    public static final String REL_SITUATION_INVOLVEDRULEELEMENTS = CLS_SITUATION + "/InvolvedRuleElements";
    public static final String REL_SITUATION_CHECKED_BY = CLS_SITUATION + "/checkedBy";
    public static final String REL_CONSTRAINT_HASPARAMETERS = CLS_CONSTRAINT + "/hasParameters";
    
    // BDSG, BGB laws (juris)
    public static final String REL_LAW_PARAGRAPHS = CLS_LAW + "/Paragraphs";
    public static final String REL_PARAGRAPH_SECTIONS = CLS_PARAGRAPH + "/Sections";
	// BSI
	public static final String REL_BSIELEMENT_THREATS = CLS_BSIELEMENT + "/ReferredThreats";
	public static final String REL_BSIELEMENT_MEASURES = CLS_BSIELEMENT + "/ReferredMeasures";
    // MARisk
    public static final String REL_MARISKCLAUSE_HASSUBCLAUSES= CLS_MARISKCLAUSE + "/hasSubClauses";
    public static final String REL_MARISKCLAUSE_HASENTRIES = CLS_MARISKCLAUSE + "/hasEntries";
    public static final String REL_MARISKBINDING_HASCOMMENTS = CLS_MARISKBINDING + "/hasComments";
    // GoBS
    public static final String REL_GUIDELINES_GUIDELINESENTRY = CLS_GUIDELINES + "/hasEntry";
    
    // Annotations attached to relations
    public static final String PROP_REL_CONSTRAINT_HASPARAMETERS_INDEX = REL_CONSTRAINT_HASPARAMETERS + "/Index";
    public static final String PROP_REL_RULE_RULEELEMENTS_REPRESENTATION = REL_RULE_RULEELEMENTS + "/Representation";
    public static final String PROP_REL_RULE_RULEELEMENTS_STARTINDEX = REL_RULE_RULEELEMENTS + "/StartIndex";
    public static final String PROP_REL_RULE_RULEELEMENTS_ENDINDEX = REL_RULE_RULEELEMENTS + "/EndIndex";
    public static final String PROP_REL_RULEELEMENTS_RULEELEMENTS_TYPE = REL_RULEELEMENTS_RULEELEMENTS + "/Type";
    public static final String PROP_REL_RULEELEMENTS_RULEELEMENTS_DESCRIPTION = REL_RULEELEMENTS_RULEELEMENTS + "/Description";
    
	// base IRI for a regulatory ontology
	public final static String ONTOLOGY_IRI_BASE = "http://carisma.umlsec.de/ontologies/regulatory/";
	
	private Map<String, OWLClass> owlClasses = null;
	private Map<String, OWLAnnotationProperty> owlAnnotationProperties = null;
	private Map<String, OWLObjectProperty> owlObjectProperties = null;
	private Map<String, OWLAnnotationProperty> owlRelationAnnotationProperties = null;
	private Map<String, String[]> relatedRuleElements = null;
	
	/**
	 * creates a new Roh.
	 */
	public RegulatoryOntologyHelper() {
	    this(new GenericOntologyHelper());
	}
	
	/**
	 * creates a new Roh with a given GenericOntologyHelper.
	 * @param usedGoh 
	 */
	public RegulatoryOntologyHelper(GenericOntologyHelper usedGoh) {
		goh = usedGoh;
		goh.setPrefix(ONTOLOGY_IRI_BASE);
		try {
		    initTypes();
		} catch (Exception e) {
		    throw new ExceptionInInitializerError("Initialisation of the types failed!");
		}
	}
	
	/**
	 * returns the used Ontology.
	 * @return
	 */
	public OWLOntology getOntology() {
	    return goh.getOntology();
	}
	
	/**
	 * creates a new Ontology and initializes the Ontology.
	 */
	public void createNewRegulatoryOntology() {
        goh.createNewOntology(ONTOLOGY_IRI_BASE);
        initializeOntology(goh);
	}
	
	
	/**
	 * returns the GenericOntologyHelper.
	 * @return
	 */
	public GenericOntologyHelper getGoh() {
		return goh;
	}

	/**
	 * returns the OWLClass, which matches the given name. Or null if there is no match.
	 * @param name
	 * @return
	 */
	public OWLClass getOWLClass(String name) {
		return owlClasses.get(name);
	}
	
	/**
	 * returns the OWLAnnotationProperty with the given name. Or null if there is no match.
	 * @param name
	 * @return
	 */
	public OWLAnnotationProperty getOWLAnnotationProperty(String name) {
		return owlAnnotationProperties.get(name);
	}
	
	/**
     * returns the OWLObjectProperty with the given name. Or null if there is no match.
     * @param name
     * @return
     */
	public OWLObjectProperty getOWLObjectProperty(String name) {
	    return owlObjectProperties.get(name);
	}
	
	/**
	 * initializes the types which will be used in the Ontology.
	 */
	private void initTypes() {
		this.owlClasses = new HashMap<String, OWLClass>();
		// root
        this.owlClasses.put(CLS_RULE, goh.getOWLClass(CLS_RULE));
        // RuleElements
        this.owlClasses.put(CLS_RULEELEMENT, goh.getOWLClass(CLS_RULEELEMENT));
        this.owlClasses.put(CLS_PROCESS, goh.getOWLClass(CLS_PROCESS));
        this.owlClasses.put(CLS_ROLE, goh.getOWLClass(CLS_ROLE));
        this.owlClasses.put(CLS_ACTIVITY, goh.getOWLClass(CLS_ACTIVITY));
        this.owlClasses.put(CLS_ARTIFACT, goh.getOWLClass(CLS_ARTIFACT));
        this.owlClasses.put(CLS_PROPERTY, goh.getOWLClass(CLS_PROPERTY));
        
        // Checkable Situations, their associated constraints and their parameters
        this.owlClasses.put(CLS_SITUATION, goh.getOWLClass(CLS_SITUATION));
        this.owlClasses.put(CLS_CONSTRAINT, goh.getOWLClass(CLS_CONSTRAINT));
        
        // BDSG, BGB laws (juris)
        this.owlClasses.put(CLS_LAWRULE, goh.getOWLClass(CLS_LAWRULE));
        this.owlClasses.put(CLS_LAW, goh.getOWLClass(CLS_LAW));
        this.owlClasses.put(CLS_PARAGRAPH, goh.getOWLClass(CLS_PARAGRAPH));
        this.owlClasses.put(CLS_SECTION, goh.getOWLClass(CLS_SECTION));
        // BSI
		this.owlClasses.put(CLS_BSIRULE, goh.getOWLClass(CLS_BSIRULE));
		this.owlClasses.put(CLS_BSIELEMENT, goh.getOWLClass(CLS_BSIELEMENT));
		this.owlClasses.put(CLS_BSITHREAT, goh.getOWLClass(CLS_BSITHREAT));
		this.owlClasses.put(CLS_BSIMEASURE, goh.getOWLClass(CLS_BSIMEASURE));
		// MARisk
		this.owlClasses.put(CLS_MARISKRULE, this.goh.getOWLClass(CLS_MARISKRULE));
		this.owlClasses.put(CLS_MARISKCLAUSE, this.goh.getOWLClass(CLS_MARISKCLAUSE));
		this.owlClasses.put(CLS_MARISKENTRY, this.goh.getOWLClass(CLS_MARISKENTRY));
		this.owlClasses.put(CLS_MARISKBINDING, this.goh.getOWLClass(CLS_MARISKBINDING));
		this.owlClasses.put(CLS_MARISKCOMMENT, this.goh.getOWLClass(CLS_MARISKCOMMENT));
		// GoBS
		this.owlClasses.put(CLS_GUIDELINES, this.goh.getOWLClass(CLS_GUIDELINES));
		this.owlClasses.put(CLS_GUIDELINESENTRY, this.goh.getOWLClass(CLS_GUIDELINESENTRY));
		
		
		this.owlAnnotationProperties = new HashMap<String, OWLAnnotationProperty>();
		// root
        this.owlAnnotationProperties.put(PROP_RULE_TITLE, goh.getOWLAnnotationProperty(PROP_RULE_TITLE));
        this.owlAnnotationProperties.put(PROP_RULE_UNPROVABLE, goh.getOWLAnnotationProperty(PROP_RULE_UNPROVABLE));
        this.owlAnnotationProperties.put(PROP_RULE_KEYWORDS, goh.getOWLAnnotationProperty(PROP_RULE_KEYWORDS));
        // Checkable Situations, their associated constraints and their parameters
        this.owlAnnotationProperties.put(PROP_SITUATION_NAME, goh.getOWLAnnotationProperty(PROP_SITUATION_NAME));
        this.owlAnnotationProperties.put(PROP_CONSTRAINT_SPECIFICATION, goh.getOWLAnnotationProperty(PROP_CONSTRAINT_SPECIFICATION));
        
        // BDSG, BGB laws (juris)
		this.owlAnnotationProperties.put(PROP_SECTION_CONTENT, goh.getOWLAnnotationProperty(PROP_SECTION_CONTENT));
		this.owlAnnotationProperties.put(PROP_PARAGRAPH_NUMBER, goh.getOWLAnnotationProperty(PROP_PARAGRAPH_NUMBER));
		// BSI
		this.owlAnnotationProperties.put(PROP_BSIRULE_CONTENT, goh.getOWLAnnotationProperty(PROP_BSIRULE_CONTENT));
		// Marisk
		this.owlAnnotationProperties.put(PROP_MARISKCLAUSE_NAME, goh.getOWLAnnotationProperty(PROP_MARISKCLAUSE_NAME));
		this.owlAnnotationProperties.put(PROP_MARISKCLAUSE_NUMBER, goh.getOWLAnnotationProperty(PROP_MARISKCLAUSE_NUMBER));
		this.owlAnnotationProperties.put(PROP_MARISKENTRY_NUMBER, goh.getOWLAnnotationProperty(PROP_MARISKENTRY_NUMBER));
		this.owlAnnotationProperties.put(PROP_MARISKENTRY_TEXT, goh.getOWLAnnotationProperty(PROP_MARISKENTRY_TEXT));
		// GoBS
//		this.owlAnnotationProperties.put(PROP_GUIDELINESENTRY_KEYWORDS, goh.getOWLAnnotationProperty(PROP_GUIDELINESENTRY_KEYWORDS));
		this.owlAnnotationProperties.put(PROP_GUIDELINESENTRY_TEXT, goh.getOWLAnnotationProperty(PROP_GUIDELINESENTRY_TEXT));
		this.owlAnnotationProperties.put(PROP_GUIDELINESENTRY_TITLE, goh.getOWLAnnotationProperty(PROP_GUIDELINESENTRY_TITLE));
		
		this.owlObjectProperties = new HashMap<String, OWLObjectProperty>();
		// root
        this.owlObjectProperties.put(REL_RULE_REFFEREDRULES, goh.getOWLObjectProperty(REL_RULE_REFFEREDRULES));
        
        // RuleElements
        this.owlAnnotationProperties.put(PROP_RULEELEMENT_NAME, goh.getOWLAnnotationProperty(PROP_RULEELEMENT_NAME));
        this.owlAnnotationProperties.put(PROP_RULEELEMENT_TYPE, goh.getOWLAnnotationProperty(PROP_RULEELEMENT_TYPE));
        this.owlObjectProperties.put(REL_ACTIVITY_PROPERTIES, goh.getOWLObjectProperty(REL_ACTIVITY_PROPERTIES));
        this.owlObjectProperties.put(REL_ACTIVITY_USEDARTIFACTS, goh.getOWLObjectProperty(REL_ACTIVITY_USEDARTIFACTS));
        this.owlObjectProperties.put(REL_ARTIFACT_PROPERTIES, goh.getOWLObjectProperty(REL_ARTIFACT_PROPERTIES));
        this.owlObjectProperties.put(REL_PROCESS_ACTIVITIES, goh.getOWLObjectProperty(REL_PROCESS_ACTIVITIES));
        this.owlObjectProperties.put(REL_PROCESS_PARTICIPANTS, goh.getOWLObjectProperty(REL_PROCESS_PARTICIPANTS));
        this.owlObjectProperties.put(REL_PROCESS_PROPERTIES, goh.getOWLObjectProperty(REL_PROCESS_PROPERTIES));
        this.owlObjectProperties.put(REL_PROCESS_USEDARTIFACTS, goh.getOWLObjectProperty(REL_PROCESS_USEDARTIFACTS));
        this.owlObjectProperties.put(REL_ROLE_PERFORMABLEACTIVITIES, goh.getOWLObjectProperty(REL_ROLE_PERFORMABLEACTIVITIES));
        this.owlObjectProperties.put(REL_ROLE_MANAGEDARTIFACTS, goh.getOWLObjectProperty(REL_ROLE_MANAGEDARTIFACTS));
        this.owlObjectProperties.put(REL_ROLE_PROPERTIES, goh.getOWLObjectProperty(REL_ROLE_PROPERTIES));
        this.owlObjectProperties.put(REL_RULE_RULEELEMENTS, goh.getOWLObjectProperty(REL_RULE_RULEELEMENTS));
        this.owlObjectProperties.put(REL_RULE_CONTAINEDACTIVITIES, goh.getOWLObjectProperty(REL_RULE_CONTAINEDACTIVITIES));
        this.owlObjectProperties.put(REL_RULE_CONTAINEDARTIFACTS, goh.getOWLObjectProperty(REL_RULE_CONTAINEDARTIFACTS));
        this.owlObjectProperties.put(REL_RULE_CONTAINEDROLES, goh.getOWLObjectProperty(REL_RULE_CONTAINEDROLES));
        this.owlObjectProperties.put(REL_RULE_CONTAINEDPROPERTIES, goh.getOWLObjectProperty(REL_RULE_CONTAINEDPROPERTIES));
        this.owlObjectProperties.put(REL_RULE_CONTAINEDPROCESSES, goh.getOWLObjectProperty(REL_RULE_CONTAINEDPROCESSES));
        this.owlObjectProperties.put(REL_RULEELEMENTS_RULEELEMENTS, goh.getOWLObjectProperty(REL_RULEELEMENTS_RULEELEMENTS));
        
        // Checkable Situations, their associated constraints and their parameters
        this.owlObjectProperties.put(REL_SITUATION_INVOLVEDRULES, goh.getOWLObjectProperty(REL_SITUATION_INVOLVEDRULES));
        this.owlObjectProperties.put(REL_SITUATION_INVOLVEDRULEELEMENTS, goh.getOWLObjectProperty(REL_SITUATION_INVOLVEDRULEELEMENTS));
        this.owlObjectProperties.put(REL_SITUATION_CHECKED_BY, goh.getOWLObjectProperty(REL_SITUATION_CHECKED_BY));
        this.owlObjectProperties.put(REL_CONSTRAINT_HASPARAMETERS, goh.getOWLObjectProperty(REL_CONSTRAINT_HASPARAMETERS));
        
        // BDSG, BGB laws (juris)
        this.owlObjectProperties.put(REL_LAW_PARAGRAPHS, goh.getOWLObjectProperty(REL_LAW_PARAGRAPHS));
        this.owlObjectProperties.put(REL_PARAGRAPH_SECTIONS, goh.getOWLObjectProperty(REL_PARAGRAPH_SECTIONS));
        this.owlObjectProperties.put(REL_SITUATION_INVOLVEDRULES, goh.getOWLObjectProperty(REL_SITUATION_INVOLVEDRULES));
        // BSI
        this.owlObjectProperties.put(REL_BSIELEMENT_MEASURES, goh.getOWLObjectProperty(REL_BSIELEMENT_MEASURES));
        this.owlObjectProperties.put(REL_BSIELEMENT_THREATS, goh.getOWLObjectProperty(REL_BSIELEMENT_THREATS));
		// MARisk
		this.owlObjectProperties.put(REL_MARISKCLAUSE_HASSUBCLAUSES, goh.getOWLObjectProperty(REL_MARISKCLAUSE_HASSUBCLAUSES));
		this.owlObjectProperties.put(REL_MARISKCLAUSE_HASENTRIES, goh.getOWLObjectProperty(REL_MARISKCLAUSE_HASENTRIES));
		this.owlObjectProperties.put(REL_MARISKBINDING_HASCOMMENTS, goh.getOWLObjectProperty(REL_MARISKBINDING_HASCOMMENTS));
		// GoBS
		this.owlObjectProperties.put(REL_GUIDELINES_GUIDELINESENTRY, goh.getOWLObjectProperty(REL_GUIDELINES_GUIDELINESENTRY));
		
		owlRelationAnnotationProperties = new HashMap<String, OWLAnnotationProperty>();
//TODO: Is this correct?
		owlRelationAnnotationProperties.put(PROP_REL_CONSTRAINT_HASPARAMETERS_INDEX, goh.getOWLAnnotationProperty(PROP_REL_CONSTRAINT_HASPARAMETERS_INDEX));
        owlRelationAnnotationProperties.put(PROP_REL_RULE_RULEELEMENTS_REPRESENTATION, goh.getOWLAnnotationProperty(PROP_REL_RULE_RULEELEMENTS_REPRESENTATION));
        owlRelationAnnotationProperties.put(PROP_REL_RULE_RULEELEMENTS_STARTINDEX, goh.getOWLAnnotationProperty(PROP_REL_RULE_RULEELEMENTS_STARTINDEX));
        owlRelationAnnotationProperties.put(PROP_REL_RULE_RULEELEMENTS_ENDINDEX, goh.getOWLAnnotationProperty(PROP_REL_RULE_RULEELEMENTS_ENDINDEX));
        owlRelationAnnotationProperties.put(PROP_REL_RULEELEMENTS_RULEELEMENTS_TYPE, goh.getOWLAnnotationProperty(PROP_REL_RULEELEMENTS_RULEELEMENTS_TYPE));
        owlRelationAnnotationProperties.put(PROP_REL_RULEELEMENTS_RULEELEMENTS_DESCRIPTION, goh.getOWLAnnotationProperty(PROP_REL_RULEELEMENTS_RULEELEMENTS_DESCRIPTION));
        
            // init a map with the right relations between two rule elements and if they are in the right order
        relatedRuleElements = new HashMap<String, String[]>();
            // relation between Activities and Properties
        relatedRuleElements.put("Activity,Property", new String[]{RegulatoryOntologyHelper.REL_ACTIVITY_PROPERTIES, "true"});
        relatedRuleElements.put("Property,Activity", new String[]{RegulatoryOntologyHelper.REL_ACTIVITY_PROPERTIES, "false"});
            // relation between Activities and Artifacts
        relatedRuleElements.put("Activity,Artifact", new String[]{RegulatoryOntologyHelper.REL_ACTIVITY_USEDARTIFACTS, "true"});
        relatedRuleElements.put("Artifact,Activity", new String[]{RegulatoryOntologyHelper.REL_ACTIVITY_USEDARTIFACTS, "false"});
            // relation between Artifacts and Properties
        relatedRuleElements.put("Artifact,Property", new String[]{RegulatoryOntologyHelper.REL_ARTIFACT_PROPERTIES, "true"});
        relatedRuleElements.put("Property,Artifact", new String[]{RegulatoryOntologyHelper.REL_ARTIFACT_PROPERTIES, "false"});
            // relation between Process and Activities
        relatedRuleElements.put("Activity,Process", new String[]{RegulatoryOntologyHelper.REL_PROCESS_ACTIVITIES, "false"});
        relatedRuleElements.put("Process,Activity", new String[]{RegulatoryOntologyHelper.REL_PROCESS_ACTIVITIES, "true"});
            // relation between Process and Roles
        relatedRuleElements.put("Process,Role", new String[]{RegulatoryOntologyHelper.REL_PROCESS_PARTICIPANTS, "true"});
        relatedRuleElements.put("Role,Process", new String[]{RegulatoryOntologyHelper.REL_PROCESS_PARTICIPANTS, "false"});
            // relation between Process and Properties
        relatedRuleElements.put("Process,Property", new String[]{RegulatoryOntologyHelper.REL_PROCESS_PROPERTIES, "true"});
        relatedRuleElements.put("Property,Process", new String[]{RegulatoryOntologyHelper.REL_PROCESS_PROPERTIES, "false"});
            // relation between Process and Artifacts
        relatedRuleElements.put("Process,Artifact", new String[]{RegulatoryOntologyHelper.REL_PROCESS_USEDARTIFACTS, "true"});
        relatedRuleElements.put("Artifact,Process", new String[]{RegulatoryOntologyHelper.REL_PROCESS_USEDARTIFACTS, "false"});
            // relation between Roles and Artifacts
        relatedRuleElements.put("Role,Artifact", new String[]{RegulatoryOntologyHelper.REL_ROLE_MANAGEDARTIFACTS, "true"});
        relatedRuleElements.put("Artifact,Role", new String[]{RegulatoryOntologyHelper.REL_ROLE_MANAGEDARTIFACTS, "false"});
            // relation between Roles and Activities
        relatedRuleElements.put("Role,Activity", new String[]{RegulatoryOntologyHelper.REL_ROLE_PERFORMABLEACTIVITIES, "true"});
        relatedRuleElements.put("Activity,Role", new String[]{RegulatoryOntologyHelper.REL_ROLE_PERFORMABLEACTIVITIES, "false"});
            // relation between Roles and Properties
        relatedRuleElements.put("Role,Property", new String[]{RegulatoryOntologyHelper.REL_ROLE_PROPERTIES, "true"});
        relatedRuleElements.put("Property,Role", new String[]{RegulatoryOntologyHelper.REL_ROLE_PROPERTIES, "false"});
	}
	
	/**
	 * checks if this Ontology has been initialized.
	 * @return
	 */
	public boolean isOntologyInitialized() {
	    OWLOntology currentOntology = goh.getOntology();
	    if (currentOntology == null) {
	        return false;
	    }
// FIXME: Check if the current ontology contains some axioms (CLS_RULEELEMENT and CLS_MARISKCOMMENT e.g.)
	    return true;
	}
	/**
	 * Creates the classes for a new (i.e. empty) ontology.
	 * @param goh
	 */
	public void initializeOntology(final GenericOntologyHelper outsideGoh) {
	    GenericOntologyHelper usedGoh = null;
	    if (outsideGoh != null) {
	        usedGoh = outsideGoh;
	    } else if (goh != null) {
	        usedGoh = goh;
	    } else {
	        System.err.println("ERROR! Can't initialize regulatory ontology. No valid GenericOntologyHelper!");
	        return;
	    }
	    if (usedGoh.getOntology() == null) {
            System.err.println("ERROR! Can't initialize regulatory ontology. No current ontology!");
            return;	        
	    }
	    usedGoh.setPrefix(ONTOLOGY_IRI_BASE);
        // RuleElements
	    usedGoh.createSubclassOfAxiom(CLS_PROCESS, CLS_RULEELEMENT);
	    usedGoh.createSubclassOfAxiom(CLS_ROLE, CLS_RULEELEMENT);
	    usedGoh.createSubclassOfAxiom(CLS_ACTIVITY, CLS_RULEELEMENT);
	    usedGoh.createSubclassOfAxiom(CLS_PROPERTY, CLS_RULEELEMENT);
	    usedGoh.createSubclassOfAxiom(CLS_ARTIFACT, CLS_RULEELEMENT);   
        // BDSG, BGB laws (juris)
	    usedGoh.createSubclassOfAxiom(CLS_LAWRULE, CLS_RULE);
	    usedGoh.createSubclassOfAxiom(CLS_LAW, CLS_LAWRULE);
	    usedGoh.createSubclassOfAxiom(CLS_PARAGRAPH, CLS_LAWRULE);
		usedGoh.createSubclassOfAxiom(CLS_SECTION, CLS_LAWRULE);
		// BSI
		usedGoh.createSubclassOfAxiom(CLS_BSIRULE, CLS_RULE);
		usedGoh.createSubclassOfAxiom(CLS_BSIELEMENT, CLS_BSIRULE);
		usedGoh.createSubclassOfAxiom(CLS_BSITHREAT, CLS_BSIRULE);
		usedGoh.createSubclassOfAxiom(CLS_BSIMEASURE, CLS_BSIRULE);		
		// MARisk
		usedGoh.createSubclassOfAxiom(CLS_MARISKRULE, CLS_RULE);
		usedGoh.createSubclassOfAxiom(CLS_MARISKCLAUSE, CLS_MARISKRULE);
		usedGoh.createSubclassOfAxiom(CLS_MARISKENTRY, CLS_MARISKRULE);
		usedGoh.createSubclassOfAxiom(CLS_MARISKBINDING, CLS_MARISKENTRY);
		usedGoh.createSubclassOfAxiom(CLS_MARISKCOMMENT, CLS_MARISKENTRY);
		// GoBS
		usedGoh.createSubclassOfAxiom(CLS_GUIDELINES, CLS_RULE);
		usedGoh.createSubclassOfAxiom(CLS_GUIDELINESENTRY, CLS_GUIDELINES);
	}
	
	/**
	 * returns the relatedRuleElements map.
	 * @return
	 */
	public final Map<String, String[]> getRelatedRuleElementsMap() {
	    return relatedRuleElements;
	}
	
	/**
	 * saves the Ontology to a given file.
	 * @param ontologyFile
	 */
	public void saveOntologyToFile(final File ontologyFile) {
	    getGoh().saveOntologyToFile(ontologyFile);
	}

	/**
	 * Creates a rule-ruleElement relation with a given a rule, the name of a possibly new rule element and the values for the relation annotations.
	 * @param rule
	 * @param ruleElementName
	 * @param ruleElementType
	 * @param textualRepresentation
	 * @param startIndex
	 * @param endIndex
	 * @return
	 */
    public OWLObjectPropertyAssertionAxiom createContainedRuleElementsRelation(final OWLNamedIndividual rule, final String ruleElementName, final String ruleElementType, String textualRepresentation, int startIndex, int endIndex) {
        String id = IndividualIDGenerator.generateRuleElementID(ruleElementName, ruleElementType);
        OWLNamedIndividual ruleElement = createRE(id, ruleElementType, ruleElementName);
        return createContainedRuleElementsRelation(rule, ruleElement, textualRepresentation, startIndex, endIndex);
    }
    /**
     * Creates a rule-ruleElement relation with a given a rule, the name of a possibly new rule element and the values for the relation annotations.
     * @param rule
     * @param ruleElementName
     * @param ruleElementType
     * @param textualRepresentation
     * @param startIndex
     * @param endIndex
     * @return
     */
    public OWLObjectPropertyAssertionAxiom createContainedRuleElementsRelation(final OWL2AbstractElement rule,
            final String ruleElementName, final String ruleElementType, String textualRepresentation, int startIndex, int endIndex) {
        return createContainedRuleElementsRelation(getIndividualFromOWL2AbstractElement(rule), 
                                                    ruleElementName, ruleElementType, textualRepresentation, startIndex, endIndex);
    }
    
    /**
     * Creates a rule-ruleElement relation with a given rule and ruleElement individual and the values for the relation annotations.
     * @param rule - the rule containing the rule element
     * @param ruleElement - the rule element associated with the rule
     * @param textualRepresentation - the textual representation of the rule element in the rule
     * @param startIndex - the index in the rule where the rule element representation starts
     * @param endIndex - the index in the rule where the rule element representation ends
     * @return
     */
	public OWLObjectPropertyAssertionAxiom createContainedRuleElementsRelation(final OWLNamedIndividual rule, final OWLNamedIndividual ruleElement, String textualRepresentation, int startIndex, int endIndex) {
	    Set<Annotation> annotations = new HashSet<Annotation>();
	    annotations.add(new Annotation(RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_REPRESENTATION, textualRepresentation));
        annotations.add(new Annotation(RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_STARTINDEX, startIndex));
        annotations.add(new Annotation(RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_ENDINDEX, endIndex));
        return goh.createSimpleRelationship(rule, ruleElement, RegulatoryOntologyHelper.REL_RULE_RULEELEMENTS, annotations);	    
	}
	 /**
     * Creates a rule-ruleElement relation with a given rule and ruleElement and the values for the relation annotations.
     * @param rule - the rule containing the rule element
     * @param ruleElement - the rule element associated with the rule
     * @param textualRepresentation - the textual representation of the rule element in the rule
     * @param startIndex - the index in the rule where the rule element representation starts
     * @param endIndex - the index in the rule where the rule element representation ends
     * @return
     */
	public OWLObjectPropertyAssertionAxiom createContainedRuleElementsRelation(final OWL2AbstractElement rule, 
	            final OWL2AbstractElement ruleElement, String textualRepresentation, int startIndex, int endIndex) {
	    return createContainedRuleElementsRelation(getIndividualFromOWL2AbstractElement(rule), 
	                                                getIndividualFromOWL2AbstractElement(ruleElement), 
	                                                textualRepresentation, startIndex, endIndex);
	}
	
	/**
	 * Creates a ruleElement-ruleElement relation with a given sourceRuleElement and a given targetRuleElement 
	 * and the values for the relation annotations.
	 * @param sourceRuleElement
	 * @param targetRuleElement
	 * @param relationType
	 * @param description
	 * @return
	 */
	public OWLObjectPropertyAssertionAxiom createRelatedRuleElementsRelation(final OWLNamedIndividual sourceRuleElement, final OWLNamedIndividual targetRuleElement, final String relationType, final String description) {
	    Set<Annotation> annotations = new HashSet<Annotation>();
	    annotations.add(new Annotation(RegulatoryOntologyHelper.PROP_REL_RULEELEMENTS_RULEELEMENTS_TYPE, relationType));
	    annotations.add(new Annotation(RegulatoryOntologyHelper.PROP_REL_RULEELEMENTS_RULEELEMENTS_DESCRIPTION, description));
	    return goh.createSimpleRelationship(sourceRuleElement, targetRuleElement, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS, annotations);
	}
	/**
     * Creates a ruleElement-ruleElement relation with a given sourceRuleElement and a given targetRuleElement 
     * and the values for the relation annotations.
     * @param sourceRuleElement
     * @param targetRuleElement
     * @param relationType
     * @param description
     * @return
     */
	public OWLObjectPropertyAssertionAxiom createRelatedRuleElementsRelation(final OWL2AbstractElement sourceRuleElement, 
	                                                                         final OWL2AbstractElement targetRuleElement, 
	                                                                         final String relationType, final String description) {
	    return createRelatedRuleElementsRelation(getIndividualFromOWL2AbstractElement(sourceRuleElement),
	            getIndividualFromOWL2AbstractElement(targetRuleElement),
	            relationType, description);
	}

    /**
     * Creates a new ruleelement. 
     * A ruleelement consists of an id, a ruleelement-type and a name.
     * @param id
     * @param name
     * @return
     */
    public OWLNamedIndividual createRE(String id, String type, String name) {
        OWLNamedIndividual ruleElement = goh.createOWLIndividual(id, type);
        setRuleElementProperties(ruleElement, type, name);
        return ruleElement;
    }

 
    /**
     * Returns ruleelement with the type Activity.
     * For this purpose a new individual id is generated with the name and the type of the ruleelement. In the Ontology it will be
     * checked if the is already an individual with the id contained, and otherwise a new individual will be created.
     * @param ruleElementName
     * @return
     */
    public OWLNamedIndividual createREActivity(final String ruleElementName) {
        String ruleElementId = IndividualIDGenerator.generateRuleElementID(ruleElementName, CLS_ACTIVITY); 
        OWLNamedIndividual ruleElement = goh.getIndividualById(ruleElementId);
        if (ruleElement == null || !goh.getTypeOfIndividual(ruleElement).getFragment().equals(CLS_ACTIVITY)) {
            ruleElement = createRE(ruleElementId, CLS_ACTIVITY, ruleElementName);
        }
        return ruleElement;
    }
    
    /**
     * Returns ruleelement with the type Artifact.
     * For this purpose a new individual id is generated with the name and the type of the ruleelement. In the Ontology it will be
     * checked if the is already an individual with the id contained, and otherwise a new individual will be created.
     * @param ruleElementName
     * @return
     */
    public OWLNamedIndividual createREArtifact(final String ruleElementName) {
        String ruleElementId = IndividualIDGenerator.generateRuleElementID(ruleElementName, CLS_ARTIFACT); 
        OWLNamedIndividual ruleElement = goh.getIndividualById(ruleElementId);
        if (ruleElement == null || !goh.getTypeOfIndividual(ruleElement).getFragment().equals(CLS_ARTIFACT)) {
            ruleElement = createRE(ruleElementId, CLS_ARTIFACT, ruleElementName);
        }
        return ruleElement;
    }
    
    /**
     * Returns ruleelement with the type Process.
     * For this purpose a new individual id is generated with the name and the type of the ruleelement. In the Ontology it will be
     * checked if the is already an individual with the id contained, and otherwise a new individual will be created.
     * @param ruleElementName
     * @return
     */
    public OWLNamedIndividual createREProcess(final String ruleElementName) {
        String ruleElementId = IndividualIDGenerator.generateRuleElementID(ruleElementName, CLS_PROCESS); 
        OWLNamedIndividual ruleElement = goh.getIndividualById(ruleElementId);
        if (ruleElement == null || !goh.getTypeOfIndividual(ruleElement).getFragment().equals(CLS_PROCESS)) {
            ruleElement = createRE(ruleElementId, CLS_PROCESS, ruleElementName);
        }
        return ruleElement;
    }
    
    /**
     * Returns ruleelement with the type Poperty.
     * For this purpose a new individual id is generated with the name and the type of the ruleelement. In the Ontology it will be
     * checked if the is already an individual with the id contained, and otherwise a new individual will be created.
     * @param ruleElementName
     * @return
     */
    public OWLNamedIndividual createREProperty(final String ruleElementName) {
        String ruleElementId = IndividualIDGenerator.generateRuleElementID(ruleElementName, CLS_PROPERTY); 
        OWLNamedIndividual ruleElement = goh.getIndividualById(ruleElementId);
        if (ruleElement == null || !goh.getTypeOfIndividual(ruleElement).getFragment().equals(CLS_PROPERTY)) {
            ruleElement = createRE(ruleElementId, CLS_PROPERTY, ruleElementName);
        }
        return ruleElement;
    }
    
    /**
     * Returns ruleelement with the type Role.
     * For this purpose a new individual id is generated with the name and the type of the ruleelement. In the Ontology it will be
     * checked if the is already an individual with the id contained, and otherwise a new individual will be created.
     * @param ruleElementName
     * @return
     */
    public OWLNamedIndividual createRERole(final String ruleElementName) {
        String ruleElementId = IndividualIDGenerator.generateRuleElementID(ruleElementName, CLS_ROLE); 
        OWLNamedIndividual ruleElement = goh.getIndividualById(ruleElementId);
        if (ruleElement == null || !goh.getTypeOfIndividual(ruleElement).getFragment().equals(CLS_ROLE)) {
            ruleElement = createRE(ruleElementId, CLS_ROLE, ruleElementName);
        }
        return ruleElement;
    }
   
    /**
     * Sets the common properties for RuleElements.
     * @param ruleElement - the rule element to set the properties for
     * @param type
     * @param name
     */
    private void setRuleElementProperties(final OWLNamedIndividual ruleElement, String type, String name) {
        goh.createAnnotation(ruleElement, RegulatoryOntologyHelper.PROP_RULEELEMENT_TYPE, type);
        goh.createAnnotation(ruleElement, RegulatoryOntologyHelper.PROP_RULEELEMENT_NAME, name);
    }
// TODO: (DW) Unsure if this would work with an existing relation, now doing this when creating the representation 
//    /**
//     * Sets the relation annotations for a rule element representation in a rule, meaning the indices in the rule text
//     * and the textual representation of the element.
//     * @param ruleElement - the rule element to be represented in the rule
//     * @param text - the textual representation of the rule element in the rule
//     * @param startIndex - the beginning index of the rule element
//     * @param endIndex - the end index of the rule element
//     */
//    private void setRepresentationProperties(final OWLNamedIndividual ruleElement, String text, String startIndex, String endIndex) {
//        goh.createAnnotation(ruleElement, RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_REPRESENTATION, text);
//        goh.createAnnotation(ruleElement, RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_STARTINDEX, startIndex);
//        goh.createAnnotation(ruleElement, RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_ENDINDEX, endIndex);        
//    }

    // BDSG, BGB laws (juris)
	/**
	 * Creates a Law individual.
	 * @param id
	 * @param name
	 * @return
	 */
	public OWLNamedIndividual createLaw(String id, String name) {
		OWLNamedIndividual law = goh.createOWLIndividual(id, RegulatoryOntologyHelper.CLS_LAW);
		goh.createAnnotation(law, RegulatoryOntologyHelper.PROP_RULE_TITLE, name);
		return law;
	}
	
	/**
	 * Creates a situation involving the given rules and rule elements which is checked by the given constraints.
	 * @param name - the name of the situation (e.g. Vier-Augen-Prinzip)
	 * @param rules - the rules that this situation applies to
	 * @param ruleElements - the ruleElement this situation entails
	 * @return - the created situation individual
	 */
    public OWLNamedIndividual createSituation(
            final String name, final List<OWLNamedIndividual> rules,
            final List<OWLNamedIndividual> ruleElements, final List<OWLNamedIndividual> constraints) {
        String situationId = IndividualIDGenerator.generateSituationID(rules, name);
        OWLNamedIndividual situation = goh.createOWLIndividual(situationId, RegulatoryOntologyHelper.CLS_SITUATION);
        if (situation == null) {
            return null;
        }
        goh.createAnnotation(situation, RegulatoryOntologyHelper.PROP_SITUATION_NAME, name);
        for (OWLNamedIndividual rule : rules) {
            goh.createSimpleRelationship(situation, rule, RegulatoryOntologyHelper.REL_SITUATION_INVOLVEDRULES);
        }
        for (OWLNamedIndividual ruleElement : ruleElements) {
            goh.createSimpleRelationship(situation, ruleElement, RegulatoryOntologyHelper.REL_SITUATION_INVOLVEDRULEELEMENTS);
        }
        for (OWLNamedIndividual constraint : constraints) {
            goh.createSimpleRelationship(situation, constraint, RegulatoryOntologyHelper.REL_SITUATION_CHECKED_BY);
        }
        return situation;
    }
    /**
     * Creates a situation involving the given rules and ruleelements which is checked by the given constraints.
     * @param name - the name of the situation (e.g. Vier-Augen-Prinzip)
     * @param rules - the rules that this situation applies to
     * @param ruleElements - the ruleElement this situation entails
     * @return - the created situation individual
     */
    public OWLNamedIndividual createSituation(final String name, final Set<OWL2AbstractElement> rules, 
                                              final Set<OWL2AbstractElement> ruleElements, final Set<OWL2AbstractElement> constraints) {
        return createSituation(name, (List<OWLNamedIndividual>) getIndividualsFromOWL2AbstractElements(rules).values(), 
                                     (List<OWLNamedIndividual>) getIndividualsFromOWL2AbstractElements(ruleElements).values(), 
                                     (List<OWLNamedIndividual>) getIndividualsFromOWL2AbstractElements(constraints).values());
    }
    
    /**
     * returns a list of situations related to the given ruleName
     * @param ruleName the given rule
     * @return situations or an empty list
     */
    public List<OWLNamedIndividual> getSituations(final String ruleName) {
        List<OWLNamedIndividual> relatedSituations = new ArrayList<OWLNamedIndividual>();
        OWLNamedIndividual rule = goh.getIndividualById(ruleName);
//        Set<OWLNamedIndividual> situations = goh.getIndividuals(RegulatoryOntologyHelper.CLS_SITUATION, true);
        try {
            for (OWLNamedIndividual situation : goh.getInverseRelatedIndividuals(rule, 
                                                RegulatoryOntologyHelper.REL_SITUATION_INVOLVEDRULES)) {
                relatedSituations.add(situation);
            }
        } catch (NullPointerException n) {
            /* do nothing, there was no inverse related individual found */
        }
        return relatedSituations;
    }
    
    
	/**
	 * Creates a Paragraph individual.
	 * @param law
	 * @param number
	 * @param name
	 * @return
	 */
	public OWLNamedIndividual createParagraph(OWLNamedIndividual law, String number, String name) {
		OWLNamedIndividual par = goh.createOWLIndividual(law.getIRI().getFragment()+"-"+number, RegulatoryOntologyHelper.CLS_PARAGRAPH);
		goh.createAnnotation(par, RegulatoryOntologyHelper.PROP_RULE_TITLE, name);
		goh.createAnnotation(par, RegulatoryOntologyHelper.PROP_PARAGRAPH_NUMBER, number);
		goh.createSimpleRelationship(law, par, RegulatoryOntologyHelper.REL_LAW_PARAGRAPHS);
		return par;
	}
	/**
     * Creates a Paragraph individual.
     * @param law
     * @param number
     * @param name
     * @return
     */
	public OWLNamedIndividual createParagraph(final OWL2AbstractElement law, final String number, final String name) {
	    return createParagraph(getIndividualFromOWL2AbstractElement(law), number, name);
	}
	
	/**
	 * Creates a Section individual. 
	 * @param par
	 * @param number
	 * @param content
	 * @return
	 */
	public OWLNamedIndividual createSection(OWLNamedIndividual par, String number, String content) {
		OWLNamedIndividual sec = goh.createOWLIndividual(par.getIRI().getFragment()+"-"+number, RegulatoryOntologyHelper.CLS_SECTION);
		goh.createAnnotation(sec, RegulatoryOntologyHelper.PROP_RULE_TITLE, par.getIRI().getFragment()+"-"+number);
		goh.createAnnotation(sec, RegulatoryOntologyHelper.PROP_SECTION_CONTENT, content);
		goh.createSimpleRelationship(par, sec, RegulatoryOntologyHelper.REL_PARAGRAPH_SECTIONS);
		return sec;
	}
	
	/**
     * Creates a Section individual. 
     * @param par
     * @param number
     * @param content
     * @return
     */
	public OWLNamedIndividual createSection(final OWL2AbstractElement paragraph, 
	                                        final String number, final String content) {
	    return createSection(getIndividualFromOWL2AbstractElement(paragraph), number, content);
	}
	
	// BSI
	/**
	 * Creates a BSI-Element individual.
	 * @param id
	 * @param name
	 * @param text
	 * @return
	 */
	public OWLNamedIndividual createBSIElement(String id, String name, String text) {
		OWLNamedIndividual bsi = goh.createOWLIndividual(id, RegulatoryOntologyHelper.CLS_BSIELEMENT);
		goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_RULE_TITLE, name);
		goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT, text);
		return bsi;
	}

	/**
	 * Creates a BSI-Threat individual.
	 * @param id
	 * @param name
	 * @param text
	 * @return
	 */
	public OWLNamedIndividual createBSIThreat(String id, String name, String text) {
		OWLNamedIndividual bsi = goh.createOWLIndividual(id, RegulatoryOntologyHelper.CLS_BSITHREAT);
		goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_RULE_TITLE, name);
		goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT, text);
		return bsi;
	}

	/**
	 * Creates a BSI-Measure individual.
	 * @param id
	 * @param name
	 * @param text
	 * @return
	 */
	public OWLNamedIndividual createBSIMeasure(String id, String name, String text) {
		OWLNamedIndividual bsi = goh.createOWLIndividual(id, RegulatoryOntologyHelper.CLS_BSIMEASURE);
		goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_RULE_TITLE, name);
		goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT, text);
		return bsi;
	}
	
	// MARiskVA
	/**
	 * Creates a Marisk-Rule.
	 * @param id
	 * @param name
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public OWLNamedIndividual createMARiskRule(String id, String name, String text) throws Exception {
//		OWLNamedIndividual bsi = goh.createOWLIndividual(id, RegulatoryOntologyHelper.CLS_BSIMEASURE);
//		goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_RULE_TITLE, name);
//		goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT, text);
		throw new Exception("Von MARiskRule selbst keine Individuel anlegen!");
//		return null;
	}
	
	/**
	 * Creates a Marisk-Clause.
	 * @param id
	 * @param number
	 * @param name
	 * @return
	 */
	public OWLNamedIndividual createMARiskClause(String id, String number, String name) {
		OWLNamedIndividual newInd = goh.createOWLIndividual(id, RegulatoryOntologyHelper.CLS_MARISKCLAUSE);
		goh.createAnnotation(newInd, RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NAME, name);
		goh.createAnnotation(newInd, RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NUMBER, number);
		return newInd;
	}

	/**
	 * Deprecated. Doesn't create anything.
	 * @param id
	 * @param number
	 * @param text
	 * @return
	 * @throws Exception
	 */
	public OWLNamedIndividual createMARiskEntry(String id, String number, String text) throws Exception {
//		OWLNamedIndividual newInd = goh.createOWLIndividual(id, RegulatoryOntologyHelper.CLS_MARISKENTRY);
//		goh.createAnnotation(newInd, RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER, number);
//		goh.createAnnotation(newInd, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, text);
//		return newInd;
		throw new Exception("Von MARiskEntry selbst keine Individuel anlegen!");
	}


	/**
	 * Creates a Marisk-Binding
	 * @param id
	 * @param number
	 * @param text
	 * @return
	 */
	public OWLNamedIndividual createMARiskBindingEntry(String id, String number, String text) {
		OWLNamedIndividual newInd = goh.createOWLIndividual(id, RegulatoryOntologyHelper.CLS_MARISKBINDING);
		goh.createAnnotation(newInd, RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER, number);
		goh.createAnnotation(newInd, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, text);
		return newInd;
	}

	/**
	 * Creates a Marisk-Comment.
	 * @param id
	 * @param number
	 * @param text
	 * @return
	 */
	public OWLNamedIndividual createMARiskCommentEntry(String id, String number, String text) {
		OWLNamedIndividual newInd = goh.createOWLIndividual(id, RegulatoryOntologyHelper.CLS_MARISKCOMMENT);
		goh.createAnnotation(newInd, RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER, number);
		goh.createAnnotation(newInd, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, text);
		return newInd;
	}
	
        // GoBS
	public OWLNamedIndividual createGuidelinesEntry(final String id, final String name,
	        final String title, final String text, final String keywords) {
        OWLNamedIndividual bsi = goh.createOWLIndividual(id, RegulatoryOntologyHelper.CLS_GUIDELINESENTRY);
        goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_GUIDELINESENTRY_TITLE, title);
        goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_GUIDELINESENTRY_TEXT, text);
        goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_RULE_KEYWORDS, keywords);
        goh.createAnnotation(bsi, RegulatoryOntologyHelper.PROP_GUIDELINESENTRY_NAME, name);
        return bsi;
    }
	
	/**
	 * returns a list of situations
	 * @return situations
	 */
	public Set<OWLNamedIndividual> getSituations() {
	    return goh.getIndividuals(RegulatoryOntologyHelper.CLS_SITUATION, true);
	}

   
    /**
     * Creates a new constraint individual with a given id.
     * @param constraintName String used as the id.
     * @param constraintClassname String used as the classname
     * @param parameters a list of individuals used as parameters
     * @return returns the Constraint with the given id.
     */
    public OWLNamedIndividual createConstraintIndividual(final String constraintName, final String constraintClassname, List<OWLNamedIndividual> parameters) {
        OWLNamedIndividual constraintIndividual = goh.createOWLIndividual(IndividualIDGenerator.
                generateConstraintID(constraintName, constraintClassname), constraintClassname);
        int nr = 1;
        for (OWLNamedIndividual ruleElementParameter : parameters) {
            Annotation parameterIndex = new Annotation(PROP_REL_CONSTRAINT_HASPARAMETERS_INDEX, nr);
            goh.createSimpleRelationship(constraintIndividual, ruleElementParameter, REL_CONSTRAINT_HASPARAMETERS, Collections.singleton(parameterIndex));
            nr++;
        }
        return constraintIndividual;
    }
    /**
     * Creates a new constraint individual with a given id.
     * @param constraintName String used as the id.
     * @param constraintClassname String used as the classname
     * @param parameters a list of individuals used as parameters
     * @return returns the Constraint with the given id.
     */
    public OWLNamedIndividual createConstraintIndividual(final String constraintName, final String constraintClassname, final Set<OWL2AbstractElement> parameters) {
        Map<OWL2AbstractElement, OWLNamedIndividual> individualMap = getIndividualsFromOWL2AbstractElements(parameters);
        return createConstraintIndividual(constraintName, constraintClassname, (List<OWLNamedIndividual>) individualMap.values());
    }
 
    
    /**
     * Creates a subclass of a general constraint class. The new constraint class receives the name and the specification
     * for the signature of individuals of this constraint class.
     * (e.g. for a separation-of-duty constraint, the specification would be a list of two strings containing "Activity".) 
     * @param name
     * @param specification - parameter specification for the constraint class.
     * @return - the created class entity.
     */
    public OWLClass createConstraintClass(final String name, final List<String> specification) {
        OWLDeclarationAxiom isSomeClass = goh.declareClass(convertToValidOwlId(name));
        goh.createSubclassOfAxiom(name, CLS_CONSTRAINT);
        goh.createAnnotation(isSomeClass.getEntity(), PROP_CONSTRAINT_SPECIFICATION, specification.toString().replaceAll("[\\[\\]]",""));
        return isSomeClass.getEntity().asOWLClass();
    }
    
    /** 
     * The target, the Constraint, is added to the Situation as ObjectProperty 'checkedBy'. 
     * @param src
     * @param target
     */
    public void assignConstraintToSituation(OWLNamedIndividual src, OWLNamedIndividual target) {
        goh.createSimpleRelationship(src, target, REL_SITUATION_CHECKED_BY);
    }
    
	/**
     * returns a single annotation value, specified by the annotationType. If there is no matching 
     * annotation found, an empty string is given back.
     * @param individual the individual
     * @param annotationType the type (e.g. Number or Text)
     * @return the value
	 * @throws NoSuchPropertyException 
     */    
    public String getAnnotationFromIndividual(final OWLNamedIndividual individual, final String annotationPropertyName) {
        OWLLiteral literal = null;
        try {
            literal = goh.getAnnotation(individual, annotationPropertyName);
        } catch (NoSuchPropertyException e) {
            return "";
        }
        return literal.getLiteral();
    }
    
    /**
     * returns a single annotation value, specified by the annotationType. If the is no matching 
     * annotation found, an empty string is given back.
     * @param individual the individual
     * @param annotationType the type (e.g. Number or Text)
     * @return the value
     */
   public String getAnnotationFromIndividual_th(final OWLNamedIndividual individual, final String annotationType) {
        try {
            return this.goh.getStringAnnotation(individual, annotationType);
        } catch (NoSuchPropertyException e) {
            return "";
        }        
    }
    
    
    /**
     * returns a list of all rule elements referencing the desired individual.
     * the list will be sorted by the value of the start index.
     * @param ruleName the individual
     * @return list of rule elements
     */
    public final List<String> getIndicesOfRE(final String ruleName) {
        List<String> indices = new ArrayList<String>();
        String start = ""; String end = ""; String type = "";
        OWLNamedIndividual rule = goh.getIndividualById(ruleName);
        if (rule != null) {
            Set<OWLAxiom> axioms = rule.getReferencingAxioms(getOntology());
                    // searching for all rule Elements
            for (OWLAxiom axiom : axioms) {
                if (axiom.toString().contains(REL_RULE_RULEELEMENTS)) {
                    for (OWLAnnotation annotation : axiom.getAnnotations()) {
                        try {
                            if (annotation.getProperty().getIRI().toString().contains(PROP_REL_RULE_RULEELEMENTS_STARTINDEX)) {
                                start = goh.owlAnnotationValueToString(annotation.getValue());
                            } else if (annotation.getProperty().getIRI().toString().contains(PROP_REL_RULE_RULEELEMENTS_ENDINDEX)) {
                                end = goh.owlAnnotationValueToString(annotation.getValue());
                            } 
                        } catch (IndexOutOfBoundsException i) {
                            System.err.println(annotation.getValue() + "\n" + i.getLocalizedMessage());
                        }
                    }
                    for (OWLEntity entity : axiom.getSignature()) {
                        if (entity.getEntityType().getName().equals("NamedIndividual")) {
                            if (entity.toStringID().contains("Artifact")) {
                                type = "Artifact";
                            } else if (entity.toStringID().contains("Activity")) {
                                type = "Activity";
                            } else if (entity.toStringID().contains("Process")) {
                                type = "Process";
                            } else if (entity.toStringID().contains("Property")) {
                                type = "Property";
                            } else if (entity.toStringID().contains("Role")) {
                                type = "Role";
                            }
                        }
                    }
                    indices.add(start + "_" + end + "&" + type);
                }
            
              
            }
            try {
            Collections.sort(indices, new Comparator<String>() {
                    // to compare the Strings on the integer value
                @Override
                public int compare(final String o1, final String o2) {
                    if (o1.substring(0, o1.indexOf("_")).equals(o2.substring(0, o2.indexOf("_")))) {
                        return 0;
                    } else if (Integer.valueOf(o1.substring(0, o1.indexOf("_"))) < Integer.valueOf(o2.substring(0, o2.indexOf("_")))) {
                        return -1;
                    } else {
                        return 1;
                    }
                }
            });
            } catch (NumberFormatException n) {
                System.err.println("Failed to sort the list!");
                n.printStackTrace();
            }
            System.out.println("Indices for " + ruleName + " " + Arrays.toString(indices.toArray()));
        }
        return indices;
    }
    
    /**
     * collects all sub elements which belong to a given Rule Element. The result will be returned in a list.
     * @param ruleElement the Rule Element
     * @return list of sub elements
     */
    public List<String> getAllSubElementsOfRE(final String ruleElement) {
       String name = "";
       Set<OWLNamedIndividual> ruleElements = goh.getIndividuals(ruleElement, true);
       List<String> subElements = new ArrayList<String>();
           // the list should only return elements when the rule element exists
       if (ruleElement.equals(CLS_PROCESS) || ruleElement.equals(CLS_ROLE) || ruleElement.equals(CLS_PROPERTY)
           || ruleElement.equals(CLS_ACTIVITY) || ruleElement.equals(CLS_ARTIFACT)) {
           subElements.add(ruleElement);
           for (OWLNamedIndividual individual : ruleElements) {
               name = individual.getIRI().getFragment();
               if (!subElements.contains(name)) {
                   subElements.add(name);
               }
           }
       }
       return subElements;
    }
    
    /**
     * returns a list with the related rule elements from a given rule.
     * @param ruleElementRelation the relation type which was requested
     * @param rule the rule id
     * @return a list
     */
    public List<OWLNamedIndividual> getSpecificSubElementsOfRE(final String ruleElementClass, final String rule) {
        List<OWLNamedIndividual> subElements = new ArrayList<OWLNamedIndividual>();
        OWLNamedIndividual ruleIndividual = goh.getIndividualById(rule);
        Set<OWLNamedIndividual> relatedRuleElements = goh.getRelatedIndividuals(ruleIndividual, REL_RULE_RULEELEMENTS);
        if (relatedRuleElements != null) {
            for (OWLNamedIndividual individual : relatedRuleElements) {
               if (ruleElementClass.equals(goh.getTypeOfIndividual(individual).getFragment())) {
                   subElements.add(individual);
               }
            }
        }
        return subElements;
    }
	
	
	/**
	 * Assigns the given threat to the given BSI element.
	 * @param bsiElement
	 * @param bsiThreat
	 */
	public void assignThreat(OWLNamedIndividual bsiElement, OWLNamedIndividual bsiThreat) {
		goh.createSimpleRelationship(bsiElement, bsiThreat, RegulatoryOntologyHelper.REL_BSIELEMENT_THREATS);
	}

	/**
	 * Assigns the given measure to the given BSI element.
	 * @param bsiElement
	 * @param bsiMeasure
	 */
	public void assignMeasure(OWLNamedIndividual bsiElement, OWLNamedIndividual bsiMeasure) {
		goh.createSimpleRelationship(bsiElement, bsiMeasure, RegulatoryOntologyHelper.REL_BSIELEMENT_MEASURES);
	}
	
	/**
	 * Creates a reference from one rule (source) to another (target).
	 * @param source
	 * @param target
	 */
	public void createRuleReference(OWLNamedIndividual source, OWLNamedIndividual target) {
		goh.createSimpleRelationship(source, target, RegulatoryOntologyHelper.REL_RULE_REFFEREDRULES);
	}
	
	/**
     * Creates a reference from one rule (source) to another (target).
     * @param source
     * @param target
     */
	public void createRuleReference(final OWL2AbstractElement source, final OWL2AbstractElement target) {
	    createRuleReference(getIndividualFromOWL2AbstractElement(source), getIndividualFromOWL2AbstractElement(target));
	}
    
	// MARisk
	/**
	 * Creates a reference from a marisk-clause (source) to a marisk-sub-clause (target).
	 * @param source
	 * @param target
	 */
	public void createMARiskSubClause(OWLNamedIndividual source, OWLNamedIndividual target) {
		goh.createSimpleRelationship(source, target, REL_MARISKCLAUSE_HASSUBCLAUSES);
	}
	/**
     * Creates a reference from a marisk-clause (source) to a marisk-sub-clause (target).
     * @param source
     * @param target
     */
	public void createMARiskSubClause(final OWL2AbstractElement source, final OWL2AbstractElement target) {
	    createMARiskSubClause(getIndividualFromOWL2AbstractElement(source), getIndividualFromOWL2AbstractElement(target));
	}
	
	/**
	 * Creates a reference from a marisk-clause (source) to a marisk-entry (target).
	 * @param source
	 * @param target
	 */
	public void createMARiskEntry(OWLNamedIndividual source, OWLNamedIndividual target) {
		goh.createSimpleRelationship(source, target, REL_MARISKCLAUSE_HASENTRIES);
	}
	/**
     * Creates a reference from a marisk-clause (source) to a marisk-entry (target).
     * @param source
     * @param target
     */
	public void createMARiskEntry(final OWL2AbstractElement source, final OWL2AbstractElement target) {
	    createMARiskEntry(getIndividualFromOWL2AbstractElement(source), getIndividualFromOWL2AbstractElement(target));
	}
	
	/**
	 * Creates a reference from a marisk-binding (source) to a marisk-comment (target).
	 * @param source
	 * @param target
	 */
	public void createMARiskBinding(OWLNamedIndividual source, OWLNamedIndividual target) {
		goh.createSimpleRelationship(source, target, REL_MARISKBINDING_HASCOMMENTS);
	}
	/**
     * Creates a reference from a marisk-binding (source) to a marisk-comment (target).
     * @param source
     * @param target
     */
	public void createMARiskBinding(final OWL2AbstractElement source, final OWL2AbstractElement target) {
	    createMARiskBinding(getIndividualFromOWL2AbstractElement(source), getIndividualFromOWL2AbstractElement(target));
	}
	
	/**
	 * converts a given string to a valid OWLId (whitespace will be replaced by '_').
	 * @param input
	 * @return
	 */
	public static String convertToValidOwlId(String input){
		return input.replace(' ', '_');
	}
	
	  
	/**
	 * returns a list of referred individuals from the given one
	 * @param individual the individual to search the references for
	 * @return list with individuals
	 */
	public List<OWLNamedIndividual> getReferredRules(OWLNamedIndividual individual) {
	    String[] relations = {  RegulatoryOntologyHelper.REL_RULE_REFFEREDRULES, RegulatoryOntologyHelper.REL_BSIELEMENT_MEASURES, 
	                            RegulatoryOntologyHelper.REL_BSIELEMENT_THREATS, RegulatoryOntologyHelper.REL_LAW_PARAGRAPHS, 
	                            RegulatoryOntologyHelper.REL_MARISKBINDING_HASCOMMENTS, RegulatoryOntologyHelper.REL_MARISKCLAUSE_HASENTRIES, 
	                            RegulatoryOntologyHelper.REL_MARISKCLAUSE_HASSUBCLAUSES, RegulatoryOntologyHelper.REL_PARAGRAPH_SECTIONS};
	    List<OWLNamedIndividual> referredRules = new ArrayList<OWLNamedIndividual>();
	    Set<OWLNamedIndividual> individuals = null;
	    for (int i = 0; i < relations.length; i++) {
	        individuals = goh.getRelatedIndividuals(individual, relations[i]);
	        if (individuals != null) {
	            referredRules.addAll(individuals);
	        }
	    }
	    return referredRules;
	}
	
	/**
	 * returns a set of all inverse related situations from a given rule.
	 * @param rule the rule
	 * @return the situations
	 */
	public Set<OWLNamedIndividual> getSituationsFromRule(final OWLNamedIndividual rule) {
	    return goh.getInverseRelatedIndividuals(rule, RegulatoryOntologyHelper.REL_SITUATION_INVOLVEDRULES);
	}
	
	/**
	 * retrieves the OWLNamedIndividual from a given OWL2AbstractElement.
	 * @param owl2AbstractElement
	 * @return
	 */
	public final OWLNamedIndividual getIndividualFromOWL2AbstractElement(final OWL2AbstractElement owl2AbstractElement) {
	    return owl2AbstractElement.getOWLNamedIndividual();
	}
	
	/**
	 * retrieves a map with the given OWL2AbstractElements as key and the related OWLNamedIndividuals as value.
	 * @param owl2AbstractElements
	 * @return
	 */
	public final Map<OWL2AbstractElement, OWLNamedIndividual> getIndividualsFromOWL2AbstractElements(
	                                                                final Set<OWL2AbstractElement> owl2AbstractElements) {
	    Map<OWL2AbstractElement, OWLNamedIndividual> individuals = new HashMap<OWL2AbstractElement, OWLNamedIndividual>();
	    for (OWL2AbstractElement abstractElement : owl2AbstractElements) {
	        individuals.put(abstractElement, abstractElement.getOWLNamedIndividual());
	    }
	    return individuals;
	}
	
	/**
	 * retrieves the OwlAxiom from a given Owl2AbstractAxiom.
	 * @param abstractAxiom
	 * @return
	 */
	public final OWLObjectPropertyAssertionAxiom getAxiomFromOWL2AbstractAxiom(final OWL2AbstractAxiom abstractAxiom){
	    return abstractAxiom.getOwlObjectPropertyAssertionAxiom();
	}
	
	/**
	 * retrieves a map with the Owl2AbstractAxioms as key and the matching OwlPropertyAssertionAxioms as value.
	 * @param owl2AbstractAxioms
	 * @return
	 */
	public final Map<OWL2AbstractAxiom, OWLObjectPropertyAssertionAxiom> getAxiomsFromOWL2AbstractAxioms(
	                                                                        final Set<OWL2AbstractAxiom> owl2AbstractAxioms) {
	    Map<OWL2AbstractAxiom, OWLObjectPropertyAssertionAxiom> axioms = new HashMap<OWL2AbstractAxiom, OWLObjectPropertyAssertionAxiom>();
	    for (OWL2AbstractAxiom abstractAxiom : owl2AbstractAxioms) {
	        axioms.put(abstractAxiom, abstractAxiom.getOwlObjectPropertyAssertionAxiom());
	    }
	    return axioms;
	}

    /**
     * checks if the given string s contains a correct relation between two rule elements.
     * s is expected as "Activity_.....&Property_....." for example.
     * @param s
     * @return
     */
    public boolean isRelationBetweenRuleElementsValid(final String s) {
        String[] checkValidity = s.split("&");
        boolean isValid = false;
        if (relatedRuleElements.get(checkValidity[0].split("_")[0] + "," + checkValidity[1].split("_")[0]) != null) {
            isValid = true;
        }
        return isValid;
    }

    /**
     * returns the related and inverse related ruleelements to a given ruleelement.
     * @param ruleElement
     * @return
     */
    public List<OWLNamedIndividual> getRelatedRuleElements(
            OWLNamedIndividual ruleElement) {
        List<OWLNamedIndividual> ruleElements = new ArrayList<OWLNamedIndividual>();
        Set<OWLNamedIndividual> relatedIndividuals = goh.getRelatedIndividuals(
                ruleElement, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
        Set<OWLNamedIndividual> inverseRelatedIndividuals = goh.getInverseRelatedIndividuals(
                ruleElement, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
       
        if (relatedIndividuals != null) {
            ruleElements.addAll(relatedIndividuals);
        }
        if (inverseRelatedIndividuals != null) {
            ruleElements.addAll(inverseRelatedIndividuals);
        } 
        return ruleElements;
    }
    
    // Part of Babak Mansours BA
    
    /**
     * returns a list of {@link RuleElementRelationModel} referencing the desired rule
     * element.
     * 
     * @param ruleElementName
     *            the individual
     * @return list of rule elements
     */
    public final List<RuleElementRelationModel> getIndividualRelationsOfRE(
            final String ruleElementName, final String ruleElementType) {

        List<RuleElementRelationModel> relations = new ArrayList<RuleElementRelationModel>();

        String relationType = "";
        String description = "";
        String type = "";
        String name = "";
        String ruleElementId = IndividualIDGenerator.generateRuleElementID(
                ruleElementName, ruleElementType);
        OWLNamedIndividual ruleElement = goh.getIndividualByIdAndClass(
                                        ruleElementId, ruleElementType, true);
        System.out.println("getIndividualRelationOfRE (ruleElement: "
                                        + ruleElement + ")");

        if (ruleElement != null) {
            Set<OWLAxiom> axioms = ruleElement
                    .getReferencingAxioms(getOntology());
            // searching for all rule Elements
            for (OWLAxiom axiom : axioms) {
                if (axiom.toString().contains(REL_RULEELEMENTS_RULEELEMENTS)) {
                    for (OWLAnnotation annotation : axiom.getAnnotations()) {
                        try {
                            if (annotation.getProperty().getIRI().toString().contains(
                                            PROP_REL_RULEELEMENTS_RULEELEMENTS_TYPE)) {
                                relationType = goh.owlAnnotationValueToString(annotation.getValue());
                            } else if (annotation.getProperty().getIRI().toString().contains(
                                            PROP_REL_RULEELEMENTS_RULEELEMENTS_DESCRIPTION)) {
                                description = goh.owlAnnotationValueToString(annotation.getValue());
                            }
                        } catch (IndexOutOfBoundsException i) {
                            System.err.println(annotation.getValue() + "\n"
                                    + i.getLocalizedMessage());
                        }
                    }
                    OWLEntity entity = (OWLEntity) ((OWLObjectPropertyAssertionAxiom) axiom).getObject();
                    // for (OWLEntity entity : axiom.getSignature()) {
                    if (!entity.equals(ruleElement)) {
                        if (entity.getEntityType().getName()
                                .equals("NamedIndividual")) {
                            if (entity.toStringID().contains("Artifact")) {
                                System.out.println(entity.toStringID());
                                type = "Artifact";
                                name = entity.toStringID().substring(68)
                                        .replaceAll("_", " ");
                            } else if (entity.toStringID().contains("Activity")) {
                                System.out.println(entity.toStringID());
                                type = "Activity";
                                name = entity.toStringID().substring(68)
                                        .replaceAll("_", " ");
                            } else if (entity.toStringID().contains("Process")) {
                                System.out.println(entity.toStringID());
                                type = "Process";
                                name = entity.toStringID().substring(67)
                                        .replaceAll("_", " ");
                            } else if (entity.toStringID().contains("Property")) {
                                System.out.println(entity.toStringID());
                                type = "Property";
                                name = entity.toStringID().substring(68)
                                        .replaceAll("_", " ");
                            } else if (entity.toStringID().contains("Role")) {
                                System.out.println(entity.toStringID());
                                type = "Role";
                                name = entity.toStringID().substring(64)
                                        .replaceAll("_", " ");
                            }
                        }
                        // }
                    }

                    if (type != "" && name != "") {
                        relations.add(new RuleElementRelationModel(
                                relationType, description, ruleElementType, ruleElementName));
                    }
                }
          
            }
            System.out.println("Relations for " + ruleElementId + " "
                    + Arrays.toString(relations.toArray()));
        }
        return relations;
    }
    
    /**
     * returns a list of lists of all rule elements referencing the desired
     * individual. the list will be sorted by the value of the start index.
     * 
     * @param ruleName
     *            the individual
     * @return list of rule elements
     */
    public final List<RuleElementModel> getIndividualIndicesOfRE(
            final String ruleName, final String clazz) {
        
        List<RuleElementModel> ruleElements = new ArrayList<RuleElementModel>();
        int start = 0; int end = 0;
        String type = ""; String representation = ""; String name = "";
        OWLNamedIndividual rule = null;
        if (clazz.equals("MariskBinding")) { // notation in the Interfaces is different from the notation in the ROH
            rule = goh.getIndividualByIdAndClass(ruleName, RegulatoryOntologyHelper.CLS_MARISKBINDING, true);
        } else if (clazz.equals("MariskComment")) {
            rule = goh.getIndividualByIdAndClass(ruleName, RegulatoryOntologyHelper.CLS_MARISKCOMMENT, true);
        } else {
            rule = goh.getIndividualByIdAndClass(ruleName, clazz, true);
        }
        if (rule != null) {
            Set<OWLAxiom> axioms = rule.getReferencingAxioms(getOntology());
            // searching for all rule Elements
            for (OWLAxiom axiom : axioms) {                
                if (axiom.toString().contains(REL_RULE_RULEELEMENTS)) {
                    for (OWLAnnotation annotation : axiom.getAnnotations()) {
                        try {
                            if (annotation.getProperty().getIRI().toString().contains(
                                            PROP_REL_RULE_RULEELEMENTS_STARTINDEX)) {
                                start = Integer.valueOf(
                                        goh.owlAnnotationValueToString(annotation.getValue()));
                            }
                            if (annotation.getProperty().getIRI().toString().contains(
                                            PROP_REL_RULE_RULEELEMENTS_ENDINDEX)) {
                                end = Integer.valueOf(
                                        goh.owlAnnotationValueToString(annotation.getValue()));

                            } else if (annotation.getProperty().getIRI().toString().contains(
                                            PROP_REL_RULE_RULEELEMENTS_REPRESENTATION)) {
                                representation = goh.owlAnnotationValueToString(annotation
                                                .getValue());
                            }
                        } catch (IndexOutOfBoundsException i) {
                            System.err.println(annotation.getValue() + "\n"
                                    + i.getLocalizedMessage());
                        }
                    }
                    for (OWLEntity entity : axiom.getSignature()) {
                        if (entity.getEntityType().getName().equals("NamedIndividual")) {
                            String id = entity.toStringID();
                            if (id.contains("Artifact")) {
                                type = "Artifact";
                                name = id.substring(id.lastIndexOf("/") + 1).replaceAll("_", " ");
                            } else if (id.contains("Activity")) {
                                type = "Activity";
                                name = id.substring(id.lastIndexOf("/") + 1).replaceAll("_", " ");
                            } else if (id.contains("Process")) {
                                type = "Process";
                                name = id.substring(id.lastIndexOf("/") + 1).replaceAll("_", " ");
                            } else if (id.contains("Property")) {
                                type = "Property";
                                name = id.substring(id.lastIndexOf("/") + 1).replaceAll("_", " ");
                            } else if (id.contains("Role")) {
                                type = "Role";
                                name = id.substring(id.lastIndexOf("/") + 1).replaceAll("_", " ");
                            }
                        }
                    }
                    ruleElements.add(new RuleElementModel(start, end, representation, name, type));
                } else {
//                    System.out.println("Axiom does not contain any ruleelements: " + axiom.toString());   // debug
                }
            }
        } else {
//            System.out.println("No matching rule found for: " + ruleName + ", " + clazz);     // debug
        }
        return ruleElements;
    }

    /**
     * returns the relationType between the ruleelements.
     * @param firstREType
     * @param secondREType
     * @return
     * @throws WrongOrderException 
     */
    public final String getRuleElementRelationType(final String firstREType,
            final String secondREType) throws WrongOrderException {
        String relationType = "";
        String[] result = relatedRuleElements.get(firstREType + "," + secondREType);
        if (result != null && result.length > 1) {
            if (result[1].equals("true")) {
                relationType = result[0];       // this is the relation between the ruleelements
            } else {
                throw new WrongOrderException("The ruleElements have not been in the right order.");
            }
        }
        return relationType;
    }
    
    // keywords erstmal als einfacher String, evtl. spaeter als collection
    public void setKeywords(OWLNamedIndividual ind, String keywords) {
        goh.createAnnotation(ind, RegulatoryOntologyHelper.PROP_RULE_KEYWORDS, keywords);
    }
    
    public String getKeywords(OWLNamedIndividual ind) {
        return this.getAnnotationFromIndividual(ind, RegulatoryOntologyHelper.PROP_RULE_KEYWORDS);        
    }
    
    public KeywordSet getKeywordsAsSet(OWLNamedIndividual ind) {
        String keywords = this.getKeywords(ind);
        return new KeywordSet(keywords);
    }
    
    public KeywordCollection getAllKeywords(){
        KeywordCollection ret = new KeywordCollection();
        Set<OWLNamedIndividual> individuals = this.getGoh().getIndividuals(RegulatoryOntologyHelper.CLS_RULE, false);
        for (OWLNamedIndividual curInd : individuals) { // Every iteration creates those properties originating from curInd
            ret.put(curInd.getIRI().toString(), new KeywordSet(this.getKeywordsAsSet(curInd)));
        }
        return ret;
    }
    
}
