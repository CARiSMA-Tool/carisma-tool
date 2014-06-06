package carisma.xutils.regulatory.ui.controller;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TreeItem;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Ontology;
import carisma.regulatory.ontology.Rule;
import carisma.regulatory.ontology.RuleElement;
import carisma.regulatory.ontology.Situation;
import carisma.regulatory.ontology.owl.OWL2Ontology;
import carisma.regulatory.ontology.utils.IndividualIDGenerator;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.regulatory.ontology.utils.WrongOrderException;
import carisma.xutils.regulatory.ui.comparator.NaturalOrderComparatorRule;
import carisma.xutils.regulatory.ui.first.ConfigurationView;
import carisma.xutils.regulatory.ui.first.RegulationsView;
import carisma.xutils.regulatory.ui.model.Constants;
import carisma.xutils.regulatory.ui.model.ConstraintModel;
import carisma.xutils.regulatory.ui.model.CreateConstraintModel;
import carisma.xutils.regulatory.ui.model.CreationException;
import carisma.xutils.regulatory.ui.model.RULES;
import carisma.regulatory.ontology.model.*;

/**
 * Controller for communication between Ontology and GUI.
 *
 * @author bm
 */
public class MainController {

    /** The onto. */
    private static Ontology onto;
    
    /** The roh. */
    private RegulatoryOntologyHelper roh;

    // lists of Ontology-items
    /** The rules. */
    private static List<Rule> rules;
    
    /** The list situations. */
    private static ArrayList<Situation> listSituations;
    
    /** The list rule elements. */
    private static ArrayList<RuleElement> listRuleElements;
    private static MainController instance = null;

    /**
     * creates a new Ontology-controller.
     */
    private MainController() {
    	
    }
    
    public static MainController getInstance() {
    	if (instance == null) {
    		instance = new MainController();
    	}
    	return instance;
    }

    /**
     * loads Ontology from given owl-file.
     *
     * @param fileName the file name
     */
    public void loadOntology(final String fileName) {
    	if (fileName == null) {
//    		TODO Klaus R.: return null, methode von void auf boolean aendern
    	}
    	File file = new File(fileName);
    	if (!file.exists()) {
//    		TODO Klaus R.: return null, methode von void auf boolean aendern
    	}
        // deletes existing rules in the tree
        if (RegulationsView.getTree() != null) {
            for (TreeItem item : RegulationsView.getTree().getItems()) {
                item.dispose();
            }
        }

        // deletes existing String in the textBox
        if (RegulationsView.getText() != null) {
            RegulationsView.getText().setText("");
        }
        onto = OWL2Ontology.loadFromFile(file);

        roh = new RegulatoryOntologyHelper(
                ((OWL2Ontology) onto).getGenericOntologyHelper());

        // adds RuleElements to new ArrayList

        listRuleElements = new ArrayList<RuleElement>();

        for (RuleElement re : onto.getRuleElements()) {

            listRuleElements.add(re);
        }
        // System.out.println("Regelelemente in liste kopiert. Länge: "
//                + listRuleElements.size());

        // adds Situations to new ArrayList

        listSituations = new ArrayList<Situation>();

        for (Situation s : onto.getSituations()) {

            listSituations.add(s);
            // System.out.println("Sachverhaltname: " + s.getName());
        }
        // System.out.println("Sachverhalte in liste kopiert. Länge: "
//                + listSituations.size());

        // adds Rules to new ArrayList

        rules = new ArrayList<Rule>(onto.getRules());


        // sorts Law-elements
        Collections.sort(rules, new NaturalOrderComparatorRule());

        // return laws;
        RegulationsView.setRuleList(rules);
        ConfigurationView.getRuleElements();

    }

    /**
     * gets the initialized RegulatoryOntologyHelper.
     *
     * @return RegulatoryOntologyHelper
     */
    public RegulatoryOntologyHelper getROH() {
        return roh;
    }

    /**
     * gets list of RuleElements.
     *
     * @return ArrayList<RuleElement>
     */
    public List<OWLNamedIndividual> getRuleElements(final String ruleName) {
    	List<OWLNamedIndividual> ruleElements = new  ArrayList<OWLNamedIndividual>();
    	for (int i = 0; i < Constants.RULEELEMENTS.length; i++) {
    		ruleElements.addAll(getSubElementsOfRe(Constants.RULEELEMENTS[i], ruleName));
    	}
    	ruleElements.get(0);
    	return ruleElements;
    }
  
	public final Collection<? extends OWLNamedIndividual> getSubElementsOfRe(final String ruleElementClass, final String ruleName) {
	    return roh.getSpecificSubElementsOfRE(ruleElementClass, ruleName);	
	}

    /**
     * gets the ruleElement with the matching name.
     *
     * @param name the name
     * @return OWL2AbstractElement
     */
    public OWLNamedIndividual getRuleElement(final String name, final String clazz) {
    	return roh.getGoh().getIndividualByIdAndClass(
    			IndividualIDGenerator.generateRuleElementID(name, clazz), clazz, true);
    }

    /**
     * updates RuleElement list.
     */
    public void updateRuleElements() {

        listRuleElements.clear();

        listRuleElements = (ArrayList<RuleElement>) onto.getRuleElements();

        // System.out.println("Regelelemente in liste kopiert. Länge: "
//                + listRuleElements.size());
    }
    
    public OWLNamedIndividual getIndividual(final String id, final String clazz) {
    	return roh.getGoh().getIndividualByIdAndClass(id, clazz, true);
    }

    /**
     * creates new ruleElement in the Ontology.
     *
     * @param ruleElementType the rule element type
     * @param ruleElementName the rule element name
     */
    public void createNewRuleElement(String ruleElementType,
            String ruleElementName) {

        if (ruleElementType.equals("Role")) {
            roh.createRERole(ruleElementName);
        }
        if (ruleElementType.equals("Activity")) {
            roh.createREActivity(ruleElementName);
        }
        if (ruleElementType.equals("Property")) {
            roh.createREProperty(ruleElementName);
        }
        if (ruleElementType.equals("Process")) {
            roh.createREProcess(ruleElementName);
        }
        if (ruleElementType.equals("Artifact")) {
            roh.createREArtifact(ruleElementName);
        }
        System.out.println("New RuleElement " + ruleElementName + " created.");
    }

    /**
     * creates new ruleElementInstance.
     *
     * @param list the rule
     * @param ruleElementName the rule element name
     * @param ruleElementType the rule element type
     * @param textualRepresentation the textual representation
     * @param startIndex the start index
     * @param endIndex the end index
     */
//    TODO Klaus R.: testen.
    public final void createNewRuleElementInstance(final List<OWLNamedIndividual> list,
    		final String ruleElementName, final String ruleElementType,
    		final String textualRepresentation, final int startIndex,
    		final int endIndex) {
    	for (OWLNamedIndividual rule : list) {
	        roh.createContainedRuleElementsRelation(rule, ruleElementName,
	        		ruleElementType, textualRepresentation, startIndex, endIndex);
    	}
    }


    /**
     * gets list of situations.
     *
     * @return ArrayList<Situation>
     */
    public ArrayList<Situation> getSituations() {
//    	TODO Klaus R.:  umodifiable list zurueckgeben?
        return listSituations;
    }

    /**
     * updates Situation list.
     */
    public void updateSituations() {

        listSituations.clear();

        listSituations = (ArrayList<Situation>) onto.getSituations();

        // System.out.println("Sachverhalte in liste kopiert. Länge: "
//                + listSituations.size());
    }

    /**
     * gets a list of situations for the rule.
     *
     * @param rule the rule
     * @return ArrayList<Situation>
     */
    public List<OWLNamedIndividual> loadSituationsforSpecificRule(final String rule) {
    	return roh.getSituations(rule);
    }
    
    public String getAnnotationFromIndividual(final OWLNamedIndividual individual, final String annotation) {
    	return roh.getAnnotationFromIndividual(individual, annotation);
    }
    
    public Set<OWLNamedIndividual> getRelatedIndividuals(final OWLNamedIndividual individual, final String relation) {
    	return roh.getGoh().getRelatedIndividuals(individual, relation);
    }

    /**
     * gets startIndex, endIndex, type, textual representation and name of
     * RuleElementInstances in the Section.
     *
     * @param ruleName the rule name
     * @param clazz the clazz
     * @return the rule element instances
     */
    public List<RuleElementModel> getRuleElementInstances(final String ruleName, final String clazz) {
        return roh.getIndividualIndicesOfRE(ruleName, clazz);		
    }

    /**
     * creates new relation between ruleElements in the Ontology.
     *
     * @param firstRuleElement the source rule element
     * @param secondRuleElement the target rule element
     * @param relationType the relation type
     * @param description the description
     * @throws CreationException 
     */
//    TODO Klaus R.: testen
    public void createNewRelation(OWLNamedIndividual firstRuleElement,
            OWLNamedIndividual secondRuleElement, String description) throws CreationException {
    	String relationType = "";
    	String firstREName = firstRuleElement.getIRI().getFragment();
    	String secondREName = secondRuleElement.getIRI().getFragment();
	    if (checkSelectedRuleElements(firstREName, secondREName)) {	
	    	try {
	    		relationType = roh.getRuleElementRelationType(firstREName.substring(0, firstREName.indexOf("_")),
	    				secondREName.substring(0, secondREName.indexOf("_")));
	    		roh.createRelatedRuleElementsRelation(firstRuleElement,
	                    secondRuleElement, relationType, description);	    	
	    	} catch (WrongOrderException e) {
	    		try {
					relationType = roh.getRuleElementRelationType(secondREName.substring(0, secondREName.indexOf("_")),
							firstREName.substring(0, firstREName.indexOf("_")));
		    		roh.createRelatedRuleElementsRelation(secondRuleElement,
		    				firstRuleElement, relationType, description);
				} catch (WrongOrderException e1) {
					// System.out.println("createNewRelation: " + firstREName + ", " + secondREName);
					throw new CreationException("Failed to create a new relation.");
				}
	    		
	    	}
	    }
        
    }

    /**
     * gets a list of relations for the ruleElement.
     * @param ruleElementName the name of the rule element
     * @param ruleElementType the type of the rule element
     * @return {@link List} of {@link RuleElementRelationModel}
     */
    public final List<RuleElementRelationModel> getRuleElementRelations(
            final String ruleElementName, final String ruleElementType) {
        return roh.getIndividualRelationsOfRE(ruleElementName, ruleElementType);
    }

    /**
     * saves actual Ontology to file.
     */
    public final void saveOntologyToFile() {
        roh.saveOntologyToFile(new File(
//        		TODO Klaus R.: Pfad angeben, keinen fixen Pfad nutzen, testen
                "/home/bm/workspace/carisma.regulatory/"
                + "resources/Entire_Law_Ontology2.owl"));
    }

	/**
	 * Gets the text from individual.
	 *
	 * @param ruleId the rule id
	 * @param ruleClazz the rule clazz
	 * @return the text from individual
	 */
	public String getTextFromIndividual(String ruleId, String ruleClazz) {
//		TODO Klaus R.: wenn der String ruleClazz die ENumeration Rules darstellen soll, bzw ein Wert davon,
//		warum wird dann nicht die ENumeration anstatt ein String erwartet?
//		TODO Klaus R.: keine Fehlerbehandlung
		OWLNamedIndividual individual = null;
		String response = "";
		try {
			switch (RULES.valueOf(ruleClazz)) {
			case BSIElement:
				individual = roh.getGoh().getIndividualByIdAndClass(ruleId, RegulatoryOntologyHelper.CLS_BSIELEMENT, false);				
				response = roh.getGoh().getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT);
				break;
			case BSIMeasure:
				individual = roh.getGoh().getIndividualByIdAndClass(ruleId, RegulatoryOntologyHelper.CLS_BSIMEASURE, false);
				response = roh.getGoh().getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT);
				break;
			case BSIThreat:
				individual = roh.getGoh().getIndividualByIdAndClass(ruleId, RegulatoryOntologyHelper.CLS_BSITHREAT, false);
				response = roh.getGoh().getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT);
				break;
			case Section:
				individual = roh.getGoh().getIndividualByIdAndClass(ruleId, RegulatoryOntologyHelper.CLS_SECTION, false);
				response = roh.getGoh().getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_SECTION_CONTENT);
				break;
			case MariskBinding:
				individual = roh.getGoh().getIndividualByIdAndClass(ruleId, RegulatoryOntologyHelper.CLS_MARISKBINDING, false);
				response = roh.getGoh().getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT);
				break;
			case MariskComment:
				individual = roh.getGoh().getIndividualByIdAndClass(ruleId, RegulatoryOntologyHelper.CLS_MARISKCOMMENT, false);
				response = roh.getGoh().getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT);
				break;	
			default:
				break;
			}
		} catch (NoSuchPropertyException n) {
			System.err.println("There is no such Property. " + n.getLocalizedMessage());
		}
		return response;
	}
	
//	TODO Javadoc
	public boolean checkSelectedRuleElements(final String firstRuleelement,
			final String secondRuleelement) {
		return roh.isRelationBetweenRuleElementsValid(firstRuleelement
				+ "&" + secondRuleelement);
	}
	
//	TODO Javadoc
//	TODO testen
	public boolean isDataPresent(TreeItem item) {
    	return (item.getData(Constants.RULE_CLAZZ) != null 
    			&& item.getData(Constants.RULE_NUMBER) != null);
    }

//	TODO Javadoc
	public List<String> getRuleElements() {
		List<String> ruleElements = new ArrayList<String>();
		for (int i = 0; i < Constants.RULEELEMENTS.length; i++) {
			ruleElements.addAll(roh.getAllSubElementsOfRE(Constants.RULEELEMENTS[i]));
		}
		return ruleElements;
	}

//	TODO Javadoc
	public List<ConstraintModel> getConstraints() {
		List<ConstraintModel> constraints = new ArrayList<ConstraintModel>();
		OWLClass constraintClass = roh.getOWLClass(RegulatoryOntologyHelper.CLS_CONSTRAINT);
		for (OWLClassExpression subClass : constraintClass.getSubClasses(roh.getOntology())) {
		    try {
		    	List<String> rElements = new ArrayList<String>();
				List<OWLNamedIndividual> individuals = new ArrayList<OWLNamedIndividual>();
		    	String name = subClass.asOWLClass().getIRI().getFragment();
		    	String[] ruleElements =  roh.getGoh().getAnnotation(
		    			subClass.asOWLClass(), RegulatoryOntologyHelper.
	                            PROP_CONSTRAINT_SPECIFICATION)
	                            .getLiteral().split(",");
		    	for (int i = 0; i < ruleElements.length; i++) {
		    		rElements.add(ruleElements[i].trim());
		    	}
	            for (OWLNamedIndividual individual: roh.getGoh().
	                 getIndividuals(subClass.asOWLClass().getIRI().getFragment(), true)) {
	            	individuals.add(individual);
	            }
	            constraints.add(new ConstraintModel(name, rElements, individuals));	           
		    } catch (NoSuchPropertyException e) {
	            e.printStackTrace();
	        }
		}
		return constraints;
	}
	
	/**
	 * creates a new situation
	 * @param parts the parts which belongs to this situation
	 * @return if the storing was succesful
	 * @throws CreationException 
	 */
	public final void createSituation(final String name, final String ruleName,
			final String ruleClazz, final List<CreateConstraintModel> models)
					throws CreationException {
		List<OWLNamedIndividual> rules = new ArrayList<OWLNamedIndividual>();
		List<OWLNamedIndividual> constraints = new ArrayList<OWLNamedIndividual>();
		List<OWLNamedIndividual> ruleElements = new ArrayList<OWLNamedIndividual>();
		rules.add(roh.getGoh().getIndividualByIdAndClass(ruleName, ruleClazz, true));
		
		try {
			for (CreateConstraintModel model : models) {
				constraints.add(roh.createConstraintIndividual(model.getName(),
						model.getClazz(), model.getRuleElements()));
				ruleElements.addAll(model.getRuleElements());
			}
			roh.createSituation(name, rules, ruleElements, constraints);
		} catch (NullPointerException n) {
			throw new CreationException("Failed to create a new Situation. No Ruleelements are specified.");
		} catch (Exception e) {
			System.err.println("Failed to create a new Situation. Cause: " );
			e.printStackTrace();
			throw new CreationException("Failed to create a new Situation.");
		} 
		updateSituations();
	}
	/**
	 * Make message box.
	 *
	 * @param parent the parent
	 * @param text the text
	 */
	public void makeMessageBox(final Shell parent, final String text) {
    	MessageBox box = new MessageBox(parent);
		box.setMessage(text);
		box.open();
    }

	/**
	 * creates a new constraint class.
	 * @param name
	 * @param sRuleElements
	 */
	public void createNewConstraint(final Shell parent, final String name, final String[] sRuleElements) {
		try {
			roh.createConstraintClass(name, Arrays.asList(sRuleElements));
		} catch (NullPointerException e) {
			makeMessageBox(parent, "Failed to create a new Constraint. Pleas load an Ontology.");
		}
	}
	
//	TODO Javadoc
	public String getRuleElementRelation(final String sourceREType, final String targetREType) throws WrongOrderException {
		return roh.getRuleElementRelationType(sourceREType, targetREType);
	}
}