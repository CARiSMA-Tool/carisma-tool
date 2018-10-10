package carisma.xutils.regulatory.webgui;

import java.io.File;
import java.security.interfaces.DSAKey;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;

import javax.swing.text.html.HTMLDocument.Iterator;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;

import carisma.regulatory.ontology.utils.GenericOntologyHelper;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.OntologyHelperException;
import carisma.regulatory.ontology.utils.RegulatoryClasses;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

/**
 * this class helps to deal with the ontology in a web browser.
 * @author dbuerger
 *
 */

public class WebOntologyHelper {
	
	/**
	 * the generic ontology helper.
	 */
	private GenericOntologyHelper goh = null;
	/**
	 * the regulatory ontology helper.
	 */
	private RegulatoryOntologyHelper roh = null;
	/**
	 * the file.
	 */
	private File file = null;
    /** to synchronize the operations on the ontology. */
	private ReentrantLock lock = null;
	
	    /** to handle the ruleelements */
    public static String[] ruleElementClasses = {  RegulatoryOntologyHelper.CLS_ACTIVITY, RegulatoryOntologyHelper.CLS_ARTIFACT,
                   RegulatoryOntologyHelper.CLS_PROCESS, RegulatoryOntologyHelper.CLS_PROPERTY, RegulatoryOntologyHelper.CLS_ROLE };
	
	
	/**
	 * initializes the ontology helper.
	 * @param file path to the file
	 */
	public WebOntologyHelper(final String file) throws OntologyHelperException{
		this.file = new File(file);
		lock = new ReentrantLock(true);
		goh = new GenericOntologyHelper();
		goh.loadOWLOntologyFromFile(this.file);
		roh = new RegulatoryOntologyHelper(goh);		
	}
	
	/**
	 * returns the Regulatory Ontology Helper.
	 * @return the roh
	 */
	public RegulatoryOntologyHelper getRoh() {
	    return this.roh;
	}
	
	
	/**
	 * stores the ontology to the given file.
	 */
	public final void storeOntology() throws OntologyHelperException {
	    lock.lock();
		goh.saveOntologyToFile(file);
		lock.unlock();
	}
	
	
	/**
	 * for a given individual, there will be added a new reference to a matching element entry.
	 * 
	 * @param rule the individual
	 * @param ruleElementClass the class name of the element entry
	 * @param textInRule the name of the element
	 * @param ruleElementName the type of the element
	 * @param start the start index of the individual
	 * @param end the end index of the individual
	 * @return a new individual
	 */
	public final OWLNamedIndividual createAppropriateElement(final OWLNamedIndividual rule, 
	        final String ruleElementClass, final String textInRule, final String ruleElementName, final String start, final String end) {
			// this is the method from the RuleElementsCreator in superior importer
		int startIndex = Integer.parseInt(start);
		int endIndex = Integer.parseInt(end);
	    lock.lock();
		OWLNamedIndividual ruleElement;
		if (ruleElementClass.equals(RegulatoryOntologyHelper.CLS_ACTIVITY)) {
			ruleElement = roh.createREActivity(ruleElementName);
			roh.createContainedRuleElementsRelation(rule, ruleElement, textInRule, startIndex, endIndex);
		} else if (ruleElementClass.equals(RegulatoryOntologyHelper.CLS_ARTIFACT)) {
		    ruleElement = roh.createREArtifact(ruleElementName);
			roh.createContainedRuleElementsRelation(rule, ruleElement, textInRule, startIndex, endIndex);
		} else if (ruleElementClass.equals(RegulatoryOntologyHelper.CLS_PROCESS)) {
		    ruleElement = roh.createREProcess(ruleElementName);
			roh.createContainedRuleElementsRelation(rule, ruleElement, textInRule, startIndex, endIndex);
		} else if (ruleElementClass.equals(RegulatoryOntologyHelper.CLS_PROPERTY)) { 
		    ruleElement = roh.createREProperty(ruleElementName);
			roh.createContainedRuleElementsRelation(rule, ruleElement, textInRule, startIndex, endIndex);
		} else if (ruleElementClass.equals(RegulatoryOntologyHelper.CLS_ROLE)) {
		    ruleElement = roh.createRERole(ruleElementName);
			roh.createContainedRuleElementsRelation(rule, ruleElement, textInRule, startIndex, endIndex);
		} else {
			System.out.println("Error: Unsupported Element Type " + ruleElementClass);
			ruleElement = null;
			lock.unlock();
			ruleElement.toString(); //Exception zum debuggen
		}
		lock.unlock();
		return ruleElement;
	}
	
	/**
	 * search for an individual given by name.
	 * @param name the name of the individual
	 * @return the individual
	 */
	public final OWLNamedIndividual getIndividualByName(final String name) {
	    lock.lock();
	    OWLNamedIndividual individual = goh.getIndividualById(name);
	    lock.unlock();
		return individual;
	}
	
	
	/**
	 * returns the text of the individual. if the text contains marked rule elements, their occurence will be surrounded by
	 * html tags to change their background-color on the page.
	 * @param ruleName the name of the individual
	 * @param propertyType the type of the property
	 * @return the content of the individual
	 */
	public final String getText(final String ruleName, final String propertyType) {
	    lock.lock();
	        // to refactor the umlauts for HTML
	    String[] umlaute = {"ä", "&#228;", "Ä", "&#196;", "ö", "&#246;", "Ö", "&#214;", "ü", "&#252;",
	                        "Ü", "&#220;", "ß", "&#223;", "-", "&#8722;", "•", "&#8226;", "„", "&#8222;", "”", "&#8221;"};
	        // to split the given string and store the words
	    String response = "";
		OWLNamedIndividual rule = goh.getIndividualById(ruleName);
		if (rule != null) {
    		Set<OWLAnnotation> annotations = rule.getAnnotations(goh.getOntology(), goh.getOWLAnnotationProperty(propertyType));
    		ArrayList<String> indices = (ArrayList<String>) roh.getIndicesOfRE(ruleName);
    		if (!annotations.isEmpty()) {
    		    response = annotations.iterator().next().toString();
    		    response = response.substring(response.indexOf("\"") + 1, response.lastIndexOf("\""));
    		}
    		
    		if (!indices.isEmpty()) {
    		    response = insertHTML(indices, response, getTypeOfIndividual(ruleName));
    		} 
    		    // replace all the occuring umlauts with the right html-code
    		for (int i = 0; i < umlaute.length; i += 2) {
                  response = response.replace(umlaute[i], umlaute[i + 1]);
            }
    		response = response.trim();
		} else  {
		    response = "error";
		}
		lock.unlock();
//		System.out.println("Response from getText: " + response);
		return response;
	}
	
	/**
	 * inserts the desired span-tags in the result string.
	 * @param changes the changes
	 * @param result the result string
	 * @param ruleType the rule type
	 * @return the changed string
	 */
	private final String insertHTML(final ArrayList<String> changes, final String result, final String ruleType) {

	    // to split the given string and store the words
        String newResult = result; String type = "";
            // to determine the indices in the text
        int start = 0; int end = 0; int offset = 0;
        try {
            Collections.sort(changes, new Comparator<String>() {
                // to compare the Strings on the integer value, sorted downwards
                @Override
                public int compare(final String o1, final String o2) {
                    if (o1.substring(0, o1.indexOf("_")).equals(o2.substring(0, o2.indexOf("_")))) {
                        return 0;
                    } else if (Integer.valueOf(o1.substring(0, o1.indexOf("_"))) > Integer.valueOf(o2.substring(0, o2.indexOf("_")))) {
                        return -1;
                    } else {
                        return 1;
                    }
                 }
            });
        } catch (NumberFormatException n) {
            System.err.println("Failed to sort the list. Wrong numbers stored! " + n.getMessage());
            return result;
        }
            // to correct the indices 
        if (ruleType.equals(RegulatoryOntologyHelper.CLS_MARISKCOMMENT) 
                || ruleType.equals(RegulatoryOntologyHelper.CLS_MARISKBINDING)
                || ruleType.equals(RegulatoryOntologyHelper.CLS_SECTION)) {
            offset = 1;
        }
        try {
            for (String change : changes) {
                type = change.substring(change.indexOf("&")  + 1, change.length());
                start = Integer.valueOf(change.substring(0, change.indexOf("_"))) - offset;
                if (Integer.valueOf(change.substring(change.indexOf("_") + 1, change.indexOf(type) - 1)) == end) {
                    // TODO do nothing for now, only one span tag should be added for a word
                    // In the Website it is an invalid selection for now
                } else {
                    end = Integer.valueOf(change.substring(change.indexOf("_") + 1, change.indexOf(type) - 1)) - offset;
                
                        // only highlight the words (no whitespaces etc)
                    if (String.valueOf(newResult.charAt(end + 1)).equals(" ") 
                            || String.valueOf(newResult.charAt(end + 1)).equals(",")
                            || String.valueOf(newResult.charAt(end + 1)).equals(".")
                            || String.valueOf(newResult.charAt(end + 1)).equals("-")
                            || String.valueOf(newResult.charAt(end + 1)).equals(")")
                            || String.valueOf(newResult.charAt(end + 1)).equals("``")
                            || String.valueOf(newResult.charAt(end + 1)).equals("\"")
                            || String.valueOf(newResult.charAt(end + 1)).equals(":")
                            || String.valueOf(newResult.charAt(end + 1)).equals("\n")) {
                        end = end + 1;
                    }
                        // insert the span-tags
                    if (type.equals(RegulatoryOntologyHelper.CLS_ARTIFACT)) {
                        newResult = newResult.substring(0, end) + "</span>" + newResult.substring(end);
                        newResult = newResult.substring(0, start + 1) + "<span name=\"art\">" + newResult.substring(start + 1);
                    } else if (type.equals(RegulatoryOntologyHelper.CLS_PROPERTY)) {
                        newResult = newResult.substring(0, end) + "</span>" + newResult.substring(end);
                        newResult = newResult.substring(0, start + 1) + "<span name=\"ppy\">" + newResult.substring(start + 1);
                    } else if (type.equals(RegulatoryOntologyHelper.CLS_ACTIVITY)) {
                        newResult = newResult.substring(0, end) + "</span>" + newResult.substring(end);
                        newResult = newResult.substring(0, start + 1) + "<span name=\"act\">" + newResult.substring(start + 1);
                    } else if (type.equals(RegulatoryOntologyHelper.CLS_ROLE)) {
                        newResult = newResult.substring(0, end) + "</span>" + newResult.substring(end);
                        newResult = newResult.substring(0, start + 1) + "<span name=\"rol\">" + newResult.substring(start + 1);
                    } else if (type.equals(RegulatoryOntologyHelper.CLS_PROCESS)) {
                        newResult = newResult.substring(0, end) + "</span>" + newResult.substring(end);
                        newResult = newResult.substring(0, start + 1) + "<span name=\"pro\">" + newResult.substring(start + 1);
                    }
                }
            }
        } catch (NumberFormatException n) {
            System.err.println("there were not only integers... " + n.getLocalizedMessage());
        } catch (IllegalArgumentException il) {
            System.err.println("here's something going wrong .... " + il.getLocalizedMessage());
        } catch (IndexOutOfBoundsException ib) {
            System.err.println("there were curious indexes .... " + ib.getLocalizedMessage());
        }
        return newResult;
	}
	
	/**
	 * returns the individuals matching the classname.
	 * @param className the class name 
	 * @return the set of individuals
	 */
	public final Set<OWLNamedIndividual> getIndividuals(final String className) {
	    lock.lock();
	    Set<OWLNamedIndividual> set = goh.getIndividuals(className, true);
	    lock.unlock();
		return set;
	}

	/**
	 * creates a new situation
	 * @param parts the parts which belongs to this situation
	 * @return if the storing was succesful
	 */
	public final String createSituation(final String[] parts) {
	    String response = "succesful";
	    String constraintClassName = ""; String constraintName = "";
	    ArrayList<OWLNamedIndividual> rulesForSituation = new ArrayList<OWLNamedIndividual>();
	    ArrayList<OWLNamedIndividual> ruleElementsForSituation = new ArrayList<OWLNamedIndividual>();
	    List<OWLNamedIndividual> ruleElementsForConstraint = new ArrayList<OWLNamedIndividual>();
	    List<OWLNamedIndividual> constraintIndividuals = new ArrayList<OWLNamedIndividual>();
    	try {
    	    String rule = parts[0].substring(parts[0].indexOf("=") + 1);
    	    String situationName = parts[1].substring(parts[1].indexOf("=") + 1);
    	    for (int i = 2; i < parts.length; i++) {
    	        if (parts[i].startsWith("Constraint")) {
    	            constraintClassName = parts[i].substring(0, parts[i].indexOf("(")).substring(parts[i].indexOf("=") + 1);
    	            String[] specification = parts[i].substring(parts[i].indexOf("(") + 1, parts[i].indexOf(")")).split("&");
    	            constraintName = specification[0];
    	            for (int j = 1; j < specification.length; j++) {
    	                ruleElementsForConstraint.add(goh.getIndividualById(specification[j]));
    	            }
    	            constraintIndividuals.add(roh.createConstraintIndividual(constraintName, constraintClassName, ruleElementsForConstraint));
    	            ruleElementsForSituation.addAll(ruleElementsForConstraint);
    	            ruleElementsForConstraint.clear();
    	         }
    	    }
	        rulesForSituation.add(getIndividualByName(rule));
    	    roh.createSituation(situationName, rulesForSituation, ruleElementsForSituation, constraintIndividuals);
    	    System.out.println("situation created..");
    	    storeOntology();
    	} catch (OntologyHelperException o) {
    	    response = "error";
    	} catch (IndexOutOfBoundsException i) {
    	    System.err.println("Wrong request was delivered.");
    	    i.printStackTrace();
    	    response = "error";
    	} catch (Exception e) {
    	    System.err.println("Something has gone wrong ...");
    	    e.printStackTrace();
    	}
//	    System.out.println("Response from create Situation: " + response);
	    return response;
	}
	
	/**
	 * returns the sub elements of the given rule element.
	 * @param ruleElementClass the rule element
	 * @param ruleName the rule where to look for the ruleElements (leave it empty for all elements)
	 * @return a string with sub elements
	 */
	public final String getSubElementsOfRe(final String ruleElementClass, final String ruleName) {
	    String response = "";
	    if (ruleName.equals("")) {
        	List<String> subElements = roh.getAllSubElementsOfRE(ruleElementClass);
        	for (String subElement : subElements) {
        	    response = response + "?" + subElement; 
        	} 
	    } else {
	        List<OWLNamedIndividual> subElements = roh.getSpecificSubElementsOfRE(ruleElementClass, ruleName);	        
	        for (OWLNamedIndividual subElement : subElements) {
	            response = response + "?" + subElement.getIRI().getFragment();
	        }
	    }
	    return response;
	}
	
	/**
	 * returns all sub elements of the known rule elements.
	 * @return string with all sub elements
	 */
	public final String getAllRuleElements() {
	    String response = "";
	    for (int i = 0; i < ruleElementClasses.length; i++) {
	        response = response + "&" + ruleElementClasses[i] + getSubElementsOfRe(ruleElementClasses[i], "");
	    }
	    response = response.trim();
//	    System.out.println(response);
	    return response;
	}
	
	/**
	 * returns all rule elements which belong to a specific rule.
	 * @param rule
	 * @return
	 */
	public final String getRuleElementsForSpecificRule(final String rule) {
	    String response = "";
	    String[] ruleElements = {RegulatoryOntologyHelper.CLS_ACTIVITY, RegulatoryOntologyHelper.CLS_ARTIFACT, 
                RegulatoryOntologyHelper.CLS_PROCESS, RegulatoryOntologyHelper.CLS_PROPERTY,
                RegulatoryOntologyHelper.CLS_ROLE};
	    for (int i = 0; i < ruleElements.length; i++) {
            response = response + "&" + ruleElements[i] + getSubElementsOfRe(ruleElements[i], rule);
        }
        response = response.trim();
//        System.out.println(response);
        return response;
	}
	
	/**
	 * returns the ontology.
	 * @return the ontology
	 */
	public final OWLOntology getOntology() {
		return goh.getOntology();
	}
	
	/**
	 * returns a string representation of the referred individuals by the given one
	 * @param name the name of the individual
	 * @return the string with the individuals
	 */
	public final String getReferences(final String name) {
	    String response = "";	    
	    List<OWLNamedIndividual> individuals = roh.getReferredRules(getIndividualByName(name));
	    for (OWLNamedIndividual individual : individuals) {
//	        System.out.println("Referenzen: " + individual.getIRI().getFragment());
	        response =  response + getTypeOfIndividual(individual.getIRI().getFragment()) 
	                    + "&" + individual.getIRI().getFragment() + "?";	        
	    }
	    return response.trim();
	}
	
	/**
	 * returns the type of an individual as string.
	 * @param name the name of the individual
	 * @return the type
	 */
	public String getTypeOfIndividual(String name) {
	    return goh.getTypeOfIndividual(getIndividualByName(name)).getFragment();
	}
	
	
	   
	/**
	 * removes an element from the ontology. Therefore it checks if the element contains a axioms with the given value and if the 
	 * axiom contains annotations which correspond to the given boundaries.
	 * @param ruleName the rule in which the relation should be removed
	 * @param wordToDeselect the word which should be deselected
	 * @param startPos the start index of the value
	 * @param endPos the end index of the value
	 * @return "success" or "error"
	 */
	public String deleteElement(final String ruleName, final String wordToDeselect, final String startPos, final String endPos) {
	    String response = "succes"; String start = ""; String end = "";
	    OWLAxiom currentAxiom = null;
	    OWLNamedIndividual rule = getIndividualByName(ruleName);
	    List<OWLAxiom> matchingAxioms = new ArrayList<OWLAxiom>(); 
	    Set<OWLAxiom> axioms = rule.getReferencingAxioms(getOntology());
	    for (OWLAxiom axiom : axioms) {
            if (axiom.toString().contains(wordToDeselect)) {
                matchingAxioms.add(axiom);
            }
	    }  
	    if (matchingAxioms.isEmpty()) {
	        response = "error";
	    }
	    
	    // test if there are more than one matching axiom
	    while (!matchingAxioms.isEmpty()) {
	        currentAxiom = matchingAxioms.remove(0);
            for (OWLAnnotation annotation : currentAxiom.getAnnotations()) {
                try {
                    if (annotation.getProperty().getIRI().toString().contains(
                            RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_STARTINDEX)) {
                        start = goh.owlAnnotationValueToString(annotation.getValue());
                    } else if (annotation.getProperty().getIRI().toString().contains(
                            RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_ENDINDEX)) {
                        end = goh.owlAnnotationValueToString(annotation.getValue());
                    } 
                } catch (IndexOutOfBoundsException i) {
                    System.err.println(annotation.getValue().toString() + "\n" + i.getLocalizedMessage());
                }
            }
            try { 
                if ((Integer.valueOf(start) <= Integer.valueOf(startPos)) 
                     && Integer.valueOf(end) >= Integer.valueOf(endPos)) {
//                     System.out.println(currentAxiom.toString() + "\n" + "start: " + start + " <= " + startPos + " / end: " + end + " >= " + endPos);
                     goh.removeAxiom(currentAxiom);
                 } else if (matchingAxioms.isEmpty()) {
                     response = "error"; 
                     break;
                 }
             } catch (NumberFormatException n) {
                 System.err.println("Wrong values have been stored! " + n.getLocalizedMessage());
                 response = "error";
                 continue;
            }
	    }
	    return response;
	}
	
	
	/**
	 * returns the names of the situations (and their rule elements) which belong to a given rule. 
	 * @param ruleName the rule
	 * @return the situations with their belonging rule elements or an empty string
	 */
	public String getSituations(final String ruleName) {
	    String response = "";
//	    System.out.println("Situations for " + ruleName);
	    for (OWLNamedIndividual situation : roh.getSituations(ruleName)) {
	        response = response + "?" + roh.getAnnotationFromIndividual(situation, RegulatoryOntologyHelper.PROP_SITUATION_NAME);
	        Set<OWLNamedIndividual> relatedConstraints = goh.getRelatedIndividuals(situation, RegulatoryOntologyHelper.REL_SITUATION_CHECKED_BY);
	        for (OWLNamedIndividual constraint : relatedConstraints) {
	            response = response + "%" + constraint.getIRI().getFragment();
	            Set<OWLNamedIndividual> relatedRuleElements = goh.
	                    getRelatedIndividuals(constraint, RegulatoryOntologyHelper.REL_CONSTRAINT_HASPARAMETERS);
	            for (OWLNamedIndividual ruleElement : relatedRuleElements) {
	                response = response + "&" + ruleElement.getIRI().getFragment();
	            }
	        }	        
	    }
	    response = response.trim();
	    System.out.println("Response from Situations: " + response);
	    return response;
	}
	
	    
	/**
	 * returns the local constraints with their rule elements and their individuals (if some exist)
	 * @return string
	 */
	public String getConstraintClasses() {
	    String response = "";
	    OWLClass constraintClass = goh.getOWLClass(RegulatoryOntologyHelper.CLS_CONSTRAINT);
	    for (OWLClassExpression subClass : constraintClass.getSubClasses(getOntology())) {
	        response = response + "?" + subClass.asOWLClass().getIRI().getFragment();
	        try {
                response = response + "&" + "ruleElements=" + goh.
                        getAnnotation(subClass.asOWLClass(), RegulatoryOntologyHelper.
                                PROP_CONSTRAINT_SPECIFICATION).getLiteral();
                response = response + "&" + "individuals=";
                for (OWLNamedIndividual individual: goh.
                        getIndividuals(subClass.asOWLClass().getIRI().getFragment(), true)) {
                    response = response + individual.getIRI().getFragment() + ",";
                }
                
            } catch (NoSuchPropertyException e) {
                e.printStackTrace();
            }
	    }
	    System.out.println("Response from getConstraintClasses: " + response);
	    return response;
	}
	
	    
	/**
	 * returns the specification of the given constraint (the related rule elements).
	 * @param name
	 * @return
	 */
	public String getConstraintSpecification(final String name) {
	    String response = "";
	    OWLClass owlClass = goh.getOWLClass(name);
	    System.out.println(owlClass);
	    try {
            response = "ruleElements=" + goh.getAnnotation(owlClass, RegulatoryOntologyHelper.
                            PROP_CONSTRAINT_SPECIFICATION).getLiteral();
        } catch (NoSuchPropertyException e) {
            response = "error";
            e.printStackTrace();
            
        }
	    System.out.println(response);
	    return response;
	    
	}
	    
	/**
	 * creates a new constraint class 
	 * @param name the name of the class
	 * @param participants the rule elements
	 */
	public String createConstraintClass(final String name, final String[] participants) {
	    List<String> ruleelements = new ArrayList<String>(Arrays.asList(participants));
	    ruleelements.remove(0);    // remove the name of the constraint class from the list
	    System.out.println("List without name of the constraint: " + Arrays.toString(ruleelements.toArray()) + " Name: " + name);
	    roh.createConstraintClass(RegulatoryOntologyHelper.convertToValidOwlId(name), ruleelements);
	    try {
	        storeOntology();
	    } catch (OntologyHelperException o) {
	        return "error";
	    }
	    return "success";
	}
	
	/**
	 * creates a new ruleElementsRelation for each entry of the given list. 
	 * This includes that only such relations are created, which really exist (e.g. Activity has no relation to Activity).
	 * In addition, the right relationType will be computed and the elements will be ordered, if they were in an oder which
	 * doesn't fit the relationType.
	 * @param listOfRelatedRuleElements
	 * @return
	 */
	public String createRuleElementRelation(final String[] listOfRelatedRuleElements) {
		Map<String, String[]> relatedRuleElements = roh.getRelatedRuleElementsMap();
		String key = ""; String[] value = null; String description = "";
		for (int i = 0; i < listOfRelatedRuleElements.length; i++) {
				// e.g. Activity_funktionierendes&Activity_Informationssicherheit$Beschreibung
			System.out.println(listOfRelatedRuleElements[i]);
			String[] withDescription = listOfRelatedRuleElements[i].split("-");
			System.out.println(Arrays.toString(withDescription));
			try {
				description = withDescription[1];
			} catch (IndexOutOfBoundsException e) {/* do nothing, the user hasn't entered a description */}
			String[] relation = withDescription[0].split("&");
			if (relation.length != 2) {
				System.err.println("Creating a RuleElementsRelation failed. The Array: " + Arrays.toString(relation) + " has more than 2 Members!");
				return "error";
			}
			key = relation[0].substring(0, relation[0].indexOf("_")) + "," + relation[1].substring(0, relation[1].indexOf("_"));
			value = relatedRuleElements.get(key);
			try {
				if (value[1].equals("true")) {
					OWLNamedIndividual sourceIndividual = goh.getIndividualByIdAndClass(relation[0], 
																relation[0].substring(0, relation[0].indexOf("_")), true);
					OWLNamedIndividual targetIndividual = goh.getIndividualByIdAndClass(relation[1], 
																relation[1].substring(0, relation[1].indexOf("_")), true);
					if (sourceIndividual != null && targetIndividual != null) {
						roh.createRelatedRuleElementsRelation(sourceIndividual, targetIndividual, value[0], description);		
					} else {
						System.err.println("Creating a RuleElementsRelation failed. There were not matching individuals found.");
						return "error";
					}
				} else {	// switch the ruleelements
					OWLNamedIndividual sourceIndividual = goh.getIndividualByIdAndClass(relation[1], 
																relation[1].substring(0, relation[1].indexOf("_")), true);
					OWLNamedIndividual targetIndividual = goh.getIndividualByIdAndClass(relation[0], 
																relation[0].substring(0, relation[0].indexOf("_")), true);
					if (sourceIndividual != null && targetIndividual != null) {
						roh.createRelatedRuleElementsRelation(sourceIndividual, targetIndividual, value[0], description);		
					} else {
						System.err.println("Creating a RuleElementsRelation failed. There were not matching individuals found.");
						return "error";
					}
				}
			} catch (NullPointerException n) {
				System.out.println("Failed to make a new relation between the following ruleelements: " + relation[1] +
								   " and " + relation [0]);
			} catch (Exception e) {
				System.err.println("Failed to make a new relation between the following ruleelements: " + relation[1] +
						   		   " and " + relation [0] + ". Cause: " + e.getLocalizedMessage());
			}
		}
		 try {
		        storeOntology();
		    } catch (OntologyHelperException o) {
		        return "error";
		    }
		return "succes";
	}
	
	/**
	 * checks the given pairs of ruleElements if they represent a correct relation.
	 * @param pairsOfRuleElements
	 * @return
	 */
	public List<String> checkSelectedRuleElements(final String[] pairsOfRuleElements) {
		List<String> validRuleElements = new ArrayList<String>();
		for (int i = 0; i < pairsOfRuleElements.length; i++) {
			if (pairsOfRuleElements[i].length() > 1) {	// check if the string is empty
				boolean isValid = roh.isRelationBetweenRuleElementsValid(pairsOfRuleElements[i]);
				validRuleElements.add(pairsOfRuleElements[i] + "=" + isValid);
			}			
		}
		return validRuleElements;
	}

	/**
	 * returns a string with the enumeration of the related and inverse related ruleelements.
	 * @param ruleElements
	 * @return
	 */
	public String getRelatedRuleElements(final String[] ruleElements) {
		String response = ""; String elements = "";
		List<OWLNamedIndividual> relatedRuleElements = null;
		for (int i = 0; i < ruleElements.length; i++) {
			if (ruleElements[i] != "") {
				OWLNamedIndividual individual = goh.getIndividualByIdAndSuperClass(ruleElements[i], RegulatoryOntologyHelper.CLS_RULEELEMENT, true);
				if (individual != null){
					relatedRuleElements = roh.getRelatedRuleElements(individual);
					for (OWLNamedIndividual o : relatedRuleElements) {
						elements = elements + o.getIRI().getFragment() + "&";
					}
					response = response + ruleElements[i] + "=" + elements + ",";
					elements = "";
				} else {
					System.err.println("No individual found for " + ruleElements[i]);
				}
				
			}
		}
		System.out.println(response);
		return response;
	}
}
