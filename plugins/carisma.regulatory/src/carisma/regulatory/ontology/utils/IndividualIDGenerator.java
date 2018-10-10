package carisma.regulatory.ontology.utils;

import java.util.List;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

public class IndividualIDGenerator {
    
    private IndividualIDGenerator() {
        
    }
    
    /**
     * generates a individual id for a given situation. The id includes the name of the rules and the situation name.
     * @param rules
     * @param situationName
     * @return the id
     */
    public static final String generateSituationID(final List<OWLNamedIndividual> rules, final String situationName) {
        StringBuffer id = new StringBuffer();
        for (OWLNamedIndividual rule : rules) {
            id.append(rule.getIRI().getFragment());
            id.append("_");
        }
        id.append(situationName);
        return id.toString();
    }
    
    /**
     * generates a individual id for a ruleelement. The id includes the ruleelement name and its type.
     * @param ruleElementName
     * @param ruleElementtype
     * @return the id
     */
    public static final String generateRuleElementID(final String ruleElementName, final String ruleElementtype) {
        StringBuffer id = new StringBuffer();
        id.append(ruleElementtype);
        id.append("_");
        id.append(ruleElementName.replaceAll(" ", "_"));
        return id.toString();
    }
    
    /**
     * generates a individual id for a constraint. the id includes the constraint name and its type.
     * @param constraintName
     * @param constraintType
     * @return the id
     */
    public static final String generateConstraintID(final String constraintName, final String constraintType) {
        StringBuffer id = new StringBuffer();
        id.append(constraintType);
        id.append("_");
        id.append(constraintName.replaceAll(" ", "_"));
        return id.toString();
    }
    
    /**
     * generates a individual id for a guideline entry
     * @param guidelinesName
     * @param source
     * @param id
     * @return
     */
    public static final String generateGuidelineEntryID(final String guidelinesName, final String source, final String id) {
        StringBuffer newId = new StringBuffer();
        newId.append(guidelinesName + " ");;
        newId.append(id + " ");
        newId.append(source);
        return newId.toString().replace(" ", "_");
    }
    
}
