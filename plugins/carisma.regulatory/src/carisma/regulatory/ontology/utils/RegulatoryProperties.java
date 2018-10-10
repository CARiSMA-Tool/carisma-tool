package carisma.regulatory.ontology.utils;

import carisma.regulatory.ontology.utils.RegulatoryClasses;

public enum RegulatoryProperties {
    PROP_RULE_TITLE (RegulatoryClasses.CLS_RULE.getFragment() + "/Title"),
    PROP_RULE_UNPROVABLE (RegulatoryClasses.CLS_RULE.getFragment() + "/Unprovable"),
    // RuleElements
    PROP_RULEELEMENT_TYPE (RegulatoryClasses.CLS_RULEELEMENT.getFragment() + "/ElementType"),
    PROP_RULEELEMENT_NAME (RegulatoryClasses.CLS_RULEELEMENT.getFragment() + "/Name"),
    PROP_RULEELEMENT_REPRESENTATION (RegulatoryClasses.CLS_RULEELEMENT.getFragment() + "/Representation"),
    PROP_RULEELEMENT_STARTINDEX (RegulatoryClasses.CLS_RULEELEMENT.getFragment() + "/Startindex"),
    PROP_RULEELEMENT_ENDINDEX (RegulatoryClasses.CLS_RULEELEMENT.getFragment() + "/Endindex"),
    // Checkable Situations, their associated constraints and their parameters
    PROP_SITUATION_NAME (RegulatoryClasses.CLS_SITUATION.getFragment() + "/Name"),
    PROP_CONSTRAINT_SPECIFICATION (RegulatoryClasses.CLS_CONSTRAINT.getFragment() + "/Specification"),
    
    // BDSG, BGB laws (juris)
    PROP_PARAGRAPH_NUMBER (RegulatoryClasses.CLS_PARAGRAPH.getFragment() + "/Number"),
    PROP_SECTION_CONTENT (RegulatoryClasses.CLS_SECTION.getFragment() + "/Content"),
    // BSI
    PROP_BSIRULE_CONTENT (RegulatoryClasses.CLS_BSIRULE.getFragment() + "/Content"),
    // MARisk
    PROP_MARISKCLAUSE_NUMBER (RegulatoryClasses.CLS_MARISKCLAUSE.getFragment() + "/Number"),
    PROP_MARISKCLAUSE_NAME (RegulatoryClasses.CLS_MARISKCLAUSE.getFragment() + "/Name"),
    PROP_MARISKENTRY_NUMBER (RegulatoryClasses.CLS_MARISKENTRY.getFragment() + "/Number"),
    PROP_MARISKENTRY_TEXT (RegulatoryClasses.CLS_MARISKENTRY.getFragment() + "/Text");
    
    
    private String fragment = "";
    
    RegulatoryProperties(final String newName) {
        fragment = newName;
    }
    
    String getFragment() {
        return fragment;
    }
}
