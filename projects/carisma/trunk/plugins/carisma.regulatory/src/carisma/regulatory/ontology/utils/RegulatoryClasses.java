package carisma.regulatory.ontology.utils;

public enum RegulatoryClasses {
    CLS_RULE ("Rule"),
    CLS_RULEELEMENT ("RuleElement"),
    CLS_PROCESS ("Process"),
    CLS_ROLE ("Role"),
    CLS_ACTIVITY ("Activity"),
    CLS_ARTIFACT ("Artifact"),
    CLS_PROPERTY ("Property"),
    
    // Checkable Situations, their associated constraints and their parameters
    CLS_SITUATION ("Situation"),
    CLS_CONSTRAINT ("Constraint"),
    
    // BDSG, BGB laws (juris)
    CLS_LAWRULE ("LawRule"),
    CLS_LAW ("Law"),
    CLS_SECTION ("Section"),
    CLS_PARAGRAPH ("Paragraph"),
    
    // BSI
    CLS_BSIRULE ("BSIRule"),
    CLS_BSIELEMENT ("BSIElement"),
    CLS_BSITHREAT ("BSIThreat"),
    CLS_BSIMEASURE ("BSIMeasure"),
    // MARisk VA
    CLS_MARISKRULE ("MARiskRule"),
    CLS_MARISKCLAUSE ("MARiskClause"),
    CLS_MARISKENTRY ("MARiskEntry"),
    CLS_MARISKBINDING ("MARiskBinding"),
    CLS_MARISKCOMMENT ("MARiskComment");
    
    
    private String fragment = "";
    
    RegulatoryClasses(final String newName) {
        fragment = newName;
    }
    
    String getFragment() {
        return fragment;
    }
}
