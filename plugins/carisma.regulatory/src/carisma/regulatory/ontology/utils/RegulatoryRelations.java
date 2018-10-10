package carisma.regulatory.ontology.utils;

public enum RegulatoryRelations {
    REL_RULE_REFFEREDRULES (RegulatoryClasses.CLS_RULE + "/ReferredRules"),
    // RuleElements
    REL_ACTIVITY_USEDARTIFACTS (RegulatoryClasses.CLS_ACTIVITY + "/UsedArtifacts"),
    REL_ACTIVITY_PROPERTIES (RegulatoryClasses.CLS_ACTIVITY + "/Properties"),
    REL_ARTIFACT_PROPERTIES (RegulatoryClasses.CLS_ARTIFACT + "/Properties"),
    REL_PROCESS_ACTIVITIES (RegulatoryClasses.CLS_PROCESS + "/Activities"),
    REL_PROCESS_PARTICIPANTS (RegulatoryClasses.CLS_PROCESS + "/Participants"),
    REL_PROCESS_USEDARTIFACTS (RegulatoryClasses.CLS_PROCESS + "/UsedArtifacts"),
    REL_PROCESS_PROPERTIES (RegulatoryClasses.CLS_PROCESS + "/Properties"),
    REL_ROLE_PERFORMABLEACTIVITIES (RegulatoryClasses.CLS_ROLE + "/PerformableActivities"),
    REL_ROLE_MANAGEDARTIFACTS (RegulatoryClasses.CLS_ROLE + "/ManagedArtifacts"),
    REL_ROLE_PROPERTIES (RegulatoryClasses.CLS_ROLE + "/Properties"),
// TODO: Check if these relations are okay or too specific
    REL_RULE_RULEELEMENTS (RegulatoryClasses.CLS_RULE + "/ContainedRuleElements"),
    REL_RULE_CONTAINEDACTIVITIES (RegulatoryClasses.CLS_RULE + "/ContainedActivities"),
    REL_RULE_CONTAINEDARTIFACTS (RegulatoryClasses.CLS_RULE + "/ContainedArtifacts"),
    REL_RULE_CONTAINEDROLES (RegulatoryClasses.CLS_RULE + "/ContainedRoles"),
    REL_RULE_CONTAINEDPROPERTIES (RegulatoryClasses.CLS_RULE + "/ContainedProperties"),
    REL_RULE_CONTAINEDPROCESSES (RegulatoryClasses.CLS_RULE + "/ContainedProcesses"),
    
    // Checkable Situations, their associated constraints and their parameters
    REL_SITUATION_INVOLVEDRULES (RegulatoryClasses.CLS_SITUATION + "/InvolvedRules"),
    REL_SITUATION_INVOLVEDRULEELEMENTS (RegulatoryClasses.CLS_SITUATION + "/InvolvedRuleElements"),
    REL_SITUATION_CHECKED_BY (RegulatoryClasses.CLS_SITUATION + "/checkedBy"),
    REL_CONSTRAINT_HASPARAMETERS (RegulatoryClasses.CLS_CONSTRAINT + "/hasParameters"),
    
    // BDSG, BGB laws (juris)
    REL_LAW_PARAGRAPHS (RegulatoryClasses.CLS_LAW + "/Paragraphs"),
    REL_PARAGRAPH_SECTIONS (RegulatoryClasses.CLS_PARAGRAPH + "/Sections"),
    // BSI
    REL_BSIELEMENT_THREATS (RegulatoryClasses.CLS_BSIELEMENT + "/ReferredThreats"),
    REL_BSIELEMENT_MEASURES (RegulatoryClasses.CLS_BSIELEMENT + "/ReferredMeasures"),
    // MARisk
    REL_MARISKCLAUSE_HASSUBCLAUSES (RegulatoryClasses.CLS_MARISKCLAUSE + "/hasSubClauses"),
    REL_MARISKCLAUSE_HASENTRIES (RegulatoryClasses.CLS_MARISKCLAUSE + "/hasEntries"),
    REL_MARISKBINDING_HASCOMMENTS (RegulatoryClasses.CLS_MARISKBINDING + "/hasComments");
    
    private String fragment = "";
    
    RegulatoryRelations(final String newName) {
        fragment = newName;
    }
    
    String getFragment() {
        return fragment;
    }
}
