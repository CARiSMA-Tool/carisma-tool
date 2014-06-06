package carisma.regulatory.ontology;

/**
 * Represents a Rule -> RuleElement Reference.
 * @author dbuerger
 *
 */
public interface RuleElementContainment {
    
    /**
     * returns the ruleelement to which the reference points.
     * @return
     */
    public RuleElement getRuleElement();
    
    /**
     * returns a rule, which hold a reference to the ruleelement.
     * @return
     */
    public Rule getRule();
    
    /**
     * returns the start index of the ruleelement in the rule.
     * @return
     */
    public int getStart();
    
    /**
     * returns the end index of the ruleelement in the rule.
     * @return
     */
    public int getEnd();
    
    /**
     * returns the representation of the ruleelement in the rule.
     * @return
     */
    public String getRepresentation();
}
