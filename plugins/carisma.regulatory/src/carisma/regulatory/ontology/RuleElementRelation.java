package carisma.regulatory.ontology;

public interface RuleElementRelation {

    
    /**
     * returns the source ruleelement.
     * @return
     */
    public RuleElement getSource();
    
    /**
     * returns the target ruleelement.
     * @return
     */
    public RuleElement getTarget();
    
    /**
     * returns the type of the relation.
     * @return
     */
    public String getType();
    
    /**
     * returns the description of the relation.
     * @return
     */
    public String getDescription();
}
