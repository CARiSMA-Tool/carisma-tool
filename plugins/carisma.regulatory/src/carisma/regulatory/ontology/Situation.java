package carisma.regulatory.ontology;

import java.util.Collection;

/**
 * Represents a Situation element in the ontology.
 */
public interface Situation {
	
    /**
     * Returns the name of the situation.
     * @return - the name
     */
    public String getName();
    
	/**
	 * returns a collection of involved Rules.
	 * @return collection
	 */
	public Collection<Rule> getInvolvedRules();
	
	/**
	 * Returns a collection of rule elements involved in this situation.
	 * @return
	 */
	public Collection<RuleElement> getInvolvedRuleElements();
	
	/**
	 * Returns the constraints that this situation entails.
	 * @return
	 */
	public Collection<Constraint> getConstraintsToCheck();
	
}
