package carisma.regulatory.ontology;

import java.util.Collection;

/**
 * Represents a Ruleelement in the ontology.
 *
 */
public interface RuleElement{

	/**
	 * the element type of the rule element
	 * @return
	 */
	public String getElementType();
	
	/**
	 * the name of the rule element
	 * @return
	 */
	public String getName();
	
	
	/**
	 * returns the ruleelements to which this ruleelement has a relation.
	 * @return
	 */
	public Collection<RuleElementRelation> getRelatedRuleElements();
	
	/**
	 * returns the rules in which this ruleelement is contained.
	 * @return
	 */
	public Collection<RuleElementContainment> getContainingRules();
}
