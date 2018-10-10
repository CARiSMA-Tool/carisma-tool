package carisma.regulatory.ontology;

import java.util.Collection;

/**
 * Represents a Rule of the ontology.
 * @author wenzel
 *
 */
public interface Rule {

	/**
	 * The name of the Rule.
	 * @return
	 */
	public String getTitle();
	
	/**
	 * Is set to true if the Rule does not lead to a situation that has to be checked.
	 * @return
	 */
	public boolean isUnprovable();

	/**
	 * Other Rule individuals that are referenced.
	 * @return
	 */
	public Collection<Rule> getReferredRules();
	
	/**
	 * returns the ruleelements which are contained in this rule.
	 * @return
	 */
	public Collection<RuleElementContainment> getContainedRuleElements();
	
}
