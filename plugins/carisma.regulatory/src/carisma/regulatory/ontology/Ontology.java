package carisma.regulatory.ontology;

import java.util.Collection;

/**
 * Represents the ontology.
 * 
 * @author wenzel
 *
 */
public interface Ontology {
	
	/**
	 * The Rule individuals stored in the ontology.
	 * @return
	 */
	public Collection<Rule> getRules();
	
	/**
	 * The Situation individuals stored in the ontology.
	 * @return
	 */
	public Collection<Situation> getSituations();
	
	
	/**
	 * The RuleElements stored in this ontology.
	 * @return
	 */
	public Collection<RuleElement> getRuleElements();

}
