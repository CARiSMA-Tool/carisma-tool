package carisma.xutils.regulatory.ui.model;

import java.util.Collections;
import java.util.List;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

public class ConstraintModel {
	
	private String name = "";
	private List<String> ruleelements = null;
	private List<OWLNamedIndividual> instances = null;
	
	public ConstraintModel(final String name, final List<String> ruleElements, final List<OWLNamedIndividual> instances) {
		this.setName(name);
		this.setRuleelements(ruleElements);
		this.setInstances(instances);
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name the name to set
	 */
	private void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the ruleelements
	 */
	public List<String> getRuleelements() {
		return Collections.unmodifiableList(ruleelements);
	}

	/**
	 * @param ruleelements the ruleelements to set
	 */
	private void setRuleelements(List<String> ruleelements) {
		this.ruleelements = ruleelements;
	}

	/**
	 * @return the instances
	 */
	public List<OWLNamedIndividual> getInstances() {
		return Collections.unmodifiableList(instances);
	}

	/**
	 * @param instances the instances to set
	 */
	private void setInstances(List<OWLNamedIndividual> instances) {
		this.instances = instances;
	}
	

}
