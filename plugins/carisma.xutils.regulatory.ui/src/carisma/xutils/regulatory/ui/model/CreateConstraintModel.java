package carisma.xutils.regulatory.ui.model;

import java.util.Collections;
import java.util.List;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

public class CreateConstraintModel {
	
	private String name = "";
	private String clazz = "";
	private List<OWLNamedIndividual> parameters = null;
	private List<String> neededRuleElements;
	
	public CreateConstraintModel(final String clazz, final List<String> neededRuleElements) {
		this.setNeededRuleElements(neededRuleElements);
		this.setClazz(clazz);
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
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the clazz
	 */
	public String getClazz() {
		return clazz;
	}

	/**
	 * @param clazz the clazz to set
	 */
	private void setClazz(String clazz) {
		this.clazz = clazz;
	}

	/**
	 * @return the parameters
	 */
	public List<OWLNamedIndividual> getRuleElements() {
		return Collections.unmodifiableList(parameters);
	}

	/**
	 * @param parameters the parameters to set
	 */
	public void setRuleElements(List<OWLNamedIndividual> parameters) {
		this.parameters = parameters;
	}

	/**
	 * @return the neededRuleElements
	 */
	public List<String> getNeededRuleElements() {
		return Collections.unmodifiableList(neededRuleElements);
	}

	/**
	 * @param neededRuleElements the neededRuleElements to set
	 */
	private void setNeededRuleElements(List<String> neededRuleElements) {
		this.neededRuleElements = neededRuleElements;
	}
	
	/**
	 * deletes the variables which will be needed to create a constraint.
	 * This means name and parameters are deleted.
	 */
	public void deleteVars() {
		this.name = "";
		this.parameters = null;
	}
}
