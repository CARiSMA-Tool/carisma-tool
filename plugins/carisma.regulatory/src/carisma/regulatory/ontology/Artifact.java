package carisma.regulatory.ontology;

import java.util.List;

/**
 * Represents an Artifact Ruleelement in the Ontology
 *
 */
public interface Artifact extends RuleElement {
    /**
     * returns the properties of the Artifact element
     * @return list of properties
     */
    public List<Property> getProperties();
}
