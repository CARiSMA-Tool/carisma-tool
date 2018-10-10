package carisma.regulatory.ontology;

import java.util.List;

/**
 * Represents an Activity RuleElement in the Ontology.
 *
 */
public interface Activity extends RuleElement {
    
    /**
     * returns the properties of the Activity element.
     * @return list of properties
     */
    public List<Property> getProperties();
    
    /**
     * returns the used artifacts of the Activity element.
     * @return list of artifacts
     */
    public List<Artifact> getUsedArtifacts();
}
