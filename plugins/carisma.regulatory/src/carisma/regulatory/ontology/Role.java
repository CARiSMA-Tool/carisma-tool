package carisma.regulatory.ontology;

import java.util.List;

/**
 * Represents a Role Ruleelement in the ontology.
 *
 */
public interface Role extends RuleElement {
    /**
     * returns a list of performable activities of this Ruleelement.
     * @return list
     */
    public List<Activity> getPerformableActivities();
    
    /**
     * returns a list of artifacts of this Ruleelement.
     * @return list
     */
    public List<Artifact> getManagedArtifacts();
    
    /**
     * returns a list of properties of this Ruleelement.
     * @return list
     */
    public List<Property> getProperties();
}
