package carisma.regulatory.ontology;

import java.util.List;

/**
 * Represents a Process Ruleelement in the ontology.
 *
 */
public interface Process extends RuleElement {
    
    /**
     * returns a list of activities of this Process Element.
     * @return list 
     */
    public List<Activity> getActivities();
    
    /**
     * returns a list of participants of this Process Element.
     * @return list
     */
    public List<Role> getParticipants();
    
    /**
     * returns a list of artifacts used by this Process Element.
     * @return list
     */
    public List<Artifact> getUsedArtifacts();
    
    /**
     * returns a list of properties of this Process Element.
     * @return list
     */
    public List<Property> getProperties();
}
