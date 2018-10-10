package carisma.regulatory.ontology;

import java.util.Collection;

public interface Constraint {
    
// TODO: remove when the file containing the mapping of constraints to carisma checks exist
    /**
     * Temporary method to return the type of the constraint so
     * that recommendations can be given.
     * @return
     */
    public String getType();
    
    /**
     * Returns the parameters this constraints uses.
     * @return
     */
    public Collection<RuleElement> getParameters();

}
