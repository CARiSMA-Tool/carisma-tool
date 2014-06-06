package carisma.regulatory.ontology.utils;

/**
 * Utility class for simple creation of Annotations for OWL.
 * @author dwarzecha
 *
 */
public class Annotation {
    private String propertyName = null;
    
    private Object propertyValue = null;
    
    public Annotation(final String newPropertyName, final Object newPropertyValue) {
        propertyName = newPropertyName;
        propertyValue = newPropertyValue;
    }
    
    public String getName() {
        return propertyName;
    }
    
    public Object getValue() {
        return propertyValue;
    }
}
