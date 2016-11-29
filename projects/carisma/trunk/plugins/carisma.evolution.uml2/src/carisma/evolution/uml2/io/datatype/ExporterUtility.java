package carisma.evolution.uml2.io.datatype;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

/** Utility class for Delta export.
 * 
 * @author bberghoff
 *
 */
public final class ExporterUtility {
    
    /** Hide constructor.
     */
    private ExporterUtility() {
    }
    
    /** Creates a HashMap where each value that is null is replaced by the String '@null'.
     * 
     * @param origValues the Map on which the returned Map is based.
     * @return a newMap with replaced null values.
     */
    public static Map<String, Object> getValuesWithStringNull(final Map<String, Object> origValues) {
        Map<String, Object> values = new HashMap<>();
        if (origValues != null) {
           for (Entry<String, Object> x : origValues.entrySet()) {
               if (x.getValue() == null) {
                   values.put(x.getKey(), "@null");
               } else {
                   values.put(x.getKey(), x.getValue());
               }
           }
        }
        return values;
    }
    
    /** Creates a HashMap where each value that is '@null' is replaced by null.
     * 
     * @param origValues the Map on which the returned Map is based.
     * @return a newMap with replaced '@null' values.
     */
    public static Map<String, Object> getValuesWithNull(final Map<String, Object> origValues) {
        Map<String, Object> values = new HashMap<>();
        if (origValues != null) {
            for (Entry<String, Object> entry : origValues.entrySet()) {
                if ("@null".equals(entry.getValue())) {
                    values.put(entry.getKey(), null);
                } else {
                    values.put(entry.getKey(), entry.getValue());
                }
            }
        }
        return values;
    }
}
