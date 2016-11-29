package carisma.tool.evolution.uml2.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import carisma.evolution.uml2.io.datatype.ExporterUtility;


/** Tests for the ExporterUtility class.
 * 
 * @author bberghoff
 *
 */
public class ExporterUtilityTest {
    
    /**
     * String Value.
     */
    private static final String NAME = "name";
    
    /** getValuesWithStringNull(null) should return an empty List.
     * 
     */
    @SuppressWarnings("static-method")
	@Test
    public final void getValuesWithStringNullParameterIsNullTest() {
        assertTrue(ExporterUtility.getValuesWithStringNull(null).isEmpty());
    }
    
    /** getValuesWithStringNull(Map&lt;String, Object&gt;) called which has one entry where the value is null.
     * 
     */
    @SuppressWarnings("static-method")
	@Test 
    public final void getValuesWithStringNull() {
        Map<String, Object> origValues = new HashMap<>();
        origValues.put(NAME, null);
        assertEquals("@null", ExporterUtility.getValuesWithStringNull(origValues).get(NAME));
    }  
    
    /** getValuesWithStringNull(Map&lt;String, Object&gt;) called which has one entry where the value is NotNull.
     * 
     */
    @SuppressWarnings("static-method")
	@Test 
    public final void getValuesWithStringNullNotNullParameter() {
        Map<String, Object> origValues = new HashMap<>();
        Integer i = Integer.valueOf(4);
        origValues.put(NAME, i);
        assertEquals(i, ExporterUtility.getValuesWithStringNull(origValues).get(NAME));
    }
    
    
    /** getValuesWithNull(null) should return an empty List.
     * 
     */
    @SuppressWarnings("static-method")
	@Test
    public final void getValuesWithNullParameterIsNullTest() {
        assertTrue(ExporterUtility.getValuesWithNull(null).isEmpty());
    }
    
    /** getValuesWithStringNull(Map&lt;String, Object&gt;) called which has one entry where the value is null.
     * 
     */
    @SuppressWarnings("static-method")
	@Test 
    public final void getValuesWithNull() {
        Map<String, Object> origValues = new HashMap<>();
        origValues.put(NAME, "@null");
        assertEquals(null, ExporterUtility.getValuesWithNull(origValues).get(NAME));
    }  
    
    /** getValuesWithStringNull(Map&lt;String, Object&gt;) called which has one entry where the value is NotNull.
     * 
     */
    @SuppressWarnings("static-method")
	@Test 
    public final void getValuesWithNullNotStringNullParameter() {
        Map<String, Object> origValues = new HashMap<>();
        Integer i = Integer.valueOf(4);
        origValues.put(NAME, i);
        assertEquals(i, ExporterUtility.getValuesWithNull(origValues).get(NAME));
    }
    
}
