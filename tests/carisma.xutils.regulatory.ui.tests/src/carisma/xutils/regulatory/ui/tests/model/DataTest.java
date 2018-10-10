package carisma.xutils.regulatory.ui.tests.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.junit.Test;

import carisma.xutils.regulatory.ui.model.Data;
import carisma.xutils.regulatory.ui.model.RULEELEMENTS;

/**
 * This JUnit test-class test the Data class.
 * @author Klaus Rudack
 *
 */
public class DataTest {

	/**
	 * this method test the instance() method of the data class.
	 */
	@Test
	public final void testInstance() {
		Data data = Data.instance();
		assertNotNull(data);
	}
	
	/**
	 * this method tests the methods getColor() and setColor().
	 */
	@Test
	public final void testColor() {
		RGB propertyColor = new RGB(238, 238, 0);
		Data data = Data.instance();
		assertNotNull(data);
		assertEquals(propertyColor, data.getColor(RULEELEMENTS.Property));
		propertyColor = new RGB(178, 38, 35);
		data.setColor(propertyColor, RULEELEMENTS.Property);
		assertEquals(propertyColor, data.getColor(RULEELEMENTS.Property));
	}
	
	/**
	 * this method tests the methods getRole(), addRole() and clearRole().
	 */
	@Test
	public  final void testRole() {
		String newRole = "TestRole";
		Data data = Data.instance();
		assertNotNull(data);
		assertEquals(0, data.getRole().size());
		data.addRole(newRole);
		assertEquals(1, data.getRole().size());
		assertEquals(newRole, data.getRole().get(0));
		data.clearRole();
		assertEquals(0, data.getRole().size());
	}
	
	/**
	 * this method tests if an {@link UnsupportedOperationException} will be thrown
	 * if the a string will be added to the returnvalue of the getRole() method.
	 */
	@Test(expected = UnsupportedOperationException.class)
	public final void testRoleException() {
		List<String> testList;
		Data data = Data.instance();
		assertNotNull(data);
		testList = data.getRole();
		testList.add("NewRole");
	}
	
	/**
	 * this method tests the listToStringArray() method.
	 */
	@Test
	public final void testListToStringArray() {
		List<String> stringList = new ArrayList<String>();
		String[] stringArray = new String[5];
		String[] resultArray;
		Data data = Data.instance();
		assertNotNull(data);
		for (int i = 0; i < 5; i++) {
			stringList.add("Element " + i);
			stringArray[i] = "Element " + i;
		}
		resultArray = data.listToStringArray(stringList);
		assertTrue(Arrays.equals(stringArray, resultArray));
	}
	
}
