package carisma.xutils.regulatory.ui.tests.controller;

import static org.junit.Assert.assertEquals;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.junit.Test;

import carisma.xutils.regulatory.ui.controller.DataController;
import carisma.xutils.regulatory.ui.model.RULEELEMENTS;

/**
 * This JUnit test-class test the DataController class.
 * @author Klaus Rudack
 *
 */
public class DataControllerTest {
	
	/**
	 * This test tests the setColor() and getColor() method of the DataController class.
	 */
	@Test
	public final void testSetGetColor() {
		RGB rgb = new RGB(13, 230, 33);
		Color color = new Color(null, rgb);
		DataController dc = new DataController();
		dc.setColor(color, RULEELEMENTS.Role);
		assertEquals(rgb, dc.getColor(RULEELEMENTS.Role));
	}
	
	/**
	 * This test test the addRuleElement() method of the DataController class.
	 */
	@Test
	public final void testAddRuleElement() {
		DataController dc = new DataController();
		assertEquals(0, dc.getRole().size());
		dc.addRuleElement("Some text", 0);
		dc.addRuleElement("Another text", 0);
		assertEquals(2, dc.getRole().size());
		assertEquals(0, dc.getActivity().size());
		dc.addRuleElement("RuleElement 1", 1);
		assertEquals(1, dc.getActivity().size());
		assertEquals(0, dc.getProperty().size());
		dc.addRuleElement("RuleElement 2", 2);
		assertEquals(1, dc.getProperty().size());
		assertEquals(0, dc.getProcess().size());
		dc.addRuleElement("RuleElement 3.1", 3);
		dc.addRuleElement("RuleElement 3.2", 3);
		assertEquals(2, dc.getProcess().size());
		assertEquals(0, dc.getArtifact().size());
		dc.addRuleElement("RuleElement 4.1", 4);
		dc.addRuleElement("RuleElement 4.2", 4);
		dc.addRuleElement("RuleElement 4.3", 4);
		assertEquals(3, dc.getArtifact().size());
		dc.clearRuleElements();
	}
	
	
	/**
	 * This test tests the clearRuleElements() method of the DataController class.
	 */
	@Test
	public final void testClearRuleElements() {
		DataController dc = new DataController();
		dc.clearRuleElements();
		assertEquals(0, dc.getRole().size());
		dc.addRuleElement("Some text", 0);
		assertEquals(1, dc.getRole().size());
		dc.clearRuleElements();
		assertEquals(0, dc.getRole().size());		
	}

}
