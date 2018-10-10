package carisma.processanalysis.tests;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.List;

import org.junit.Test;

import carisma.processanalysis.textmodel.ProcessEntity;
import carisma.processanalysis.textmodel.Text;
import carisma.processanalysis.textmodel.TextKind;

/**
 * JUnit test-cases for the ProcessEntity class.
 * @author Klaus Rudack
 *
 */
public class ProcessEntityTests {

	/**
	 * this test tests the add and get Text methods.
	 */
	@Test
	public final void testAddAndGetText() {
		boolean success = false;
		List<Text> results;
		String type = "test_type";
		String id = "test_id";
		String exampleText1 = "example_text1";
		String exampleText2 = "example_text2";
		String exampleText3 = "example_text3";
		TextKind kind = TextKind.PROCESSCOMMENT;
		ProcessEntity pe = new ProcessEntity(type, id, null);
		results = pe.getTexts();
		assertEquals(0, results.size());
		pe.addText(exampleText1, kind);
		results = pe.getTexts();
		assertEquals(1, results.size());
		pe.addText(exampleText2, kind);
		pe.addText(exampleText3, kind);
		results = pe.getTexts();
		assertEquals(3, results.size());
		for (Text t : results) {
			if ((t.getTextKind() == kind) && (t.getEntityText().equals(exampleText2))) {
				success = true;
			}
		}
		if (!success) {
			fail("Example element with kind = Comment and text = " + exampleText2 + " not been found!");
		}
	}

}
