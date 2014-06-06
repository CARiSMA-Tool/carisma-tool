package carisma.evolution.emfdelta;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.DelElement;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;

/**
 * 
 * @author jkowald
 *
 */
public class EMFDeltaScenarioBPMNTest {
	
	@Test
	public void test() {
		int addcounter = 0;
		int delcounter = 0;
		int editcounter = 0;
		List<Change> changeList	= TestHelper.quickEMFDelta("addition_ref.bpmn2", "addition_cha.bpmn2");
		assertEquals(EMFDeltaHelper.getAnalysisMode(),EMFDeltaHelper.ANALYSIS_MODE_BPMN);
		assertTrue(changeList.size() > 4);
		
		for (Change change : changeList) {
			assertEquals(change.getAlternatives().size(),1);
			for (Alternative alternative : change.getAlternatives()) {
				assertEquals(alternative.getDeltaElements().size(),1);
				for (DeltaElement de : alternative.getDeltaElements()) {
					if (de instanceof AddElement) {
					    addcounter++;
					}
					if (de instanceof DelElement) { 
					    delcounter++;
					}
					if (de instanceof EditElement) {
					    editcounter++;
					}
				}
			}
		}
		assertTrue(addcounter >= 1);
		assertTrue(delcounter >= 1);
		assertTrue(editcounter >= 1);
	}
}
