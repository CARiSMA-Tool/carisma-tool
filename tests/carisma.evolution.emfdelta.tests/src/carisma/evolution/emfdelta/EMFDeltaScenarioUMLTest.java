package carisma.evolution.emfdelta;

import static org.junit.Assert.*;

import java.util.List;

import org.junit.Test;

import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.DelElement;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;
import carisma.evolution.emfdelta.EMFDeltaHelper;

/**
 * 
 * @author jkowald
 *
 */
public class EMFDeltaScenarioUMLTest {

	@Test
	public void test() {
		int addcounter = 0;
		int delcounter = 0;
		int editcounter = 0;
		int substcounter = 0;
		List<Change> changeList	= TestHelper.quickEMFDelta("classd_substitution_ref.uml", "classd_substitution_cha.uml");
		assertEquals(EMFDeltaHelper.getAnalysisMode(),EMFDeltaHelper.ANALYSIS_MODE_UML);		
		assertEquals(changeList.size(),8);
		
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
					if (de instanceof SubstElement) {
					    substcounter++;
					}
				}
			}
		}
		assertEquals(addcounter,2);
		assertEquals(delcounter,2);
		assertEquals(editcounter,3);
		assertEquals(substcounter,1);
	}
}
