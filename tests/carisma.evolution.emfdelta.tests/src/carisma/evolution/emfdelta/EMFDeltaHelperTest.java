package carisma.evolution.emfdelta;

import static org.junit.Assert.*;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.compare.match.metamodel.MatchElement;
import org.eclipse.emf.ecore.EObject;
import org.junit.Test;

import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.DeltaElement;

/**
 * Tests the methods of the class carisma.evolution.emfdelta.EMFDeltaHelper.java.
 * @author jkowald
 *
 */
public class EMFDeltaHelperTest {
	
	private String modeldir = "";

	/**
	 * This case tests the methods setAnalysisModeByFilename and getAnalysisMode of the
	 * static class EMFDeltaHelper.
	 */
	@Test
	public void analysisModeTests() {
		File file_bpmn = new File(modeldir + File.separator + "addition_ref.bpmn2");
		EMFDeltaHelper.setAnalysisModeByFilename(file_bpmn);
		assertEquals(EMFDeltaHelper.getAnalysisMode(), EMFDeltaHelper.ANALYSIS_MODE_BPMN);
		
		File file_uml = new File(modeldir + File.separator + "classd_substitution_ref.uml");
		EMFDeltaHelper.setAnalysisModeByFilename(file_uml);
		assertEquals(EMFDeltaHelper.getAnalysisMode(), EMFDeltaHelper.ANALYSIS_MODE_UML);
		
		File file_xml = new File(modeldir + File.separator + "sample.xml");
		EMFDeltaHelper.setAnalysisModeByFilename(file_xml);
		assertEquals(EMFDeltaHelper.getAnalysisMode(), EMFDeltaHelper.ANALYSIS_MODE_UNKNOWN);
	}
	
	/**
	 * This case tests the method getChangedModelElement when the MatchElementList
	 * is null.
	 */
	@Test
	public void matchElementListNullTests() {
		// Test with list = null
		try {
			EMFDeltaHelper.getChangedModelElement(null);
			fail("getChangedModelElement should throw MatchElementsUnsetException before reached here");
		} catch (MatchElementsUnsetException e1) {
			assertTrue(true);
		}
	}
	
	/**
	 * This case tests the method setActualMatchElementList and getChangedModelElement
	 * when the MatchElementList is empty.
	 */
	@Test
	public void matchElementListEmptyTests() {	
		try {
			List<MatchElement> emptyMatchElementList = new ArrayList<MatchElement>();
			EMFDeltaHelper.setActualMatchElementList(emptyMatchElementList);
			EObject result = EMFDeltaHelper.getChangedModelElement(null);
			assertTrue(result == null);
		} catch (MatchElementsUnsetException e2) {
			fail("The list of MatchElements should not be null but empty");
			e2.printStackTrace();
		}
	}
	
	/**
	 * This case tests the methods setActualMatchElementList, getReferenceModelElement,
	 * getReferenceModelSubElement, getChangedModelElement and getChangedModelSubElement
	 * of the static class EMFDeltaHelper.
	 */
	@Test
	public void matchElementListTests() {	
		// Test with a not empty list
		List<Change> changeList	= TestHelper.quickEMFDelta("addition_ref.bpmn2", "addition_cha.bpmn2");
		assertTrue(changeList.size() > 0);
		boolean elementFound = false;
		for (Change c : changeList) {
			assertTrue(c.getAlternatives() != null);
			assertTrue(c.getAlternatives().size() > 0);
			for (Alternative a : c.getAlternatives()) {
				assertTrue(a.getDeltaElements() != null);
				assertTrue(a.getDeltaElements().size() > 0);
				for (DeltaElement de : a.getDeltaElements()) {
					if (de instanceof AddElement) {
						AddElement ae = (AddElement) de;
						EObject target = ae.getTarget();
						assertTrue(target != null);
						try {
							EObject elementFromChangedModel = EMFDeltaHelper.getChangedModelElement(target);
							assertTrue(elementFromChangedModel != null);
							EObject elementFromReferenceModel = EMFDeltaHelper.getReferenceModelElement(elementFromChangedModel);
							assertTrue(elementFromReferenceModel.equals(target));
							elementFound = true;
						} catch (MatchElementsUnsetException e3) {
							fail("The list of MatchElements should not be null");
							e3.printStackTrace();
						}
						break;
					}
				}
				if (elementFound) break;
			}
			if (elementFound) break;
		}
		assertTrue(elementFound);
	}
}
