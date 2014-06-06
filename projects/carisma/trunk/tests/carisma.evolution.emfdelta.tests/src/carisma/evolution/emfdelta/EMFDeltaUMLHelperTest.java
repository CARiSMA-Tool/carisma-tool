package carisma.evolution.emfdelta;

import static org.junit.Assert.*;

import java.io.File;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EcoreFactory;
import org.junit.Test;


/**
 * Tests the methods of the class carisma.evolution.emfdelta.EMFDeltaUMLHelper.java.
 * @author jkowald
 *
 */
public class EMFDeltaUMLHelperTest {
	
	/**
	 * This case tests the method convertToStereotypeIfSuitable.
	 */
	@Test
	public void convertToStereotypeIfSuitableTest() {
		// Tests converting element = null
		assertEquals(null,EMFDeltaUMLHelper.convertToStereotypeIfSuitable(null));
		
		// Tests converting element not instance of org.eclipse.uml2.uml.Stereotype
		EMFDeltaHelper.setAnalysisModeByFilename(new File("resources/models/stereotype_tagvalue_ref.uml"));
		EObject noStereotype = EcoreFactory.eINSTANCE.createEObject();
		EObject result = EMFDeltaUMLHelper.convertToStereotypeIfSuitable(noStereotype);
		assertFalse(result instanceof org.eclipse.uml2.uml.Stereotype);
		
		// Tests converting element instance of org.eclipse.uml2.uml.Stereotype
		EObject element = TestHelper.quickSecrecyStereotypeFromComparison("stereotype_tagvalue_ref.uml", "stereotype_tagvalue_cha.uml");
		assertEquals(EMFDeltaHelper.getAnalysisMode(), EMFDeltaHelper.ANALYSIS_MODE_UML);
		EObject stereotype = EMFDeltaUMLHelper.convertToStereotypeIfSuitable(element);
		assertTrue(stereotype instanceof org.eclipse.uml2.uml.Stereotype);
	}
	
	/**
	 * This case tests the method convertToStereotypeApplicationIfSuitable.
	 */
	@Test
	public void convertToStereotypeApplicationIfSuitableTest() {
		// Tests converting element = null
		assertEquals(null,EMFDeltaUMLHelper.convertToStereotypeApplicationIfSuitable(null));
		
		// Tests converting element instance of org.eclipse.uml2.uml.Stereotype
		EObject element = TestHelper.quickSecrecyStereotypeFromComparison("stereotype_tagvalue_ref.uml", "stereotype_tagvalue_cha.uml");
		assertEquals(EMFDeltaHelper.getAnalysisMode(), EMFDeltaHelper.ANALYSIS_MODE_UML);
		EObject stereotypeApplication = EMFDeltaUMLHelper.convertToStereotypeApplicationIfSuitable(element);
		assertTrue(stereotypeApplication instanceof carisma.modeltype.uml2.StereotypeApplication);
	}
	
	/**
	 * This case tests the method isStereotype after using convertToStereotypeIfSuitable.
	 */
	@Test
	public void isStereotypeTest() {
		EObject element = TestHelper.quickSecrecyStereotypeFromComparison("stereotype_tagvalue_ref.uml", "stereotype_tagvalue_cha.uml");
		assertEquals(EMFDeltaHelper.getAnalysisMode(), EMFDeltaHelper.ANALYSIS_MODE_UML);
		EObject stereotype = EMFDeltaUMLHelper.convertToStereotypeIfSuitable(element);
		assertTrue(EMFDeltaUMLHelper.isStereotype(stereotype));
	}
	
	/**
	 * This case tests the method isStereotypeApplication after using convertToStereotypeApplicationIfSuitable.
	 */
	@Test
	public void isStereotypeApplicationTest() {
		EObject element = TestHelper.quickSecrecyStereotypeFromComparison("stereotype_tagvalue_ref.uml", "stereotype_tagvalue_cha.uml");
		assertEquals(EMFDeltaHelper.getAnalysisMode(), EMFDeltaHelper.ANALYSIS_MODE_UML);
		EObject stereotypeApplication = EMFDeltaUMLHelper.convertToStereotypeApplicationIfSuitable(element);
		assertTrue(EMFDeltaUMLHelper.isStereotypeApplication(stereotypeApplication));
	}
	
	/**
	 * This case tests the method findAddedStereotypeTarget.
	 */
	@Test
	public void findAddedStereotypeTargetTest() {
		// Tests to find target for element = null
		assertEquals(null,EMFDeltaUMLHelper.findAddedStereotypeTarget(null));
		
		// Tests to find target for element instance of org.eclipse.uml2.uml.Stereotype
		EObject element = TestHelper.quickSecrecyStereotypeFromComparison("stereotype_tagvalue_ref.uml", "stereotype_tagvalue_cha.uml");
		assertEquals(EMFDeltaHelper.getAnalysisMode(), EMFDeltaHelper.ANALYSIS_MODE_UML);
		EObject target = EMFDeltaUMLHelper.findAddedStereotypeTarget(element);
		assertTrue(target != null);
		assertTrue(target instanceof org.eclipse.uml2.uml.Element);
	}
	
	/**
	 * This case tests the method findStereotypeParent.
	 */
	@Test
	public void findStereotypeParentTest() {
		// Tests to find target for element = null
		assertEquals(null,EMFDeltaUMLHelper.findStereotypeParent(null));
		
		// Tests to find target for element instance of org.eclipse.uml2.uml.Stereotype
		EObject element = TestHelper.quickSecrecyStereotypeFromComparison("stereotype_tagvalue_ref.uml", "stereotype_tagvalue_cha.uml");
		assertEquals(EMFDeltaHelper.getAnalysisMode(), EMFDeltaHelper.ANALYSIS_MODE_UML);
		EObject target = EMFDeltaUMLHelper.findStereotypeParent(element);
		assertTrue(target != null);
		assertTrue(target instanceof org.eclipse.uml2.uml.Element);
	}
	
	
}
