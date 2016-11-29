package carisma.tool.evolution;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import carisma.evolution.EvolutionUtility;

/**
 * JUnit test.case for EvolutionUtility.
 * @author Klaus Rudack
 *
 */
public class EvolutionUtilityTest {

	/**
	 * tests the method getNonEvolutionCheck(String id).
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void testGetNonEvolutionCheck() {
		String testCheck = "carisma.evolution.check";
		String expectedResult = "carisma.nonEvolution.check";
		assertEquals(expectedResult, EvolutionUtility.getNonEvolutionCheck(testCheck));
		assertEquals(null, EvolutionUtility.getNonEvolutionCheck("bullshit"));
	}
	
	/**
	 * tests the method getEvolutionCheck(String ID).
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void testGetEvolutionCheck() {
		String expectedResult = "carisma.evolution.check";
		String testCheck = "carisma.nonEvolution.check";
		assertEquals(expectedResult, EvolutionUtility.getEvolutionCheck(testCheck));
		assertEquals(null, EvolutionUtility.getNonEvolutionCheck("bullshit"));
	}
	
}
