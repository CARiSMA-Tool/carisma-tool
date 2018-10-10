package riskfindergui;

import static org.junit.Assert.*;

import org.junit.BeforeClass;
import org.junit.Test;

public class RiskPatternTest {

	private static RiskPattern rp;
	private static double d;
	
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String s = "name";
		d = 42.0;
		rp = new RiskPattern(s);
	}

	@Test
	public void testSetAndGetScore() {
		rp.setScore(d);
		assertTrue("Score should be 42.0", 42.0 == rp.getScore());
	}

	@Test
	public void testGetPatternString() {
		rp.setScore(d);
		assertEquals("Score should be addes to name", "name (42.0)", rp.getPatternString());
	}

}
