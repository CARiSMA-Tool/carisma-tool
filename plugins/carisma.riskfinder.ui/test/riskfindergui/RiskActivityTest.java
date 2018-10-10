package riskfindergui;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.TreeSet;

import org.junit.BeforeClass;
import org.junit.Test;

import carisma.check.riskfinder.PeschkeAnalyser;
import carisma.processanalysis.textmodel.ScoredString;

public class RiskActivityTest {
	
	private static RiskActivity ra;
	private static RelevantWords rw;
	private static ArrayList<RelevantWords> relWords; 

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		relWords = new ArrayList<RelevantWords>();
		ra = new RiskActivity("Test");
		RiskPattern p1 = new RiskPattern("p1");
		RiskPattern p2 = new RiskPattern("p2");
		RiskPattern p3 = new RiskPattern("p3");
		RiskPattern p4 = new RiskPattern("p4");
		p1.setScore(0.0);
		p2.setScore(100.0);
		p3.setScore(-101.0);
		p4.setScore(101.0);
		TreeSet<ScoredString> ts = new TreeSet<ScoredString>();
		PeschkeAnalyser pa = new PeschkeAnalyser();
		ScoredString ss = new ScoredString("v1",1);
		ts.add(ss);
		rw = new RelevantWords(ts);
		rw.addPatterns(p1);
		rw.addPatterns(p2);
		rw.addPatterns(p3);
		rw.addPatterns(p4);
		ra.addRelWords(rw);

	}


	@Test
	public void testSetScoreMaxAndGetScore() {
		ra.setScoreMax();
		assertTrue("Maximum score should be 101.0", ra.getScore() == 101.0);
	}

	@Test
	public void testGetName() {
		assertEquals("Name should be Test", "Test", ra.getName());
	}

	@Test
	public void testGetAndAddAndSetRelWords() {
		ra.setRelWords(relWords);
		assertEquals("Getter should be equal to Setter", relWords, ra.getRelWords());
		ra.addRelWords(rw);
		assertEquals("rw should be added", rw, ra.getRelWords().get(0));
	}


}
