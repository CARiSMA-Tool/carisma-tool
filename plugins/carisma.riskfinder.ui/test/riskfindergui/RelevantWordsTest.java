package riskfindergui;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.TreeSet;

import org.junit.BeforeClass;
import org.junit.Test;

import carisma.check.riskfinder.PeschkeAnalyser;
import carisma.processanalysis.textmodel.ScoredString;

public class RelevantWordsTest {
	
	private static TreeSet<ScoredString> ts;
	static RelevantWords rw;
	private ArrayList<RiskPattern> patterns = new ArrayList<RiskPattern>();

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		ts = new TreeSet<ScoredString>();
		PeschkeAnalyser pa = new PeschkeAnalyser();
		ScoredString ss = new ScoredString("v1",1);
		ts.add(ss);
		rw = new RelevantWords(ts);
	}

	@Test
	public void testGetTreeSet() {
		assertEquals("ts should be returned", ts, rw.getTreeSet());
	}

	@Test
	public void testAddAndGetAndSetPatterns() {
		RiskPattern p = new RiskPattern("pattern");
		rw.setPatterns(patterns);
		assertEquals("Getter should be equal to Setter", patterns, rw.getPatterns());
		rw.addPatterns(p);
		assertEquals("p should be added", p, rw.getPatterns().get(0));
	}

}
