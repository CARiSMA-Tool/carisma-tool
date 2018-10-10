package riskfindergui;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import org.junit.Before;
import org.junit.Test;

import carisma.check.riskfinder.AnalyserResult;
import carisma.check.riskfinder.AnalyserResultEntry;
import carisma.check.riskfinder.Pattern;
import carisma.check.riskfinder.PeschkeAnalyser;
import carisma.processanalysis.textmodel.ScoredString;

public class ModelControllerTest {

	List<RiskActivity> rl = new ArrayList<RiskActivity>();
	ArrayList<String> entityNames = new ArrayList<String>();
	ArrayList<AnalyserResult> resultAr = new ArrayList<AnalyserResult>();

	/**
	 * Creates a dummy model
	 * @throws Exception
	 */
	@Before
	public void setUp() throws Exception {

		Pattern p1 = new Pattern("bla1", "blu1", "bli1");
		HashSet<ScoredString> s1 = new HashSet<ScoredString>();
		PeschkeAnalyser pa1 = new PeschkeAnalyser();
		ScoredString ss1 = new ScoredString("v11", 1.0);
		s1.add(ss1);
		AnalyserResultEntry are1 = new AnalyserResultEntry(p1, s1);
		AnalyserResult ar1 = new AnalyserResult("");
		ar1.add(are1);

		Pattern p2 = new Pattern("bla2", "blu2", "bli2");
		HashSet<ScoredString> s2 = new HashSet<ScoredString>();
		PeschkeAnalyser pa2 = new PeschkeAnalyser();
		ScoredString ss2 = new ScoredString("v12", 2.0);
		s2.add(ss2);
		AnalyserResultEntry are2 = new AnalyserResultEntry(p2, s2);
		AnalyserResult ar2 = new AnalyserResult("");
		ar2.add(are2);

		Pattern p3 = new Pattern("bla3", "blu3", "bli3");
		HashSet<ScoredString> s3 = new HashSet<ScoredString>();
		PeschkeAnalyser pa3 = new PeschkeAnalyser();
		ScoredString ss3 = new ScoredString("v13", 3.0);
		s3.add(ss3);
		AnalyserResultEntry are3 = new AnalyserResultEntry(p3, s3);
		AnalyserResult ar3 = new AnalyserResult("");
		ar3.add(are3);

		resultAr.add(ar1);
		resultAr.add(ar2);
		resultAr.add(ar3);
		entityNames.add("name1");
		entityNames.add("name2");
		entityNames.add("name3");

		RiskActivity r1 = new RiskActivity("name1");
		RiskActivity r2 = new RiskActivity("name2");
		RiskActivity r3 = new RiskActivity("name3");
		RelevantWords rel1 = new RelevantWords(ar1.getAllRelevantWordsSorted());
		RelevantWords rel2 = new RelevantWords(ar2.getAllRelevantWordsSorted());
		RelevantWords rel3 = new RelevantWords(ar3.getAllRelevantWordsSorted());
		RiskPattern rp1 = new RiskPattern("bla1: blu1");
		RiskPattern rp2 = new RiskPattern("bla2: blu2");
		RiskPattern rp3 = new RiskPattern("bla3: blu3");
		rp1.setScore(1.0);
		rp2.setScore(2.0);
		rp3.setScore(3.0);
		rel1.addPatterns(rp1);
		rel2.addPatterns(rp2);
		rel3.addPatterns(rp3);
		r1.addRelWords(rel1);
		r2.addRelWords(rel2);
		r3.addRelWords(rel3);
		r1.setScoreMax();
		r2.setScoreMax();
		r3.setScoreMax();

		rl.add(r1);
		rl.add(r2);
		rl.add(r3);
	}

	/**
	 * Lets the ModelController create the dummy model
	 * and checks if they are equal
	 */
	@Test
	public void testFillModel() {
		List<RiskActivity> mc = ModelController
				.createModel(entityNames, resultAr);

		for (int i = 0; i < rl.size(); i++) {
			assertEquals(rl.get(i).getName(), mc.get(i).getName());
			assertTrue(rl.get(i).getScore() == mc.get(i).getScore());
			assertEquals(rl.get(i).getRelWords().get(0).getPatterns().get(0)
					.getPatternString(), mc.get(i).getRelWords().get(0)
					.getPatterns().get(0).getPatternString());
			for (int j = 0; j < rl.get(i).getRelWords().get(0).getTreeSet()
					.toArray().length; j++) {
				assertEquals(rl.get(i).getRelWords().get(0).getTreeSet()
						.toArray()[j], mc.get(i).getRelWords().get(0)
						.getTreeSet().toArray()[j]);
			}

		}

	}
}
