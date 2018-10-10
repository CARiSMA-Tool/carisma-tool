package riskfindergui;

import java.util.ArrayList;
import java.util.List;

import carisma.check.riskfinder.*;
//import carisma.check.riskfinder.AnalyserResultEntry;

/**
 * This class creates a model for the contentProvider.
 * 
 * @author meier
 * 
 */
public class ModelController {


	/**
	 * The first loop creates objects which represent one entity and objects
	 * which represent the relevant words that were found for the entity. The
	 * inner loop creates objects which represent the found patterns. At the
	 * end, the scores for the entities are set.
	 * 
	 * @return a list with all RiskActivity objects
	 */
	public static List<RiskActivity> createModel(final ArrayList<String> entityNames,
			final ArrayList<AnalyserResult> resultAr) {
		if (entityNames == null) {
			return new ArrayList<RiskActivity>();
		}
		ArrayList<RiskActivity> riskActivityList = new ArrayList<RiskActivity>();
		ArrayList<AnalyserResultEntry> resultEntries;
		for (int i = 0; i < resultAr.size(); i++) {
			resultEntries = resultAr.get(i).asSortedList();
			// only create elements with found patterns
			if (resultEntries.size() > 0) {
				RiskActivity act = new RiskActivity(entityNames.get(i));
				RelevantWords relWords = new RelevantWords(resultAr.get(i)
						.getAllRelevantWordsSorted());
				for (int j = 0; j < resultEntries.size(); j++) {
					RiskPattern p = new RiskPattern(resultEntries.get(j)
							.getPattern().getName()
							+ ": "
							+ resultEntries.get(j).getPattern().getTitle());
					p.setScore(resultEntries.get(j).getScore());
					relWords.addPatterns(p);
				}
				act.addRelWords(relWords);
				act.setScoreMax();
				riskActivityList.add(act);
			}
		}
		return riskActivityList;
	}

}
