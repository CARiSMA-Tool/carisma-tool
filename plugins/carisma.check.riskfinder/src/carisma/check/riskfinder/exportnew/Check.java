package carisma.check.riskfinder.exportnew;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Observable;

import carisma.check.riskfinder.Activator;
import carisma.core.analysis.AnalysisHost;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import de.fraunhofer.isst.wbmp.matching.Match;
import de.fraunhofer.isst.wbmp.matching.Pair;
import de.fraunhofer.isst.wbmp.matching.Result;
import de.fraunhofer.isst.wbmp.model.Document;
import de.fraunhofer.isst.wbmp.pipeline.Pipeline;
import de.fraunhofer.isst.wbmp.pipeline.def.DefaultEntityFilter;
import de.fraunhofer.isst.wbmp.pipeline.def.DefaultTextFilter;
import de.fraunhofer.isst.wbmp.pipeline.def.DefaultWordFilter;
import de.fraunhofer.isst.wbmp.pipeline.filter.StopWordSentenceFilter;
import de.fraunhofer.isst.wbmp.tool.Tool;

/**
 * Check to test the new implementation of the riskfinder.
 * @author Klaus Rudack
 *
 */
public class Check extends Observable implements CarismaCheck {

	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters,
			final AnalysisHost host) {
		host.appendLineToReport("Starting the new RiskfinderCheck test ...");
		List<Pair> resultPairs = new ArrayList<Pair>();
		Document processDocument = Activator.processDocument;
		Document patternDocument = Activator.patternDocument;
		Pipeline pipeline = new Pipeline(new DefaultEntityFilter(new DefaultTextFilter(
				new StopWordSentenceFilter(new DefaultWordFilter(), defaultStopWords(), true))));
		Tool tool = new Tool(processDocument, patternDocument, pipeline);
		Result result = tool.execute();
		for (Match matching : result.getMatchings()) {
			for (Pair pair : matching.getPairs()) {
				resultPairs.add(pair);
			}
		}
		host.appendLineToReport("Anzahl matches : " + resultPairs.size());
		return true;
	}
	
	
	
	/** Creates a Dummy Set of defaultStopWords.
	 * 
	 * @return Dummy Set with stopWords.
	 * @author Benjamin Berghoff
	 */
	private static HashSet<String> defaultStopWords() {
		HashSet<String> stopWords = new HashSet<String>();
		stopWords.add("bei");
		stopWords.add("für");
		stopWords.add("ein");
		stopWords.add("der");
		stopWords.add("die");
		stopWords.add("das");
		
		return stopWords;
	}

}
