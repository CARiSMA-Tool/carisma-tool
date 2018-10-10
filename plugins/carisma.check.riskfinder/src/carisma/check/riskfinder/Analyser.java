package carisma.check.riskfinder;

import java.util.ArrayList;

import carisma.check.riskfinder.processors.StringSetProcessorChain;
import carisma.processanalysis.textmodel.ProcessEntity;


/**
 * An interface for classes that provide a security analysis.
 * @author thumberg
 *
 */
public interface Analyser {
	/**
	 * A function that performs a security analysis. The method itself depends on the implementing class.
	 * @param catalog A collection of security patterns.
	 * @param entity The model to be examined.
	 * @param filters A set of filters that operate on sets of words. The filters will be applied in the order given by the list.
	 * @return The result of the analysis.
	 */
	AnalyserResult getRelevantPatterns(PatternCatalog catalog, ProcessEntity entity, StringSetProcessorChain textChain, StringSetProcessorChain patternChain);

	AnalyserResult getRelevantPatterns(PatternCatalog catalog, ProcessEntity entity, StringSetProcessorChain textChain, StringSetProcessorChain patternChain, int minScore);
}
