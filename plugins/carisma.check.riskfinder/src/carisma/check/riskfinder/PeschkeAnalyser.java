package carisma.check.riskfinder;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.TreeSet;

import carisma.check.riskfinder.Pattern.PatternTextType;
import carisma.check.riskfinder.processors.StringSetProcessor;
import carisma.check.riskfinder.processors.StringSetProcessorChain;
import carisma.processanalysis.textmodel.ProcessEntity;
//import carisma.processanalysis.textmodel.ScoredString;
import carisma.processanalysis.textmodel.WordSet;
import carisma.processanalysis.textmodel.Text;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordExtension;
import carisma.processanalysis.textmodel.WordKind;

/**
 * A class to execute the peschke-algorithm.
 * @author thumberg
 *
 */
public class PeschkeAnalyser implements Analyser {
	
	/**
	 * Delivers the words that are contained in both sets of words, comparison is NOT case-sensitive.
	 * @param patternWords A set or words to be compared.
	 * @param processWords Another set or words to be compared.
	 * @return A set of words, that are part of both input-sets.
	 */
	private TreeSet<Pair> getMatchingWords(final WordSet patternWords, final WordSet processWords) {
		TreeSet<Pair> ret = new TreeSet<Pair>();

		for (Word curPatternWord : patternWords) {
			for (Word curProcessWord : processWords) {
				if (curPatternWord.getContent().equalsIgnoreCase(curProcessWord.getContent())) {
//					double score = s2.getScore() * s1.getScore()/100;
					Pair newPair = new Pair(curPatternWord,curProcessWord);
//					ret.add(new ScoredString(curPatternWord.getValue(), score));
					ret.add(newPair);
				}
			}
		}
		return ret;
	}

	@Override
	public final AnalyserResult getRelevantPatterns(final PatternCatalog catalog, final ProcessEntity entity, StringSetProcessorChain processChain, final StringSetProcessorChain patternChain) {
		return this.getRelevantPatterns(catalog, entity, processChain, patternChain, 0);
	}

	@Override
	public final AnalyserResult getRelevantPatterns(final PatternCatalog catalog, final ProcessEntity entity, final StringSetProcessorChain processChain, final StringSetProcessorChain patternChain, final int minScore) {
		AnalyserResult ret = new AnalyserResult(entity);

		WordSet processsEntityWords = this.getWordsFromEntity(entity);
		processChain.processAll(processsEntityWords);
		
		for (Pattern curPattern : catalog) {
			// HashSet<String> patternWords = new
			// HashSet<String>(curPattern.getWordsSetNoStopwords(stopwords));
			WordSet patternWords = curPattern.getWordsSet(PatternTextType.TITLE); //, stopwords));
			patternChain.processAll(patternWords);
			
			WordSet patternWordsList = patternWords.getAsListUniqMaxscore();
			WordSet processWordsList = processsEntityWords.getAsListUniqMaxscore();
			
			TreeSet<Pair> matches = this.getMatchingWords(patternWordsList, processWordsList);

			if (matches.size() > 0) {
				AnalyserResultEntry newEntry = new AnalyserResultEntry(curPattern, matches);
				if(newEntry.getScore() >= minScore)
					ret.add(newEntry);
			}
		}

		return ret;
	}
	
	public WordSet getWordsFromEntity(ProcessEntity entity){
		WordSet ret = new WordSet();

		for (Text curText : entity.getTexts()) {
			for (Word curWord : curText.getWordList()) {
				//ret.add(curWord);
				ret.addUniqMaxscore(curWord); // stimmt das so?

				for (WordExtension curExtension : curWord.getExtensions()) {
//					ret.add(curExtension);
					ret.addUniqMaxscore(curWord); // stimmt das so?
				}
			}
		}
		return ret;		
	}

}
