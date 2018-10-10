package carisma.check.riskfinder.processors;

import carisma.check.processanalysis.texttools.wortschatz.WSNormalizer;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordSet;

/**
 * @author dbuerger
 * The Class WSNormalizerProcessor.
 */
public class WSNormalizerProcessor implements StringSetProcessor {

	/** The normalizer. */
	private WSNormalizer normalizer = null;

	/**
	 * Instantiates a new wS normalizer processor.
	 *
	 * @param normalizer the normalizer
	 */
	public WSNormalizerProcessor(final WSNormalizer normalizer) {
		this.normalizer = normalizer;
	}

	/* (non-Javadoc)
	 * @see carisma.check.riskfinder.processors.StringSetProcessor#process(carisma.processanalysis.textmodel.ScoredStringSet)
	 */
	@Override
	public final void process(final WordSet inputSet) {
		// TODO: Methode testen!
		for(Word curWord : inputSet){
			processNode(curWord);
		}
	}

	public final void processNode(final Word input) {
		// WordSet newStrings = new WordSet();
		// WordSet noBaseformStrings = new WordSet();
		// Word tempWord = new Word(curString.getValue());
		// TODO do we check misspelled words ??
		// if (this.normalizer.isMisspelled(tempWord)) {
		// correctSpelledWords = this.normalizer
		// .getCorrectSpelledCandidates(tempWord);
		// // TODO How to handle the sum of correct spelled words
		// }
		// TODO TH: statt Word besser WordExtension?
		Word newWord = this.normalizer.getBaseform(input);
		if(newWord.getContent().length() > 0){
			input.setContent(newWord.getContent());
		}
		// TODO: score gibt es nicht mehr explizit -> trotzdem noch korrekt?
		// newString.setScore(curWord.getScore()); // TODO so korrekt?

		for (Word curWord : input.getExtensions()) {
			processNode(curWord);
		}
		// experimentell: Ersetzen statt erg�nzen
		// inputSet.clear();
		// inputSet.addAllUniqMaxscore(noBaseformStrings);
		//
		// inputSet.addAllUniqMaxscore(newStrings);
	}

	@Override
	public String getName() {
		return "WSNormalizerProcessor";
	}

}
