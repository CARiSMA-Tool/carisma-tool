package carisma.check.riskfinder.processors;

import java.util.ArrayList;

import carisma.check.processanalysis.texttools.wortschatz.WSExpander;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordExtension;
import carisma.processanalysis.textmodel.WordSet;

public class WSExpanderProcessor implements StringSetProcessor {
	private WSExpander expander;
	private boolean synonymsOnly;
	private boolean append;

	public WSExpanderProcessor(WSExpander expander) {
		super();
		this.expander = expander;
	}

	public boolean isSynonymsOnly() {
		return synonymsOnly;
	}

	public void setSynonymsOnly(boolean synonymsOnly) {
		this.synonymsOnly = synonymsOnly;
	}

	public boolean isAppend() {
		return append;
	}

	public void setAppend(boolean append) {
		this.append = append;
	}

	@Override
	public void process(WordSet inputSet) {
		// TOOD: Methode testen!
//		WordSet newStrings = new WordSet();
		for(Word curWord: inputSet){
//			Word tempWord = new Word(curString.getValue());
			ArrayList<WordExtension> result = this.expander.getWordExtensions(curWord, synonymsOnly, append);
//			for(WordExtension curExtension : result)
//				newStrings.add(curExtension); //.asScoredString());
			curWord.addExtensions(result);
		}
//		inputSet.addAllUniqMaxscore(newStrings);
	}

	@Override
	public String getName() {		
		return "WSExpanderProcessor";
	}

}
