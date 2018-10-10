package carisma.check.riskfinder.processors;

import java.util.ArrayList;

import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordExtension;
import carisma.processanalysis.textmodel.WordSet;
import carisma.processanalysis.texttools.DomainSpecificTaxonomyExpander;

public class DomainSpecificTaxonomyExpanderProcessor implements StringSetProcessor {
	private boolean synonymsOnly;
	private boolean append;
	private DomainSpecificTaxonomyExpander expander;
	
		public DomainSpecificTaxonomyExpanderProcessor(DomainSpecificTaxonomyExpander expander) {
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
		// TODO: Methode testen!
//		WordSet newStrings = new WordSet();
		for(Word curWord: inputSet){
//			Word tempWord = new Word(curString.getValue());
			ArrayList<WordExtension> result = this.expander.getWordExtensions(curWord, synonymsOnly, append);
//			for(WordExtension curExtension : result)
//				newStrings.add(curExtension);
			curWord.addExtensions(result);
			
			// Anti-Synonyme filtern:
			this.expander.cleanupWord(curWord);
		}
//		inputSet.addAllUniqMaxscore(newStrings);
	}

	@Override
	public String getName() {		
		return "DomainSpecificTaxonomyExpanderProcessor";
	}

}
