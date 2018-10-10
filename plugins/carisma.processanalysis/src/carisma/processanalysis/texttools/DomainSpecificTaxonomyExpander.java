package carisma.processanalysis.texttools;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.TreeSet;

import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;
import carisma.processanalysis.textmodel.Text;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordExtension;
import carisma.processanalysis.textmodel.WordKind;

public class DomainSpecificTaxonomyExpander implements Expander {

	private DomainSpecificKeywordTaxonomy dskt;
	
	public DomainSpecificTaxonomyExpander() {
		// Taxonomie einlesen; Datei: aus Homeverzeichnis? -> wie wsexpander
		String taxonomyFilename = System.getProperty("user.home")+System.getProperty("file.separator") + "taxonomy.csv";		
		this.dskt = new DomainSpecificKeywordTaxonomy();
		this.dskt.loadFromCSV(taxonomyFilename, false);		

		String synonymsFilename = System.getProperty("user.home")+System.getProperty("file.separator") + "synonyms.csv";		
		this.dskt.loadSynonymsFromCSV(synonymsFilename);		
		
		String antiSynonymsFilename = System.getProperty("user.home")+System.getProperty("file.separator") + "antisynonyms.csv";
		this.dskt.loadAntiSynonymsFromCSV(antiSynonymsFilename);
	}

	@Override
	public ArrayList<WordExtension> getWordExtensions(Word word, boolean synonymsOnly) {
		return this.getWordExtensions(word, synonymsOnly, false);
	}

	@Override
	public ArrayList<WordExtension> getWordExtensions(Word word, boolean synonymsOnly, boolean append) {
		ArrayList<WordExtension> ret = new ArrayList<WordExtension>();
		
		String curKeyword = word.getContent();
		
		TreeSet<String> newKeywords = dskt.getAllPredecessors(curKeyword);
		
		for(String curNewKeyword : newKeywords){
			ret.add(new WordExtension(curNewKeyword, WordKind.TAXONOMYPREDECESSOR));
		}		
		
		return ret;
	}
	

	/**
	 * Uebernommen aus WSExpander
	 */
	public final boolean calcExtensions(final ProcessEntity entity, final boolean synonymsOnly, final boolean append) {
		boolean res = true;

		for (Text curText : entity.getTexts()) {
			for (Word curWord : curText.getWordList()) {
//				System.out.println("Expand: " + curWord.getContent());
				ArrayList<WordExtension> result = getWordExtensions(curWord, synonymsOnly, append);

				if (result != null) {
					if (append) {
						curWord.addExtensions(result);
					} else {
						curWord.setExtensions(result);
					}
				} else {
					res = false;
				}
			}
		}

		return res;
	}

	/**
	 * Uebernommen aus WSExpander
	 */
	public final boolean calcExtensions(final ProcessDescription procDesc, final boolean synonymsOnly, final boolean append) {
		boolean res = true;

		for (ProcessEntity curEntity : procDesc.getEntities()) {
			res &= calcExtensions(curEntity, synonymsOnly, append);
		}

		return res;
	}
	
	
	/**
	 * Filtert rekursiv die Extensions des übergebenen Words nach Antisynonymen. Diese werden entfernt.
	 * Relativ ungetestet.
	 * @param inputWord
	 */
	public void cleanupWord(Word inputWord){
		Iterator<WordExtension> it = inputWord.getExtensions().iterator();
//		ArrayList<Word> delWords = new ArrayList<Word>();
		while(it.hasNext()){
			WordExtension curExtension = it.next();
			if(this.dskt.forbidsExtension(inputWord.getContent(), curExtension.getContent())){
				it.remove();
//				delWords.add(curExtension);
			}
			cleanupWord(curExtension);
		}
//		inputWord.getExtensions().removeAl(delWords);
	}
	

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
