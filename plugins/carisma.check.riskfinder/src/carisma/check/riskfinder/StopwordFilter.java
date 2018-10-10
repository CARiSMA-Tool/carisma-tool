package carisma.check.riskfinder;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import carisma.check.riskfinder.processors.StringSetProcessor;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordExtension;
import carisma.processanalysis.textmodel.WordSet;

/**
 * A class to remove stopwords from a given set of words.
 * @author thumberg
 *
 */
public class StopwordFilter extends HashSet<String> implements StringFilter, StringSetProcessor {
	/**
	 * An UID to avoid checkstyle warnings.
	 */
	private static final long serialVersionUID = -7970216534041546231L;

	/**
	 * L�dt Stopw�rter aus der Datei inFile.
	 * Ein Wort pro Zeile.
	 * @param inFile Quelldatei
	 * @return true, wenn erfolgreich, sonst false
	 */
	public final boolean loadFromFile(final String inFile) {
		BufferedReader in = null;
		try {
//			in = new BufferedReader(new FileReader(inFile));
			in = new BufferedReader(new InputStreamReader(new FileInputStream(inFile), "UTF-8"));
			String zeile = null;
			while ((zeile = in.readLine()) != null) {
				this.add(zeile.trim());
			}
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		} finally {
			try {
				in.close();
			} catch (IOException e) {				
				e.printStackTrace();
			}
		}
		return true;
	}
	
	private boolean isStopword(final Word word) {
		for (String curStopWord : this) {
			if (word.getContent().equalsIgnoreCase(curStopWord)) {
				return true;
			}
		}
		return false;
	}
	
	private void deleteStopwordsFromCollection(final Collection<Word> input){
		HashSet<Word> del = new HashSet<Word>();
		for (Word curPatternWord : input) {
			if(this.isStopword(curPatternWord))
				del.add(curPatternWord);
		}
		input.removeAll(del);		
	}
	
	private void deleteStopwordsFromExtensionList(final Collection<WordExtension> input){
		HashSet<Word> del = new HashSet<Word>();
		for (Word curPatternWord : input) {
			if(this.isStopword(curPatternWord))
				del.add(curPatternWord);
		}
		input.removeAll(del);		
		
		for(Word curWord : input){
			this.deleteStopwordsFromExtensionList(curWord.getExtensions());
		}		
	}	

	@Override
	public final boolean filter(final Collection<Word> input) {
		this.deleteStopwordsFromCollection(input);
		
		for(Word curWord : input){
			this.deleteStopwordsFromExtensionList(curWord.getExtensions());
		}
		
		return true;
	}

	@Override
	public void process(WordSet inputSet) {
		this.filter(inputSet);		
	}

	@Override
	public String getName() {		
		return "StopwordFilter";
	}
	
}
