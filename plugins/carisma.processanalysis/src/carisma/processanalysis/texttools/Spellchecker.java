package carisma.processanalysis.texttools;

import java.util.ArrayList;

import carisma.processanalysis.textmodel.Word;


/**
 * Classes that implement this interface must provide the method checkSpelling.
 * Thus they are also able to provide a list of possible candidates for
 * misspelled words.
 * 
 * @author wessel
 * 
 */
public interface Spellchecker {
	/**
	 * Checks the spelling of a given word.
	 * 
	 * @param word
	 *            The word that has to be checked.
	 * @return true if the word is spelled correctly, false otherwise.
	 */
	boolean isMisspelled(Word word);

	/**
	 * Provides a list of correctly spelled words, which are candidates for the
	 * given misspelled word.
	 * 
	 * @param word
	 *            for which candidates are to be retrieved
	 * @return an ArrayList of correctly spelled candidates
	 */
	ArrayList<String> getCorrectSpelledCandidates(Word word);
}
