package carisma.processanalysis.texttools;

import java.util.ArrayList;

import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordExtension;


/**
 * Classes that implements this interface must provide a method that retrieves
 * the extensions of a for word. Thus these classes acts as a thesaurus.
 * 
 * @author Christian Wessel
 * 
 */
public interface Expander {
	/**
	 * This method takes a word as parameter and retrieves synonyms and
	 * cooccurent words.
	 * 
	 * @param word
	 *            The word for which extensions have to be retrieved
	 * @param synonymsOnly
	 *            If this parameter is true only synonyms will be searched,
	 *            otherwise also cooccurences will be searched.
	 * @return A List of extensions for the given word
	 */
	ArrayList<WordExtension> getWordExtensions(final Word word, final boolean synonymsOnly);

	/**
	 * This method takes a word as parameter and retrieves synonyms and
	 * cooccurent words.
	 * 
	 * @param word
	 *            The word for which extensions have to be retrieved
	 * @param synonymsOnly
	 *            If this parameter is true only synonyms will be searched,
	 *            otherwise also cooccurences will be searched.
	 * @param append
	 *            If append is set to true, word extensions for the given word
	 *            are appended to the list of extensions otherwise a new list is
	 *            created. An eventually existing list is then discarded.
	 * @return A List of extensions for the given word
	 */
	ArrayList<WordExtension> getWordExtensions(final Word word, final boolean synonymsOnly, final boolean append);
}
