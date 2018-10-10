package carisma.processanalysis.texttools;

import carisma.processanalysis.textmodel.Word;

/**
 * Classes that implement this interface have to be able for getting the base
 * form of words.
 * 
 * @author Christian Wessel
 * 
 */
public interface Normalizer {
	/**
	 * Retrieves the base form for the given word. A base form is e.g. "bottle"
	 * for the given word "bottles".
	 * 
	 * @param word
	 *            for which the base form is needed.
	 * @return the base form for the given word.
	 */
	Word getBaseform(final Word word);
}
