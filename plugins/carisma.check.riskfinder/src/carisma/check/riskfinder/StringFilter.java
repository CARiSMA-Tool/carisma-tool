package carisma.check.riskfinder;

import java.util.Collection;

import carisma.processanalysis.textmodel.Word;

/**
 * The interface describes a class that can be used to modify a set of Strings.
 * @author thumberg
 *
 */
public interface StringFilter {
	/**
	 * Modifies the given set of strings by rules to be specified by the implementing class.
	 * @param input A set of strings, will be modified by the function.
	 * @return true, iff the operation was successful.
	 */
	boolean filter(Collection<Word> input);
}
