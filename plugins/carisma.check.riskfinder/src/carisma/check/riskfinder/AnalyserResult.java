package carisma.check.riskfinder;


import java.util.ArrayList;
import java.util.HashSet;
import java.util.TreeSet;

import carisma.processanalysis.textmodel.ProcessEntity;
import carisma.processanalysis.textmodel.Word;

//import carisma.processanalysis.textmodel.ScoredString;


/**
 * The result of a security analysis.
 * Eine Instanz von AnalyserResult bezieht sich immer auf genau eine ProcessEntity
 * @author thumberg
 *
 */
public class AnalyserResult extends HashSet<AnalyserResultEntry> {
	
	/**
	 * Checkstyle!
	 */
	private static final long serialVersionUID = 6238524086687312899L;
	private final ProcessEntity entity;

	public AnalyserResult(ProcessEntity entity) {
		super();
		this.entity= entity;
	}

	/**
	 * Retrieves the set of all security patterns that were identified as relevant.
	 * @return A set of relevant patterns.
	 */
	public final HashSet<String> getAllRelevantPatternNames() {
		HashSet<String> ret = new HashSet<String>();
		
		for (AnalyserResultEntry curEntry : this) {
			ret.add(curEntry.getPattern().getName().getEntityText());
		}

		return ret;		
	}

	/**
	 * A set of strings, consisting of the identifiers and titles of all identified patterns.
	 * @return A set of identifiers and titles of each relevant pattern.
	 */
	public final HashSet<String> getAllRelevantPatternNamesAndTitles() {
		HashSet<String> ret = new HashSet<String>();
		
		for (AnalyserResultEntry curEntry : this) {
			ret.add(curEntry.getPattern().getName() + " : " + curEntry.getPattern().getTitle());
		}
		
		return ret;		
	}

	/**
	 * Retrieves a set of all words that were identified as security-relevant in any pattern.
	 * @return A set of relevant words from all patterns.
	 */
	public final HashSet<Word> getAllRelevantWords() {
		HashSet<Word> ret = new HashSet<Word>();
		// TODO: Methode testen!
		for (AnalyserResultEntry curEntry : this) {
			ret.addAll(curEntry.getRelevantWords());
		}
		
		return ret;		
	}
	
	public final TreeSet<Word> getAllRelevantWordsSorted() {
		TreeSet<Word> ret = new TreeSet<Word>();
		// TODO: Methode testen!
		for (AnalyserResultEntry curEntry : this) {
			ret.addAll(curEntry.getRelevantWords());
		}
		
		return ret;		
	}
	
	public final ArrayList<AnalyserResultEntry> asSortedList() {
		TreeSet<AnalyserResultEntry> ret = new TreeSet<AnalyserResultEntry>(new AnalyserResultEntry());
		ret.addAll(this);		
		return new ArrayList<AnalyserResultEntry>(ret);
	}
	
	/**
	 * Create a string containing all found patterns, one per line.
	 * All pattern titles are vertically alligned, depending on the length of the pattern IDs.
	 * @param prefix The prefix for every line
	 * @return The resulting string
	 */
	public final String toPrettyString(final String prefix) {
		String ret = "";
		int maxLen = 0;
		for (AnalyserResultEntry curEntry : this) {
			maxLen = Math.max(maxLen, curEntry.getPattern().getName().getEntityText().length());
		}
		
		ArrayList<AnalyserResultEntry> tempList = this.asSortedList();

//		for (AnalyserResultEntry curEntry : this) {
		for (AnalyserResultEntry curEntry : tempList) {
			ret += prefix;
			ret += curEntry.getPattern().getName().getEntityText();
			for (int i = curEntry.getPattern().getName().getEntityText().length(); i < maxLen; i++) {
				ret += " ";
			}
			ret += " : ";
			ret += curEntry.getPattern().getTitle().getEntityText();
			ret += " (" + curEntry.getScore() + ")";
//			ret += " [" + curEntry.getPairsList() + "]";
			ret += "\n";
		}
		return ret;
	}
	
	
	// TODO: reparieren
	public final String toXMLString(){
		String ret = "";
		
		ret += "  <ActivityResult";
		ret += " title=\"" + this.entity.getAllEntityTexts()
				.replace("\"", "&quot;");
		ret += "\" score=\"" + this.getScore_method1(3);
		ret += "\" >\n";
		
		// rel. words:
		for(Word curWord : this.getAllRelevantWordsSorted()){
			ret += curWord.toXMLString();
		}

		// pattern
		for(AnalyserResultEntry curEntry : this.asSortedList()){
			ret += curEntry.toXMLString();
		}
		ret += "  </ActivityResult>\n";
		
		return ret;
	}
	
	public final String toSimpleXMLString(){
		String ret = "";
		for(AnalyserResultEntry curEntry : this){		
			ret += "<match";
			ret += " task=\"" + this.entity.getAllEntityTexts()
					.replace("\"", "&quot;");
			ret += "\" threat=\"" + curEntry.getPattern().getTitle()
					.getEntityText().replace("\"", "&quot;");
			ret += "\" score=\"" + curEntry.getScore();
			ret += "\" />\n";		
		}
		return ret;
	}
	
	/**
	 * Gives an overall Score for this result: Value is the average of the first entryCount AnalyserEntries (if exist)
	 * @param entryCount
	 * @return
	 */
	public final double getScore_method1(int entryCount){
		ArrayList<AnalyserResultEntry> tempList = this.asSortedList();
		
		double sum = 0;
		int count = 0;
		for(AnalyserResultEntry curEntry : tempList){
			sum += curEntry.getScore();
			count ++;
			if(count >= entryCount)
				break;
		}
		
		if(count > 0)
			return sum / count;
		else
			return 0;
	}

	public AnalyserResult getFilteredByMinscore(int minScore) {
		AnalyserResult ret = new AnalyserResult(this.entity);
		
		for(AnalyserResultEntry curEntry : this){
			if(curEntry.getScore() >= minScore)
				ret.add(curEntry);
		}
		
		return ret;
	}

}
