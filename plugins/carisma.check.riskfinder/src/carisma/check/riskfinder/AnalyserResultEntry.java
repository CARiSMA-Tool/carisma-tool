package carisma.check.riskfinder;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import carisma.processanalysis.textmodel.Word;

//import carisma.processanalysis.textmodel.ScoredString;


/**
 * A single entry for the output of a security analysis.
 * Repräsentiert die Verbindung zwischen einer ProcessEntity(Aktivität) und einem Pattern
 * @author thumberg
 *
 */
public class AnalyserResultEntry implements Comparable<AnalyserResultEntry>, Comparator<AnalyserResultEntry>{
	/** The pattern that caused the result. **/
	private Pattern pattern;

	TreeSet<Pair> matchingPairs;
	
	/** A set of words that were indentified to be security-relevant. **/	
	// obsolet, wird direkt aus der liste der pairs generiert
//	private HashSet<ScoredString> relevantWords; 
	
	
	/** The score gives the relevance for the current analysis. **/
//	private double score;
	
	/**
	 * Nicht benutzen, nur fuer comparator interface
	 */
	public AnalyserResultEntry() {
		
	}
	
	/**
	 * Constructor for AnalyserResultEntry .
	 * @param pattern Relevant pattern.
	 * @param relevantWords Set of relevant words.
	 */
	public AnalyserResultEntry(final Pattern pattern, final Collection<Pair> pairs) {
		super();
		this.pattern = pattern;
//		this.relevantWords = new HashSet<ScoredString>(relevantWords);
//		this.score = Double.NaN;
		this.matchingPairs = new TreeSet<Pair>(pairs);
	}

	public double getScore(){
		double ret=0;
		for(Pair curPair : this.matchingPairs) {
			ret += curPair.getScore();
		}
		return ret;
	}
	
	
	/**
	 * Get the pattern that caused the entry.
	 * @return The relevant entry.
	 */
	public final Pattern getPattern() {
		return pattern;
	}

	/**
	 * Set the relevant pattern.
	 * @param pattern The pattern that caused the entry.
	 */
	public final void setPattern(final Pattern pattern) {
		this.pattern = pattern;
	}

//	public double getScore() {
//		return score;
//	}
//
//	public void setScore(double score) {
//		this.score = score;
//	}

	
	// TODO: würde die Rückgabe von Pairs mehr Sinn machen?
	
	/**
	 * A set of relevant words from the pattern.
	 * @return The set of relevant words.
	 */
	public final HashSet<Word> getRelevantWords() {
		HashSet<Word> ret = new HashSet<Word>();
		
		for(Pair curPair : this.matchingPairs){
			if(curPair.getScore() > 0)
				ret.add(curPair.getProcessWord()); //TODO: oder PatternWord?
		}
		
		return ret;
	}

	/**
	 * Sets the relevant words.
	 * @param relevantWords A set of words.
	 */
//	public final void setRelevantWords(final HashSet<ScoredString> relevantWords) {
//		this.relevantWords = relevantWords;
//	}
//	
//	/**
//	 * Add a single relevant words to the collection.
//	 * @param word A singe relevant word.
//	 */
//	public final void addRelevantWord(final ScoredString word) {
//		this.relevantWords.add(word);
//	}

	@Override
	public final String toString() {
		return "TODO: Implementieren: ARE.toString"; //TODO: implementieren
		//this.pattern.toString() + " : " + this.relevantWords.toString();
	}

	//TODO: auf Sinnigkeit pruefen
	// inverse order!
	@Override
	public int compareTo(AnalyserResultEntry o) {
		if(this.getScore() != o.getScore())
			return - Double.compare(this.getScore(), o.getScore());
		else
			return this.pattern.compareTo(o.getPattern());
	}

	// inverse order!
	@Override
	public int compare(AnalyserResultEntry o1, AnalyserResultEntry o2) {
		if((new Double(o1.getScore()).compareTo(o2.getScore())) != 0)
			return - (new Double(o1.getScore()).compareTo(o2.getScore()));
		else
			return o1.getPattern().compareTo(o2.getPattern());		
	}

	public final String toXMLString() {
		String ret = "";
		
		ret += "    <ResultEntry ";
		ret += "ID=\"" + this.getPattern().getName().getEntityText()
				.replace("\"", "&quot;");
		ret += "\" title=\"" + this.getPattern().getTitle()
				.getEntityText().replace("\"", "&quot;");
		ret += "\" score=\"" + this.getScore();
		ret += "\" />\n";

		return ret;
	}

	public String getPairsList() {
		String ret = "";
		
		for(Pair curPair : this.matchingPairs){
			ret += "," + curPair;
		}
		
		return ret.substring(1);
	}
	
}
