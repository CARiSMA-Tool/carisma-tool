package carisma.processanalysis.textmodel;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * This class contains words that can be extracted from a text of an entity.
 * 
 * @author Christian Wessel
 * 
 */
public class Word implements Serializable, Comparable<Word> {
	/**
	 * Contains the characters of the word.
	 */
	private String content = "";

	/**
	 * Contains the list of possible extensions for this word.
	 */
	private ArrayList<WordExtension> extensions = new ArrayList<WordExtension>();

//	private double score; // als Ersatz für ScoredString

	/**
	 * The kind of  
	 */
	private WordKind kind = WordKind.UNKNOWN;


	/**
	 * The default constructor.
	 */
	public Word() {
	}

	/**
	 * Defines a constructor, that is initialized with a text.
	 * 
	 * @param text The text of the word.
	 */
	public Word(final String text) {
		this.content = text;
	}

	public Word(String text, WordKind wordKind) {
		this.content = text;
		this.kind = wordKind;
	}

	/**
	 * Returns the the characters of the word as a String. This method does
	 * nothing more than the toString method. Maybe in later versions this
	 * method is of more use.
	 * 
	 * @return text
	 */
	public final String getContent() {
		return content;
	}

	/**
	 * Replaces the given text with any that may be stored in this word.
	 * 
	 * @param text The text of the word.
	 */
	public final void setContent(final String text) {
		this.content = text;
	}

	/**
	 * Gets the type of the extension.
	 * @return The type of the extension.
	 */
	public final WordKind getKind() {
		return kind;
	}

	/**
	 * Sets the type of the extension.
	 * @param kind The type to be set.
	 */
	public final void setKind(final WordKind kind) {
		this.kind = kind;
	}
	
	/**
	 * 
	 * @return true, if this word has a list of extensions (i.e. synonyms or
	 *         coocurrences) or false otherwise.
	 */
	public final boolean hasExtensions() {
		return extensions.size() > 0;
	}

	/**
	 * Returns the list of extensions for this word.
	 * 
	 * @return extensions
	 */
	public final ArrayList<WordExtension> getExtensions() {
		return extensions;
	}

	/**
	 * Sets a manually created list of extensions for this word.
	 * 
	 * @param extensions A set of extensions that is assigned to the Word.
	 */
	public final void setExtensions(final ArrayList<WordExtension> extensions) {
		this.extensions = extensions;
		for(WordExtension curExtension : this.extensions)
			curExtension.setParent(this);
	}

	/**
	 * Adds an single WordExtension to the list of extensions for this word.
	 * 
	 * @param extension A single extension that is added to the Word.
	 */
	public final void addExtension(final WordExtension extension) {
		this.extensions.add(extension);
		extension.setParent(this);
	}

	/**
	 * Sets a manually created list of extensions for this word.
	 * 
	 * @param extensions A set of extensions that are added to the Word, keeping the existing ones.
	 */
	public final void addExtensions(final ArrayList<WordExtension> extensions) {
		this.extensions.addAll(extensions);
		// nur NEUE extensions:
		for(WordExtension curExtension : extensions)
			curExtension.setParent(this);
	}

	/**
	 * Alle Words, die auf dem Pfad zum Ursprungswort liegen, entspr. der parent-Einträge	
	 * @return
	 */
	public LinkedList<Word> getWordsOnPathToRoot(){
		LinkedList<Word> ret = new LinkedList<Word>();
		ret.add(this);
		return ret;
	}

	public String getWordsOnPathToRootAsString() {
		String ret = "";
		LinkedList<Word> path = this.getWordsOnPathToRoot();
		
		//Iterator<Word> it = path.descendingIterator();
		Iterator<Word> it = path.iterator();
		while(it.hasNext()){
			Word curWord = it.next();			
			String connector = " " + curWord.getShortSignForWordKind() + " " ;
			ret +=   " " + curWord + " " + connector.trim();			
		}
				
		return "[" + ret.trim() + "]";
	}
	
	@Override
	public final String toString() {
		return content + "(" + this.getScore() + ")";
	}
	

	/**
	 * sehr unvollständig, angepasst für getWordsOnPathToRootAsString
	 * @return
	 */
	public String getShortSignForWordKind(){
		switch(this.getKind()){
		case SOURCE:
			return "";
		case COOCCURRENCE:
			return "~";		
		case SYNONYME:
			return "=";
		case TAXONOMYPREDECESSOR:
			return "->";
		default:
			return "?";		
		}		
	}
	

	public double getScore() {
		double score = 100;

		switch (this.kind) {
		case SOURCE:
			score *= 5;
			break;
		case SYNONYME:
			score *= 2;
			break;
		case COOCCURRENCE:
			score *= 1;
			break;
		case TAXONOMYPREDECESSOR:
			score *= 3; // evtl. zu hoch?
			break;
		case PATTERNNAME:
			score *= 2;
			break;
		case PATTERNKEYWORD:
			score *= 1.5;
			break;
		case PATTERNTITLE:
			score *= 1;
			break;
		case PATTERNTEXT:
			score *= .2;
			break;
		default:
			score *= 1;
			System.out.println("!!! ExtensionKind ohne Score !!!");
			break;
		}

		return score;

	}

	// TODO: warum werden zuerst die Scores verglichen?
//	// inverse order!	
	@Override
	public int compareTo(Word o) {
		if ((new Double(this.getScore()).compareTo(o.getScore())) != 0)
			return -(new Double(this.getScore()).compareTo(o.getScore()));
		else
			return this.getContent().compareTo(o.getContent());
	}

	public WordSet asList() {
		WordSet ret = new WordSet();
		ret.add(this);
		
		for(Word curExtension : this.extensions)
			ret.addAll(curExtension.asList());
		
		return ret;
	}

	public final String toXMLString(){
		String ret = "    <RelevantWord ";
		ret += "word=\"" + this.getContent();
		ret += "\" score=\"" + this.getScore();
		ret += "\" />\n";
		
		return ret;
	}

}
