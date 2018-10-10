package carisma.check.riskfinder;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;

//import carisma.processanalysis.textmodel.ScoredString;
import carisma.processanalysis.textmodel.WordKind;
import carisma.processanalysis.textmodel.WordSet;
import carisma.processanalysis.textmodel.Text;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.texttools.KeywordSet;

/**
 * Klasse fuer ein einzelens Sicherheitspattern.
 * 
 * @author thumberg
 * 
 */

// TODO: Filter-Klasse z.B. fuer Stopwords

public class Pattern implements Comparable<Pattern> {
	
	/**
	 * Enumeration to identify differnent types of text within the pattern.
	 * @author thumberg
	 *
	 */
	public enum PatternTextType {
		/** An identifier that is unique within a catalogue. **/
		NAME,
		/** The title, a short description. **/
		TITLE,
		/** The full text of the pattern. **/
		TEXT
	}
	
	/**
	 * Name/Titel des Pattern. Bei BSI z.B. "BSIGrundschutz_G1.1". Sollte
	 * eindeutig innerhalb eines Kataloges sein.
	 */
	private Text name;
	
	/**
	 * Kurzbeschreibung.
	 * z.B. "Personalausfall"
	 */
	private Text title; 

	/**
	 * Der Text des Pattern, incl. Stoppw�rtern.
	 */
	private Text text;
	
	private KeywordSet keywords; //wird zusaetzlich noch die id benoetigt?

	/**
	 * Menge bekannter Stopw�rter. TODO: 2. F�llen 3. Testen
	 */
//	private StopwordCollection stopwords; doch nicht hier

	/**
	 * Constructor.
	 * 
	 * @param nameP Name des Pattern
	 * @param textP Text des Pattern
	 * @param titleP Title of the Pattern
	 */
	public Pattern(final Text nameP, final Text titleP, final Text textP) {
		super();
		this.name = nameP;
		this.title = titleP;
		this.text = textP;
		this.keywords = new KeywordSet();
		
//		this.stopwords = new StopwordCollection();
		
		//nur zum testen! TODO entfernen
//		this.stopwords.add("ist");
	}

	public Pattern(final Text nameP, final Text titleP, final Text textP, final KeywordSet keywords) {
		super();
		this.name = nameP;
		this.title = titleP;
		this.text = textP;
		this.keywords = keywords;
	}
	
	/**
	 * Rate mal...
	 *
	 * @return Name
	 */
	public final Text getName() {
		return name; //evtl. toString?
	}

	/**
	 * adsf.
	 *
	 * @param nameP Name des Pattern
	 */
	public final void setName(final Text nameP) {
		this.name = nameP;
	}

	/**
	 * Returns the title of the pattern.
	 * @return The title of the pattern.
	 */
	public final Text getTitle() {
		return title;
	}

	/**
	 * Sets the title of the pattern. 
	 * @param title Title to be set.
	 */
	public final void setTitle(final Text title) {
		this.title = title;
	}

	/**
	 * dxfsdasf.
	 *
	 * @return Text
	 */
	public final Text getText() {
		return text;
	}
	
	public final KeywordSet getKeywords() {
		return this.keywords;
	}
	
	public final void setKeywords(final KeywordSet keywords) {
		this.keywords = keywords;
	}

	// TODO: zum Splitten evtl. besser Tokenizer?
	
	/**
	 * Text des Pattern als Array von Einzelw�rtern.
	 * Ergibt sich aus dem Text durch Split mit splitEx.
	 *
	 * @param textType Determines what part of the pattern will be used.
	 * @return Text
	 */
	public final ArrayList<Word> getWords(final PatternTextType textType) {
		Text tempText;
//		WordKind wordKind = WordKind.UNKNOWN;
//		double score = 100;
		switch (textType) {
		case NAME:
			tempText = this.name;
//			wordKind = WordKind.PATTERNNAME;
//			score *= 1.5;
			break;
		case TITLE:
			tempText = this.title;
//			wordKind = WordKind.PATTERNTITLE;
//			score *= 1;
			break;
		case TEXT:
			tempText = this.text;
//			wordKind = WordKind.PATTERNTEXT;
//			score *= .2;
			break;
		default:
			// throw (new java.lang.Exception("Unbekannter TextTyp"));
			tempText = null; // Exception zum debuggen
		}
		
		// Warum noch mal tokenizer?
		
//		StringTokenizer tokenizer = new StringTokenizer(tempText.getEntityText());
//		
//		ArrayList<Word> tempPatternWords = new ArrayList<Word>();
//		while (tokenizer.hasMoreElements()) {
//			// TODO TH hier evtl. schon verschiedene Scores
//			tempPatternWords.add(new Word(tokenizer.nextToken(), wordKind));
//		}
//		
		return tempText.getWordList();
		
//		return tempPatternWords;
	}

	/**
	 * Text des Pattern als Array von Einzelw�rtern.
	 *  Ergibt sich aus dem Text durch Split an Whitespaces und Satzzeichen.
	 *
	 * @return Text
	 */
//	public final String[] getWords() {
//		return this.getWords("[\\W]+");
//	}

	/**
	 * Text des Pattern als Menge von Einzelw�rtern.
	 * Ergibt sich aus dem Text durch Split mit splitEx.
	 *
	 * @param textType Determines what part of the pattern will be used.
	 * @return Text
	 */
	public final WordSet getWordsSet(final PatternTextType textType) {
		WordSet ret = new WordSet();
		ret.addAll(this.getWords(textType));
		// neu fuer keywords:
		ret.addAll(this.keywords);
		
		return ret;
	}

	/**
	 * Text des Pattern als Menge von Einzelw�rtern.
	 *  Ergibt sich aus dem Text durch Split an Whitespaces und Satzzeichen.
	 *
	 * @return Text
	 */
//	public final Set<String> getWordsSet() {
//		return this.getWordsSet("[\\W]+");
//	}
//

//	/**
//	 * Text des Pattern als Menge von Einzelw�rtern, ohne Stopw�rter.
//	 * Ergibt sich aus dem Text durch Split mit splitEx.
//	 *
//	 * @param splitEx
//	 *            Alternative Regex zum Splitten des Textes. Default:
//	 *            Whitespaces und Satzzeichen
//	 * @return Text
//	 */
//	public final Set<String> getWordsSetNoStopwords(final PatternTextType textType, StopwordFilter stopwords) {
//		HashSet<String> tempPatternWords = new HashSet<String>(this.getWords(textType));
//		HashSet<String> ret = new HashSet<String>(tempPatternWords);
//		
////		ret.removeAll(stopwords);
//		
//		for(String curPatternWord : tempPatternWords)
//			for(String curStopWord : stopwords)
//				if(curPatternWord.equalsIgnoreCase(curStopWord))
//					ret.remove(curPatternWord);
//		return ret;
//	}

//	/**
//	 * Text des Pattern als Menge von Einzelw�rtern, ohne Stopw�rter.
//	 *  Ergibt sich aus dem Text durch Split an Whitespaces und Satzzeichen.
//	 *
//	 * @return Text
//	 */
//	public final Set<String> getWordsSetNoStopwords(StopwordCollection stopwords) {
//		return this.getWordsSetNoStopwords("[\\W]+", stopwords);
//	}
//
//	public final Set<String> getTitleWordsSetNoStopwords(StopwordCollection stopwords) {
//		//HashSet<String> tempPatternWords = new HashSet<String>(Arrays.asList(this.title.split("[\\W]+")));
//		
//		HashSet<String> ret = new HashSet<String>(tempPatternWords);
//		
//		for(String curPatternWord : tempPatternWords)
//			for(String curStopWord : stopwords)
//				if(curPatternWord.equalsIgnoreCase(curStopWord))
//					ret.remove(curPatternWord);
//		return ret;
//	}


	/**
	 * Und mit Punkt biem ersten Satz.
	 *
	 * @param textP
	 *            Text
	 */
	public final void setText(final Text textP) {
		this.text = textP;
	}

	@Override
	public final String toString() {
		return "(" + this.name + " : " + this.title + " : " + this.text + ")";
	}


	@Override
	public int compareTo(Pattern o) {		
		return this.getName().getEntityText().compareTo(o.getName().getEntityText());
	}


}
