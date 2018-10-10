package carisma.processanalysis.textmodel;

import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * This class contains one text of an entity.
 * 
 * @author Christian Wessel
 * 
 */
public class Text {

	/**
	 * Contains all words that could be extracted from a model. The list is
	 * ordered, so that the initial text may be recovered by an in-order
	 * traversal of the list.
	 */
	private String entityText = "";

	/**
	 * Contains the actual kind this text is.
	 */
	private TextKind kind = TextKind.OTHER;

	/**
	 * Contains all words a text can be broken into.
	 */
	private final ArrayList<Word> words = new ArrayList<Word>();

	/**
	 * The default constructor. Does nothing by default
	 */
	public Text() {
	}

	/**
	 * A Constructor taking the text and the kind of the text as input.
	 * 
	 * @param text The text itself, may consist of several words.
	 * @param kind The type of the text.
	 */
	public Text(final String text, final TextKind kind) {
		this.entityText = text;
		this.kind = kind;

		this.tokenize();
	}

	/**
	 * Tokenizes the stored text into words.
	 * TODO TH: hier evtl. anders trennen, um auch Ausdrücke aus mehreren Wörtern berücksichtigen zu können?
	 */
	public void tokenize() {
		if (entityText == null) {
			return;
		}
		
		if (entityText.isEmpty()) {
			return;
		}
			
		words.clear();
		
		WordKind wordKind = WordKind.UNKNOWN;
//		double score = 100;
		switch (this.kind) {
		case PROCESSNAME:
			wordKind = WordKind.SOURCE;
			break;
		case PROCESSDESCRIPTION:
			wordKind = WordKind.SOURCE;
			break;
		case PROCESSCOMMENT:
			wordKind = WordKind.SOURCE;
			break;
		case PATTERNNAME:
			wordKind = WordKind.PATTERNNAME;
			break;
		case PATTERNTITLE:
			wordKind = WordKind.PATTERNTITLE;
			break;
		case PATTERNTEXT:
			wordKind = WordKind.PATTERNTEXT;
			break;
		case OTHER:
			wordKind = WordKind.UNKNOWN;
			break;				
		default:
			wordKind = WordKind.UNKNOWN;
			// throw (new java.lang.Exception("Unbekannter TextTyp"));
		}

		StringTokenizer tokenizer = new StringTokenizer(this.entityText);

		while (tokenizer.hasMoreElements()) {
			String nextToken = tokenizer.nextToken();
			StringTokenizer additionalTokenizer = new StringTokenizer(nextToken,"-:.,;_");
			while(additionalTokenizer.hasMoreElements()){
				String nextToken2 = additionalTokenizer.nextToken();
				// workaround: z.B. eignet sich zerlegt nur bedingt als Ausdruck
				if(nextToken2.length() <= 1)
					continue;
				this.words.add(new Word(nextToken2, wordKind));
			}
		}
	}

	@Override
	public final String toString() {
		String res = "";

		switch (kind) {
		case PROCESSNAME:
		case PATTERNNAME:
			res = "Name: ";
			break;
		case PROCESSDESCRIPTION:
			res = "Description: ";
			break;
		case PROCESSCOMMENT:
			res = "Comment: ";
			break;
		case PATTERNTITLE:
			res = "Title: ";
			break;			
		case PATTERNTEXT:
			res = "Text: ";
			break;
		default:
			res = "Other: ";
			break;
		}

		res += "\"" + this.entityText + "\"";

		return res;
	}

	/**
	 * Returns the text of the entity.
	 * 
	 * @return entityText
	 */
	public final String getEntityText() {
		return entityText;
	}

	/**
	 * Sets the given text of this entity.
	 * 
	 * @param text The text.
	 */
	public final void setEntityText(final String text) {
		this.entityText = text;
		tokenize();
	}

	/**
	 * Returns the type the entity text.
	 * 
	 * @return kind
	 */
	public final TextKind getTextKind() {
		return kind;
	}

	/**
	 * Sets the given entity type as new type of this text.
	 * 
	 * @param kind The type of the text.
	 */
	public final void setTextKind(final TextKind kind) {
		this.kind = kind;
	}

	/**
	 * Returns the list of words of the entity text.
	 * 
	 * @return The seperate words of the stored text.
	 */
	public final ArrayList<Word> getWordList() {
		return words;
	}
}
