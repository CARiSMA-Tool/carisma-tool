package carisma.processanalysis.textmodel;

import java.util.LinkedList;

/**
 * This class saves the text and the extension type for an word that is found in
 * a text of a entity.
 * 
 * @author Christian Wessel
 * 
 */
public class WordExtension extends Word {

	private Word parent;
	
	/**
	 * Standard constructor, stores no extension.
	 */
	public WordExtension() {
		this.parent = null;
	}
	/**
	 * Constructor that takes the extending word an its type.
	 * @param word The word that is the extension to a given word.
	 * @param kind The type of the extension.
	 */
	public WordExtension(final String word, final WordKind kind) {
		this.setContent(word);
		this.setKind(kind);
		this.parent = null;
	}
	
	public Word getParent() {
		return parent;
	}
	public void setParent(Word parent) {
		this.parent = parent;
	}
	
	@Override
	public LinkedList<Word> getWordsOnPathToRoot() {
		LinkedList<Word> ret = new LinkedList<Word>();
		ret.add(this);
		if(this.parent != null)
			ret.addAll(this.parent.getWordsOnPathToRoot());
		return ret;
	}	

}
