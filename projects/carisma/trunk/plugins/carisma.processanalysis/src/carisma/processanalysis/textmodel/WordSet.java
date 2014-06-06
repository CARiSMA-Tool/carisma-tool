package carisma.processanalysis.textmodel;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.TreeMap;
import java.util.TreeSet;

public class WordSet extends TreeSet<Word> {

	/**
	 * Fügt die ScoredStrings aus der Collection newStrings hinzu. Wenn es eine String mit gleichem value (String)
	 * schon gibt, wird der Eintrag mit dem höheren Score behalten.
	 * @param newStrings
	 */
	// TODO: Methodenname ist nicht wirklich gut
	public void addAllUniqMaxscore(WordSet newStrings) {
		for(Word curNewString : newStrings){
			this.addUniqMaxscore(curNewString);
		}
	}
	
	public boolean containsValueOnlyIgnoreCase(Word string){
		for(Word curWord : this){
			if(curWord.getContent().equalsIgnoreCase(string.getContent()))
				return true;
		}
		return false;
	}
	
	public double getScoreByValueOnlyIgnoreCase(Word string){
		for(Word curString : this){
			if(curString.getContent().equalsIgnoreCase(string.getContent()))
				return curString.getScore();
		}
		return Double.NaN;
	}
	
	public void updateScoreByValueOnlyIgnoreCase(Word string, double newScore){
		Word theString = null;
		for(Word curString : this){
			if(curString.getContent().equalsIgnoreCase(string.getContent())){
				theString = curString;
				break;
			}
		}
//		theString.setScore(newScore);
		// TODO: umstellen!, wie?
	}
	
	public void addUniqMaxscore(Word newString){
		if(this.containsValueOnlyIgnoreCase(newString)){
			double oldScore = this.getScoreByValueOnlyIgnoreCase(newString);
			double newScore = Math.max(oldScore, newString.getScore());
			this.updateScoreByValueOnlyIgnoreCase(newString, newScore);
		} else {
			this.add(newString);
		}
	}
	
	/**
	 * Alle enthaltenen Words und deren Extensions (!) rekursiv als Liste
	 * @return
	 */
	public WordSet getAsListUniqMaxscore(){
		WordSet ret = new WordSet();
		
		for(Word curWord : this)
			ret.addAllUniqMaxscore(curWord.asList());
		
		return ret;		
	}
}
