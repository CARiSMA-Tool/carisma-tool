package riskfindergui;

import java.util.ArrayList;
import java.util.List;

/**
 * this class represents a single activity. it has a list of its relevant words
 * as an attribute
 */
public class RiskActivity {

	/**
	 * the name of the activity.
	 */
	private String name;
	
	/**
	 * the relevant words for this activity.
	 */
	private ArrayList<RelevantWords> relWords = new ArrayList<RelevantWords>();
	
	/**
	 * the score of this activity.
	 */
	private double score = 0;

	/**
	 * constructor.
	 * @param name the name of this activity.
	 */
	public RiskActivity(final String name) {
		this.name = name;
	}
	
	/**
	 * return the score of this activity.
	 * @return the score of this activity
	 */
	public final double getScore() {
		return score;
	}

	/**
	 * Sets the score for this entity to the maximum of every pattern score.
	 */
	public final void setScoreMax() {
		double maxScore = 0;
		for (RiskPattern pat : this.relWords.get(0).getPatterns()) {
			if (pat.getScore() > maxScore) {
				maxScore = pat.getScore();
			}
		}
		this.score = maxScore;
	}

	/**
	 * sets the score for this activity.
	 * @param score the new score
	 */
	public final void setScore(final double score) {
		this.score = score;
	}

	/**
	 * gets the name of this activity.
	 * @return the name of this activity
	 */
	public final String getName() {
		return name;
	}

	/**
	 * gets the relevant words for this activity.
	 * @return the relevant words for this activity
	 */
	public final List<RelevantWords> getRelWords() {
		return relWords;
	}
	
	/**
	 * sets the relevant words for this activity.
	 * @param rw the relevant words for this activity
	 */
	public final void setRelWords(final ArrayList<RelevantWords> rw) {
		this.relWords = rw;
	}

	/**
	 * adds new relevant words to the existing relevant words of this activity.
	 * @param relWord new relevant words for this activity
	 */
	public final void addRelWords(final RelevantWords relWord) {
		relWords.add(relWord);
	}

}