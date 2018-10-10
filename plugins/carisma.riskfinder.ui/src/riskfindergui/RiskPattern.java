package riskfindergui;


/**
 * this class represents one pattern.
 * a pattern has a name and a score.
 */
public class RiskPattern {

	private String patternName;
	private Double score = 0.0;

	public Double getScore() {
		return score;
	}

	public void setScore(Double score) {
		this.score = score;
	}

	public RiskPattern(String s) {
		patternName = s;
	}
	
	public String getPatternString(){
		return patternName+ " (" + score + ")";
	}

}
