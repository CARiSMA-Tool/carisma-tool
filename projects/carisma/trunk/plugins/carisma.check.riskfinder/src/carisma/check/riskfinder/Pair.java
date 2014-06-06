package carisma.check.riskfinder;

import carisma.processanalysis.textmodel.Word;

public class Pair implements Comparable<Pair>{

	public Pair(Word patternWord, Word processWord) {
		super();
		this.patternWord = patternWord;
		this.processWord = processWord;
	}

	private Word patternWord;
	private Word processWord;

	public Word getPatternWord() {
		return patternWord;
	}

	public void setPatternWord(Word patternWord) {
		this.patternWord = patternWord;
	}

	public Word getProcessWord() {
		return processWord;
	}

	public void setProcessWord(Word processWord) {
		this.processWord = processWord;
	}

	public double getScore(){
		return patternWord.getScore()*processWord.getScore()/100;
	}

	@Override
	public int compareTo(Pair o) {
		if(this.patternWord.compareTo(o.patternWord) != 0)
			return this.patternWord.compareTo(o.patternWord);
		else
			return this.processWord.compareTo(o.processWord);
	}
	
	@Override
	public String toString() {
		return "(" + this.patternWord.getContent() + "," + this.processWord.getContent() + ")";
	}

}
