package riskfindergui;

import java.util.ArrayList;
import java.util.TreeSet;

import carisma.processanalysis.textmodel.ScoredString;


/**
 * This class represents the relevant words which were found for an element.
 * A RelevantWords object has a list of found patterns. 
 * @author meier
 *
 */
public class RelevantWords {

	private TreeSet<ScoredString> treeSet;
	private ArrayList<RiskPattern> patterns = new ArrayList<RiskPattern>();



	public RelevantWords(TreeSet<ScoredString> ts) {
		treeSet = ts;
	}
	
	public TreeSet<ScoredString> getTreeSet() {
		return treeSet;
	}
	
	public void addPatterns(RiskPattern p) {
		patterns.add(p);
	}
	
	public void setPatterns(ArrayList<RiskPattern> p) {
		this.patterns = p;
	}
	
	public ArrayList<RiskPattern> getPatterns() {
		return patterns;
	}
	
}