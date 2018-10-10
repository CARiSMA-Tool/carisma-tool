//package carisma.processanalysis.textmodel;
//
//import java.util.Comparator;
//
//import carisma.processanalysis.textmodel.ScoredString;
//
//public class ScoredString implements Comparable<ScoredString>{
//	private String value;
//	private double score;
//		
//	public ScoredString(String value, double score) {
//		super();
//		this.score = score;
//		this.value = value;
//	}
//
//	public String getValue() {
//		return this.value;
//	}
//
//	public void setValue(String value) {
//		this.value= value;
//	}
//	
//	public double getScore() {
//		return score;
//	}
//
//	public void setScore(double score) {
//		this.score = score;
//	}
//	
//	public String toString(){
//		return this.value + "(" + this.score + ")";
//	}		
//	
//	public final String toXMLString(){
//		String ret = "    <RelevantWord ";
//		ret += "word=\"" + this.getValue();
//		ret += "\" score=\"" + this.getScore();
//		ret += "\" />\n";
//		
//		return ret;
//	}
//
//	//TODO: pr�fen, evtl. Hashsets ersetzen, z.b. treeset
//	public int hashCode() {
//		int ret = (new Integer(this.value.hashCode() * (new Double(this.score)).hashCode())).hashCode();
//		return ret;
//	}
//	
//	public boolean equals(Object obj){
//		return this.score==((ScoredString)obj).getScore() &&
//				this.value.equals(((ScoredString)obj).getValue());
//	}
//
//	// inverse order!		
//	@Override
//	public int compareTo(ScoredString o) {
//		if((new Double(this.getScore()).compareTo(o.getScore())) != 0)
//			return - (new Double(this.getScore()).compareTo(o.getScore()));
//		else
//			return this.getValue().compareTo(o.getValue());
//	}
//
//}
