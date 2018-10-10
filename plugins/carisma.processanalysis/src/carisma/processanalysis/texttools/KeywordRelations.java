package carisma.processanalysis.texttools;

import java.util.Collection;
import java.util.LinkedList;
import java.util.TreeSet;

/**
 * Enthaelt Beziehung eines Keywords zu den direkten Spezialisierungen
 * @author thumberg
 * 
 */
public class KeywordRelations {
	private String keyword;

	// jeweils nur direkte Vorgaenger und Nachfolger:
	private TreeSet<String> parents;
	private TreeSet<String> children;
	private TreeSet<String> synomyms;
	
	public KeywordRelations(String keyword, TreeSet<String> parents, TreeSet<String> successors, TreeSet<String> synonyms) {
		super();
		this.keyword = keyword;
		this.parents = parents;
		this.children = successors;
		this.synomyms = synonyms;
	}

	public KeywordRelations(String keyword, String... successors) {
		super();
		this.keyword = keyword;
		this.parents = new TreeSet<String>();
		this.children = new TreeSet<String>();
		this.synomyms = new TreeSet<String>();
		for(String curChild : successors)
			this.children.add(curChild);
	}
	
	public KeywordRelations(String keyword, Collection<String> children) {
		this.keyword = keyword;
		this.parents = new TreeSet<String>();
		this.children = new TreeSet<String>(children);
		this.synomyms = new TreeSet<String>();
	}

	// So eine Art Factoryfunktion fuer Relations, die nur Synonyme enthalten
	// Ersatz fuer Konstruktor, weil es diese Methodensignatur schon gibt.
	//Stil: naja
	public static KeywordRelations getSynonymsRelation(String keyword, Collection<String> synonyms) {
		KeywordRelations ret = new KeywordRelations(keyword);
		ret.getSynomyms().addAll(synonyms);
//		for (String curSynonym : synonyms) {
//			ret.getSynomyms().add(curSynonym);
//		}
		return ret;		
	}

	public static KeywordRelations getSynonymsRelation(String keyword, String...  synonyms) {
		KeywordRelations ret = new KeywordRelations(keyword);
		for (String curSynonym : synonyms) {
			ret.getSynomyms().add(curSynonym);
		}
		return ret;		
	}

	
	
	public TreeSet<String> getSynomyms() {
		return synomyms;
	}

	public void setSynomyms(TreeSet<String> synomyms) {
		this.synomyms = synomyms;
	}

	public String getKeyword() {
		return keyword;
	}

	public void setKeyword(String keyword) {
		this.keyword = keyword;
	}

	public TreeSet<String> getChildren() {
		return children;
	}

	public void setChildren(TreeSet<String> children) {
		this.children = children;
	}

	public TreeSet<String> getParents() {
		return parents;
	}

	public void setParents(TreeSet<String> parents) {
		this.parents= parents;
	}

	@Override
	public String toString() {
		String ret = "(" + this.parents + " -> \"" + this.keyword;
		if(this.synomyms.size() > 0){
			ret += "(=" + this.synomyms + ")";
		}		
		ret += "\" -> " + this.children + ")";
		return  ret;
	}
	

}
