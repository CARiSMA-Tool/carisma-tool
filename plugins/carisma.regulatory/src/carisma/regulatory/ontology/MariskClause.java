package carisma.regulatory.ontology;

import java.util.List;

/**
 * Represents a Marisk Clause in the Ontology.
 * @author dbuerger
 *
 */
public interface MariskClause extends MariskRule{
	
	/**
	 * the number of this clause.
	 * @return the number of the clause
	 */
	public String getNumber();
	
	/**
	 * the text of this clause.
	 * @return the name of the clause
	 */
	public String getTitle();
	
	/**
	 * the entries of this clause.
	 * @return list of Marisk Entries
	 */
	public List<MariskEntry> getEntries();
	
	/**
	 * the sub clauses from this clause.
	 * @return list of Marisk Subclauses
	 */
	public List<MariskClause> getSubclauses();
	
	

}
