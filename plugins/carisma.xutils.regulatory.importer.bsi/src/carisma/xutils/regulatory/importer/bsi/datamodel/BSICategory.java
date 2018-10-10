package carisma.xutils.regulatory.importer.bsi.datamodel;

import java.util.HashSet;
import java.util.Set;

public class BSICategory /*extends HashSet<BSIEntry>*/ {
	private String name;
	private String id;
	private String url;
	private Set<BSIEntry> entries;
	
	public BSICategory(final String name, final String id, final String uRL) {
		super();
		this.name = name;
		this.id = id;
		url = uRL;
		this.entries = new HashSet<BSIEntry>();
	}

	/**
	 * Returns the name of the BSICategory.
	 * @return the name of the BSI Category
	 */
	public final String getName() {
		return name;
	}
	
	/**
	 * Sets the name of the BSICategory.
	 * @param name the new name of the BSI Category
	 */
	public final void setName(final String name) {
		this.name = name;
	}
	
	/**
	 * Returns the id of the BSICategory.
	 * @return the id of the BSI Category
	 */
	public final String getId() {
		return id;
	}
	
	/**
	 * Sets the id of the BSICategory.
	 * @param id the new id of the BSI Category
	 */
	public final void setId(final String id) {
		this.id = id;
	}

	public final Set<BSIEntry> getEntries() {
		return entries;
	}

	public final void setEntries(final Set<BSIEntry> entries) {
		this.entries = entries;
	}
	
	public final void addEntry(final BSIEntry entry){
		this.entries.add(entry);
	}
	

	public final String getURL() {
		return url;
	}

	public final void setURL(final String uRL) {
		url = uRL;
	}
}
