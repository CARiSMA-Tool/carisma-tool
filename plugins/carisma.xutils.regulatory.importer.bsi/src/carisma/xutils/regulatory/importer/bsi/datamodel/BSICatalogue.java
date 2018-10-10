package carisma.xutils.regulatory.importer.bsi.datamodel;

import java.util.HashSet;
import java.util.Set;

public class BSICatalogue /* extends HashSet<BSICategory> */{
	private Set<BSICategory> categories;
	//TODO: Name!

	public BSICatalogue() {
		super();
		this.categories = new HashSet<BSICategory>();
	}
	
	public BSICatalogue(final Set<BSICategory> categories) {
		super();
		this.categories = categories;
	}
	
	public final Set<BSICategory> getCategories() {
		return categories;
	}

	public final void setCategories(final Set<BSICategory> categories) {
		this.categories = categories;
	}

	public final void addCategory(final BSICategory category) {
		this.categories.add(category);
	}

}
