package carisma.check.processanalysis.texttools.wortschatz;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;

import carisma.processanalysis.textmodel.WordExtension;

/**
 * this class stands for the storable expander cache.
 * 
 * @author dbuerger
 *
 */
public class CacheExpander implements Serializable {

		/** the id. */
	private static final long serialVersionUID = 1070004750681917209L;
		/** the cache. */
	private HashMap<String, ArrayList<WordExtension>> map = null;
	
	/**
	 * Constructor. Initializes the cache
	 */
	public CacheExpander() {
		map = new HashMap<String, ArrayList<WordExtension>>();
	}
	
	/**
	 * puts a new pair on the cache.
	 * 
	 * @param key the key of the pair
	 * @param value the value of the pair
	 */
	public final void put(final String key, final ArrayList<WordExtension> value) {
		if (!this.map.containsKey(key)) {
			this.map.put(key, value);
		}
	}
	
	/**
	 * returns the matching list.
	 * 
	 * @param key the key to search for
	 * @return the list
	 */
	public final ArrayList<WordExtension> getList(final String key) {
		if (!this.map.containsKey(key)) {
			return null;
		} else {
			return this.map.get(key);
		}
	}

	/**
	 * clears the cache.
	 */
	public final void clear() {
		this.map.clear();
	}
	
	public final int getSize(){
		return this.map.size();
	}
}
