package carisma.check.processanalysis.texttools.wortschatz;

import java.io.Serializable;
import java.util.HashMap;

/**
 * this class stands for the storable normalizer cache.
 * 
 * @author dbuerger
 *
 */
public class CacheNormalizer implements Serializable {

		/** the id. */
	private static final long serialVersionUID = -4776295856784602186L;
		/** list of corresponding Strings. */
	private HashMap<String, String> map = null;
	
	/**
	 * Constructor. Initializes the Cache
	 */
	public CacheNormalizer() {
		map = new HashMap<String, String>();
	}
	
	/**
	 * puts a new pair in the Cache.
	 * 
	 * @param key the key of the pair
	 * @param value the value of the pair
	 */
	public final void put(final String key, final String value) {
		if (!this.map.containsKey(key)) {
			this.map.put(key, value);
		}
	}
	
	/**
	 * returns the matching pair if there is one in the cache, otherwise null.
	 * 
	 * @param key the key to search for
	 * @return the pair or null
	 */
	public final String[] getPair(final String key) {
		if (!this.map.containsKey(key)) {
			return null;
		} else {
			return new String[]{key, this.map.get(key)};
		}
	}
	
	/**
	 * returns the matching value if there is one in the cache, otherwise null.
	 * 
	 * @param key the key to search for
	 * @return the value or null
	 */
	public final String getValue(final String key) {
		if (!this.map.containsKey(key)) {
			//TODO: noch mal überdenken, aber erst nach der Demo
			
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
