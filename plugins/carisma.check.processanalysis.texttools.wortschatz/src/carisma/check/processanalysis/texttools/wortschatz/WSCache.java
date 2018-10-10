package carisma.check.processanalysis.texttools.wortschatz;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;

import carisma.core.Carisma;
import carisma.processanalysis.textmodel.WordExtension;

/**
 * this class stands for the cache to store frequently used words for 'wortschatz'.
 * 
 * @author dbuerger
 *
 */
public class WSCache {
	
	private static final String PROPERTY_EXPANDER_CACHE_FILENAME = "carisma.check.processanalysis.texttools.wortschatz.cache.expander";
	private static final String PROPERTY_NORMALIZER_CACHE_FILENAME = "carisma.check.processanalysis.texttools.wortschatz.cache.normalizer";
	/** the type of the cache. */
	private WSCacheType type;
	
	
		// flags to switch between the caches
	//TODO: sinnvoll? evtl. besser separate Klassen?
	public enum WSCacheType{
		EXPANDER_CACHE,
		NORMALIZER_CACHE
	}
	
		// the caches
	private CacheExpander expCache = null;
	private CacheNormalizer normCache = null;
	
	
	/**
	 * constructor.
	 * @param flag the type of the cache (e.g. WSCache.EXPANDER_CACHE)
	 * @throws WrongElementException when there is no matching type 
	 */
	public WSCache(final WSCacheType flag) throws WrongElementException {
		this.type = flag;
		Carisma.getInstance().getPreferenceManager().setDefaultPreferenceValue(PROPERTY_EXPANDER_CACHE_FILENAME, System.getProperty("user.home")+System.getProperty("file.separator")+"wsexpandercache.ser");
		Carisma.getInstance().getPreferenceManager().setDefaultPreferenceValue(PROPERTY_NORMALIZER_CACHE_FILENAME, System.getProperty("user.home")+System.getProperty("file.separator")+"wsnormalizercache.ser");
		
		/** to load the file */
		File inFile = null;
		FileInputStream fis = null;
			/** to load the cache */
		ObjectInputStream ois = null;
		try { 
				// determine which kind of cache has to be loaded
			if (this.type == WSCacheType.EXPANDER_CACHE) {
				inFile = new File((String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_EXPANDER_CACHE_FILENAME));
				fis = new FileInputStream(inFile);
				ois = new ObjectInputStream(fis);
				expCache = (CacheExpander) ois.readObject();
				System.out.println("Expander cache succesfully loaded with " + expCache.getSize() + " entries");
			} else if (this.type == WSCacheType.NORMALIZER_CACHE) {
				inFile = new File((String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_NORMALIZER_CACHE_FILENAME)); 
				fis = new FileInputStream(inFile);
				ois = new ObjectInputStream(fis);
				normCache = (CacheNormalizer) ois.readObject();
				System.out.println("Normalizer cache succesfully loaded with " + normCache.getSize() + " entries");
			}
		} catch (FileNotFoundException fne) {
			System.out.println("File " + inFile + " not found ...");
//			System.out.println("Seeked at " + FILE_NAME);
//			System.out.println(System.getProperty("user.dir"));
			createCache();
		} catch (IOException io) {
			System.out.println("Error while loading the file ...");
			System.out.println(io);
			createCache();
		} catch (ClassNotFoundException cne) {
			System.out.println("Cannot find the class definition ...");
			System.out.println(cne);
			createCache();
		}
	}
	
	/**
	 * store the cache.
	 */
	public final void storeCache() {
			/** to write the file */
		FileOutputStream fos = null;
			/** to write the object */
		ObjectOutputStream oos = null;
		try {
				// determine which type of cache has do be stored
			if (type == WSCacheType.EXPANDER_CACHE) {
				fos = new FileOutputStream((String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_EXPANDER_CACHE_FILENAME));
				oos = new ObjectOutputStream(fos);
				oos.writeObject(expCache);
				oos.close();
			} else if (type == WSCacheType.NORMALIZER_CACHE) {
				fos = new FileOutputStream((String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_NORMALIZER_CACHE_FILENAME));
				oos = new ObjectOutputStream(fos);
				oos.writeObject(normCache);
				oos.close();
			}
		} catch (FileNotFoundException fne) {
			System.err.println("File not Found!");
		} catch (IOException io) {
			System.err.println("Error while storing the file!");
			io.printStackTrace();
		}
	}
		
	/**
	 * to put a new element on the normalizer cache.
	 * @param key the key for the element
	 * @param value the value 
	 * @throws WrongElementException when the method is called on the wrong cache
	 */
	public final void putNorm(final String key, final String value) throws WrongElementException {
		if (type != WSCacheType.NORMALIZER_CACHE || normCache == null) {
			throw new WrongElementException("wrong kind of cache. this method only is suitable for normalizer cache");
		}
		normCache.put(key, value);
		
	}
	
	/**
	 * to put a new element on the expander cache.
	 * @param key the key to store the list
	 * @param list the list
	 * @throws WrongElementException when the method is called on the wrong cache
	 */
	public final void putEx(final String key, final ArrayList<WordExtension> list) throws WrongElementException {
		if (type != WSCacheType.EXPANDER_CACHE || expCache == null) {
			throw new WrongElementException("wrong kind of cache. this method only is suitable for expander cache");
		}
		expCache.put(key, list);
		
	}
	
	/**
	 * returns the matching value or null if the key was not found.
	 * @param key the key
	 * @return the value 
	 * @throws WrongElementException 
	 */
	public final String getNorm(final String key) throws WrongElementException {
		if (type != WSCacheType.NORMALIZER_CACHE || normCache == null) {
			throw new WrongElementException("wrong kind of cache. this method only is suitable for normalizer cache");
		}
		return normCache.getValue(key);	
	}
	
	/**
	 * returns the matching list or null if the key was not found.
	 * @param key the key
	 * @return the list
	 * @throws WrongElementException
	 */
	public final ArrayList<WordExtension> getEx(final String key) throws WrongElementException {
		if (type != WSCacheType.EXPANDER_CACHE || expCache == null) {
			throw new WrongElementException("wrong kind of cache. this method only is suitable for expander cache");
		}
		return expCache.getList(key);
	}

	/**
	 * create a new cache if needed.
	 */
	private void createCache() {
		if (type == WSCacheType.EXPANDER_CACHE) {
			expCache = new CacheExpander();
			System.out.println("New Cache for the Expander created!");
		} else {
			normCache = new CacheNormalizer();
			System.out.println("New Cache for the Normalizer created!");
		}
	}
	
}
