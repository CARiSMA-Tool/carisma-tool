//package carisma.check.processanalysis.texttools.wortschatz.tests;
//
//import static org.junit.Assert.assertNotNull;
//import static org.junit.Assert.assertTrue;
//import static org.junit.Assert.fail;
//
//import java.io.File;
//import java.util.ArrayList;
//import java.util.Random;
//
//import org.junit.Test;
//
//import carisma.core.Carisma;
//import carisma.check.processanalysis.texttools.wortschatz.WSCache;
//import carisma.check.processanalysis.texttools.wortschatz.WrongElementException;
//import carisma.processanalysis.textmodel.WordExtension;
//import carisma.processanalysis.textmodel.WordExtensionKind;
//
///**
// * Tests the WSCache class with one test for each, the normalizer cache and the expander cache.
// * @author jkowald
// *
// */
//public class WSCacheTest {
//	
//	/**
//	 * A static string which contains the preference key for the expander cache 
//	 * filename in the carisma preferences.
//	 */
//	private static final String PROPERTY_EXPANDER_CACHE_FILENAME = "carisma.check.processanalysis.texttools.wortschatz.cache.expander";
//	
//	/**
//	 * A static string which contains the preference key for the normalizer cache 
//	 * filename in the carisma preferences.
//	 */
//	private static final String PROPERTY_NORMALIZER_CACHE_FILENAME = "carisma.check.processanalysis.texttools.wortschatz.cache.normalizer";
//	
//	/**
//	 * An instance of the random class to get random numbers.
//	 */
//	private Random rnd;
//	
//	/**
//	 * A String which defines the characters which can occur in the random strings.
//	 */
//	private String characters;
//	
//	/**
//	 * The normalizer cache test entries.
//	 */
//	private String[][] normEntries;
//	
//	/**
//	 * The expander cache test entries.
//	 */
//	private String[][] expEntries;
//	
//	/**
//	 * The expander cache test WordExtensionKinds.
//	 */
//	private WordExtensionKind[][] expEntryWordExtensionKinds;
//	
//	/**
//	 * The number of test entries which will be created and filled into the test caches.
//	 */
//	private int testPairCount;
//	
//	/**
//	 * Constructor. All necessary variables are filled here.
//	 */
//	public WSCacheTest(){
//		testPairCount = 1000;
//		rnd = new Random();
//		characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
//		normEntries = new String[testPairCount][2];
//		expEntries = new String[testPairCount][11];
//		expEntryWordExtensionKinds = new WordExtensionKind[testPairCount][10];
//		
//	}
//	
//	/**
//	 * Tests the normalizer cache by accessing it through the WSCache class. 
//	 * A normalizer test cache will be created and filled with a defined count of
//	 * test entries. Afterwards, the cache will be reloaded and the entries are checked
//	 * for conformity with the test entries.
//	 */
//	@Test
//	public final void testNormalizerCache() {
//		System.out.println("WSCache: Testing NormalizerCache ...");
//		
//		String randomFilename = randomString();
//		
//		System.out.println("Setting Carisma preference value");
//		Carisma.getInstance().getPreferenceManager().setPreferenceValue(PROPERTY_NORMALIZER_CACHE_FILENAME, "resources" + System.getProperty("file.separator") + "norm_" + randomFilename + ".ser");
//		assertTrue(
//				Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_NORMALIZER_CACHE_FILENAME).equals(
//						"resources" + System.getProperty("file.separator") + "norm_" + randomFilename + ".ser"));	
//		
//		try {
//			System.out.println("Initializing test normalizer cache");
//			WSCache wscache = new WSCache(WSCache.WSCacheType.NORMALIZER_CACHE);
//			assertNotNull(wscache);
//			assertNotNull(rnd);
//			assertNotNull(normEntries);
//			
//			System.out.println("Putting " + testPairCount + " random key-value pairs into test normalizer cache");
//			for (int i=0; i<testPairCount; i++) {
//				normEntries[i][0] = randomString();
//				normEntries[i][1] = randomString();
//				// Avoids putting entries in the cache which already exist
//				if (wscache.getNorm(normEntries[i][0]) == null){
//					wscache.putNorm(normEntries[i][0], normEntries[i][1]); 
//				} else {
//					i--;
//				}
//			}
//			
//			wscache.storeCache();
//			File testFile = new File((String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_NORMALIZER_CACHE_FILENAME));
//			assertTrue(testFile.exists());
//			System.out.println("Test normalizer cache stored to " + testFile.getAbsolutePath());
//			
//			System.out.println("Reloading test normalizer cache");
//			wscache = null;
//			wscache = new WSCache(WSCache.WSCacheType.NORMALIZER_CACHE);
//			assertNotNull(wscache);
//			
//			System.out.println("Checking the key-value pairs in reloaded test normalizer cache");
//			String failmessage = "";
//			for (int i=0; i<testPairCount; i++) {
//				if (!wscache.getNorm(normEntries[i][0]).equals(normEntries[i][1])) {
//					failmessage += "The test normalizer cache contains key-value pairs which are different from the generated"
//							+ "\n  Cacheentry #" + i + " (" + normEntries[i][0] + "," + wscache.getNorm(normEntries[i][0]) + ")"
//							+ "\n  Testentry #" + i + " (" + normEntries[i][0] + "," + normEntries[i][1] + ")\n";
//				}
//			}
//			if (!failmessage.equals("")) {
//				fail(failmessage);
//			}
//			
//			System.out.println("Testing the methods putEx() and getEx() which should throw exceptions because they are not suitable for normalizer cache");
//			try { 
//				wscache.putEx("",new ArrayList<WordExtension>());
//			} catch(Exception e) {
//				assertTrue(e.getClass().equals(WrongElementException.class));
//			}
//			try { 
//				wscache.getEx("");
//			} catch(Exception e) {
//				assertTrue(e.getClass().equals(WrongElementException.class));
//			}
//			
//			System.out.println("Deleting test normalizer cache file");
//			wscache = null;
//			if (testFile.exists()) {
//				testFile.deleteOnExit();
//			}
//		} catch (WrongElementException wee) {
//			wee.printStackTrace();
//			fail(wee.getMessage());
//		}
//	}
//	
//	/**
//	 * Tests the expander cache by accessing it through the WSCache class. 
//	 * An expander test cache will be created and filled with a defined count of
//	 * test entries. Afterwards, the cache will be reloaded and the entries are checked
//	 * for conformity with the test entries.
//	 */
//	@Test
//	public final void testExpanderCache() {
//		System.out.println("WSCache: Testing ExpanderCache ...");
//		
//		String randomFilename = randomString();
//		
//		System.out.println("Setting Carisma preference value");
//		Carisma.getInstance().getPreferenceManager().setPreferenceValue(PROPERTY_EXPANDER_CACHE_FILENAME, "resources" + System.getProperty("file.separator") + "exp_" + randomFilename + ".ser");
//		assertTrue(
//				Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_EXPANDER_CACHE_FILENAME).equals(
//						"resources" + System.getProperty("file.separator") + "exp_" + randomFilename + ".ser"));	
//		
//		try {
//			System.out.println("Initializing test expander cache");
//			WSCache wscache = new WSCache(WSCache.WSCacheType.EXPANDER_CACHE);
//			assertNotNull(wscache);
//			assertNotNull(rnd);
//			assertNotNull(expEntries);
//			assertNotNull(expEntryWordExtensionKinds);
//			
//			System.out.println("Putting " + testPairCount + " random key-value pairs with 1 to 10 WordExtensions into test expander cache");
//			int wordExtCount = 0;
//			for (int i=0; i<testPairCount; i++) {
//				expEntries[i][0] = randomString();
//				ArrayList<WordExtension> expEntryArrayList = new ArrayList<WordExtension>();
//				wordExtCount = rnd.nextInt(11) + 1;
//				for (int k=1; k<wordExtCount; k++) {
//					expEntries[i][k] = randomString();
//					expEntryWordExtensionKinds[i][k-1] = randomWordExtensionKind();
//					expEntryArrayList.add(new WordExtension(expEntries[i][k], expEntryWordExtensionKinds[i][k-1]));
//				}
//				// Avoids putting entries in the cache which already exist
//				if (wscache.getEx(expEntries[i][0]) == null){
//					wscache.putEx(expEntries[i][0], expEntryArrayList); 
//				} else {
//					expEntryArrayList.clear();
//					i--;
//				}
//			}
//			
//			wscache.storeCache();
//			File testFile = new File((String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_EXPANDER_CACHE_FILENAME));
//			assertTrue(testFile.exists());
//			System.out.println("Test expander cache stored to " + testFile.getAbsolutePath());
//			
//			System.out.println("Reloading test expander cache");
//			wscache = null;
//			wscache = new WSCache(WSCache.WSCacheType.EXPANDER_CACHE);
//			assertNotNull(wscache);
//			
//			System.out.println("Checking the key-value pairs in reloaded test expander cache");
//			String failmessage = "";
//			int wec = 1;
//			for (int i=0; i<testPairCount; i++) {
//				ArrayList<WordExtension> wordExtList = wscache.getEx(expEntries[i][0]);
//				wec = 1;
//				for (WordExtension we : wordExtList) {
//					if (!(we.getContent().equals(expEntries[i][wec]))
//							|| !(we.getKind().equals(expEntryWordExtensionKinds[i][wec-1]))) {
//						failmessage += "The test expander cache contains key-value pairs which are different from the generated"
//								+ "\n  Cacheentry #" + i + "/" + wec + " (" + expEntries[i][0] + ", " + we.getContent() + ", " + we.getKind() + ")"
//								+ "\n  Testentry #" + i + "/" + wec + " (" + expEntries[i][0] + "," + expEntries[i][wec] + ", " + expEntryWordExtensionKinds[i][wec-1] + ")\n";
//					}
//					wec++;
//				}
//			}
//			if (!failmessage.equals("")) {
//				fail(failmessage);
//			}
//			
//			System.out.println("Testing the methods putNorm() and getNorm() which should throw exceptions because they are not suitable for normalizer cache");
//			try { 
//				wscache.putNorm("","");
//			} catch(Exception e) {
//				assertTrue(e.getClass().equals(WrongElementException.class));
//			}
//			try { 
//				wscache.getNorm("");
//			} catch(Exception e) {
//				assertTrue(e.getClass().equals(WrongElementException.class));
//			}
//			
//			System.out.println("Deleting test expander cache file");
//			wscache = null;
//			if (testFile.exists()) {
//				testFile.deleteOnExit();
//			}
//		} catch (WrongElementException wee) {
//			wee.printStackTrace();
//			fail(wee.getMessage());
//		}
//	}
//	
//	
//	/**
//	 * Creates a string with random characters (a-z,A-Z) or numbers in it.
//	 * @return The created string
//	 */
//	private String randomString() {
//		String returnString = "";
//		int length = rnd.nextInt(19) + 2;
//		for (int i=0; i<length; i++) {
//			returnString += characters.charAt(rnd.nextInt(characters.length()));
//		}
//		return returnString;
//	}
//	
//	
//	/**
//	 * Chooses a random WordExtensionKind and returns it.
//	 * @return The chosen WordExtensionKind
//	 */
//	private WordExtensionKind randomWordExtensionKind() {
//		int rand = rnd.nextInt(3);
//		switch (rand) {
//		default:
//			return WordExtensionKind.COOCCURRENCE;
//		case 1:
//			return WordExtensionKind.SYNONYME;
//		case 2:
//			return WordExtensionKind.UNKNOWN;
//		}
//	}
//}
