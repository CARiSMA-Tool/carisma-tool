package carisma.check.processanalysis.texttools.wortschatz;

import java.util.ArrayList;
import java.util.HashMap;

import carisma.core.Carisma;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;
import carisma.processanalysis.textmodel.Text;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordExtension;
import carisma.processanalysis.textmodel.WordKind;
import carisma.processanalysis.texttools.Expander;
import de.uni_leipzig.wortschatz.webservice.client.coocurrences.CooccurrencesClient;
import de.uni_leipzig.wortschatz.webservice.client.synonyms.SynonymsClient;

/**
 * This class implements the interface Expander.
 * Its purpose is to connect to the "Wortschatz" web service 
 * and retrieve synonyms and cooccurrent words for a given word.
 * 
 * @author Christian Wessel
 * 
 */
public class WSExpander implements Expander {

	private static final String PROPERTY_EXPANDER_USER = "carisma.check.processanalysis.texttools.wortschatz.expander.user";
	private static final String PROPERTY_EXPANDER_PWD = "carisma.check.processanalysis.texttools.wortschatz.expander.pwd";
	private static final String PROPERTY_EXPANDER_CORPUS = "carisma.check.processanalysis.texttools.wortschatz.expander.corpus";
	
	private String username = null;
	private String password = null;
	private String corpus = null;
	private String limit = Integer.toString(Integer.MAX_VALUE);
	private String minSignificance = "-1";
	private WSCache cache = null;
	
	/** flag if the cache should be used.
	 */
	public enum ExpanderMode {
		/** only the cache is used. */
		USE_ONLY_CACHE,
		/** the cache is NOT used. */
		NOT_USE_CACHE,
		/** both cache and net are used. */
		USE_CACHE_AND_NET
	};
	
	/** actual ExpanderMode.
	 */
	private ExpanderMode mode;
	
	public WSExpander(ExpanderMode newMode) throws WrongElementException {
		mode = newMode;
		Carisma.getInstance().getPreferenceManager().setDefaultPreferenceValue(PROPERTY_EXPANDER_USER, "anonymous"); 
		Carisma.getInstance().getPreferenceManager().setDefaultPreferenceValue(PROPERTY_EXPANDER_PWD, "anonymous"); 
		Carisma.getInstance().getPreferenceManager().setDefaultPreferenceValue(PROPERTY_EXPANDER_CORPUS, "de"); 
				
		this.username = (String) Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_EXPANDER_USER);
		this.password = (String) Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_EXPANDER_PWD);
		this.corpus = (String) Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_EXPANDER_CORPUS);

		if (mode != ExpanderMode.USE_CACHE_AND_NET && newMode != ExpanderMode.USE_ONLY_CACHE && newMode != ExpanderMode.NOT_USE_CACHE) {
			throw new WrongElementException("wrong type. use types of wsexpander");
		}
		if (mode != ExpanderMode.NOT_USE_CACHE) {
			try {
				cache = new WSCache(WSCache.WSCacheType.EXPANDER_CACHE);								
			} catch (WrongElementException w) {
//				System.err.println(w.getMessage());
				Logger.log(LogLevel.ERROR, "Couldn't initialize cache", w);
			}
		}
	}
	
	@Override
	public final ArrayList<WordExtension> getWordExtensions(final Word word, final boolean synonymsOnly, final boolean append) {
		ArrayList<WordExtension> result = null;
			

		// Workaround um Zahlen auszufiltern, diese verursachen
		// TimeoutExceptions beim WS-Service
		boolean number = true;

		try {
			Integer.parseInt(word.getContent());
		} catch (NumberFormatException nfe) {
			number = false;
		}

		if (number) {
			return new ArrayList<WordExtension>();
		}
	
		
		boolean success;
		do {

			// Normal weitermachen
			try {
				if (cache != null) {
					result = cache.getEx(word.toString()); 
				}
				
				// Cookurenzen filtern, wenn nicht gewuenscht
				if (synonymsOnly && result != null) {
					ArrayList<WordExtension> tempResult = new ArrayList<WordExtension>();
					for (WordExtension curExtension : result) {
						if (curExtension.getKind() == WordKind.SYNONYME) {
							tempResult.add(curExtension);
						}
					}
					result = tempResult;		
				}
				
				
				if (cache != null && result != null) {
					return result;							// return if a result was found
				} else if (mode == ExpanderMode.USE_ONLY_CACHE) { 
					return result; 							// if the internet should not be used
				} else { 
					result = new ArrayList<WordExtension>(); 	// instantiate the list
				}
				
				SynonymsClient synonymsClient = new SynonymsClient();
	//			SynonymsClient synonymsClient = new FakeSynonymsClient();
	
				synonymsClient.setCorpus(corpus);
				synonymsClient.setUsername(username);
				synonymsClient.setPassword(password);
	
				synonymsClient.addParameter("Wort", word.getContent());
				synonymsClient.addParameter("Limit", limit);
	
				synonymsClient.execute();
				String[][] synonymsResult = synonymsClient.getResult();
				//String[][] coocurrencesResult = null;
	
				for (int i = 0; i < synonymsResult.length; ++i) {
					// synonymsResult[.][1] ist ???
					// synonymsResult[.][1] ist Wortart
					result.add(new WordExtension(synonymsResult[i][0], WordKind.SYNONYME));
				}
	
				// Alles in die Ausgabe kopieren
				if (!synonymsOnly) {
					CooccurrencesClient coocurrencesClient = new CooccurrencesClient();
	
					coocurrencesClient.setCorpus(corpus);
					coocurrencesClient.setUsername(username);
					coocurrencesClient.setPassword(password);
	
					coocurrencesClient.addParameter("Wort", word.getContent());
					coocurrencesClient.addParameter("Mindestsignifikanz", minSignificance);
					coocurrencesClient.addParameter("Limit", limit);
	
					coocurrencesClient.execute();
					
					String[][] coocurrencesResult = coocurrencesClient.getResult();
	
					// Werte nur dann zur Ergebnisliste hinzuf�gen, falls sie noch
					// nicht existieren
					HashMap<String, Boolean> insertedValues = new HashMap<String, Boolean>();
	
					for (int k = 0; k < coocurrencesResult.length; ++k) {
						// [0] Ausgangswort
						// [1] Cooccurrence
						// [2] Signifikanz der Cooccurrence
	
						if (insertedValues.containsKey(coocurrencesResult[k][1])) {
							continue;
						}
						result.add(new WordExtension(coocurrencesResult[k][1], WordKind.COOCCURRENCE));
						insertedValues.put(coocurrencesResult[k][1], new Boolean(true));
					}
				}
				
				if (cache != null) {
					cache.putEx(word.toString(), result);
				}
				success = true;
			} catch (Exception e) {
				e.printStackTrace();
				System.out.println("Problemwort: " + word.getContent());
//				return null;
				success = false;
			}
		} while(!success);
		return result;
	}

	/**
	 * This method fills a ModelEntity with synonyms and if it is told so even
	 * with cooccurrences for every word that is member of the given Entity.
	 * 
	 * @param entity
	 *            The ModelEntity to be filled with synonyms, etc.
	 * @param synonymsOnly
	 *            Specifies whether to obtain synonyms only or to gather
	 *            cooccurrences too
	 * @param append 
	 *            If true, all previously contained extensions are preserved,
	 *            if false, they are replaced
	 * @return true if the operation ended successfully, false otherwise
	 */
	public final boolean calcExtensions(final ProcessEntity entity, final boolean synonymsOnly, final boolean append) {
		boolean res = true;

		for (Text curText : entity.getTexts()) {
			for (Word curWord : curText.getWordList()) {
//				System.out.println("Expand: " + curWord.getContent());
				ArrayList<WordExtension> result = getWordExtensions(curWord, synonymsOnly, append);

				if (result != null) {
					if (append) {
						curWord.addExtensions(result);
					} else {
						curWord.setExtensions(result);
					}
				} else {
					res = false;
				}
			}
		}

		return res;
	}

	/**
	 * This method fills an entire DataModel with Synonyms and if it is told so
	 * even with cooccurrences for every word that is member of the given
	 * DataModel.
	 * 
	 * @param procDesc
	 *            The DataModel to be filled with synonyms, etc.
	 * @param synonymsOnly
	 *            Specifies whether to obtain synonyms only or to gather
	 *            cooccurrences too
	 * @param append 
	 *            If true, all previously contained extensions are preserved,
	 *            if false, they are replaced
	 * @return true if the operation ended successfully, false otherwise
	 */
	public final boolean calcExtensions(final ProcessDescription procDesc, final boolean synonymsOnly, final boolean append) {
		boolean res = true;

		for (ProcessEntity curEntity : procDesc.getEntities()) {
			res &= calcExtensions(curEntity, synonymsOnly, append);
		}

		return res;
	}
	
	/** Get the username.
	 * 
	 * @return the name of the user.
	 */
	public final String getUsername() {
		return username;
	}
	
	/** Set the username.
	 * 
	 * @param username new name of the user.
	 */
	public final void setUsername(final String username) {
		this.username = username;
	}

	/** Get the password.
	 * 
	 * @return the password.
	 */
	public final String getPassword() {
		return password;
	}

	/** Set the password.
	 * 
	 * @param password the new password.
	 */
	public final void setPassword(final String password) {
		this.password = password;
	}

	/** Get the corpus.
	 * 
	 * @return the corpus.
	 */
	public final String getCorpus() {
		return corpus;
	}

	/** Set the corpus.
	 * 
	 * @param corpus the new corpus.
	 */
	public final void setCorpus(final String corpus) {
		this.corpus = corpus;
	}


	/** Get the limit.
	 * 
	 * @return the limit.
	 */
	public final String getLimit() {
		return limit;
	}

	/** Set the limit.
	 * 
	 * @param limit the new limit.
	 */
	public final void setLimit(final String limit) {
		this.limit = limit;
	}

	/** Get the minimal significance.
	 * 
	 * @return the minimal significance.
	 */
	public final String getMinSignificance() {
		return minSignificance;
	}

	/** Set the minimal significance.
	 * 
	 * @param minSignificance new minimal significance.
	 */
	public final void setMinSignificance(final String minSignificance) {
		this.minSignificance = minSignificance;
	}

	@Override
	public final ArrayList<WordExtension> getWordExtensions(final Word word, final boolean synonymsOnly) {		
		return this.getWordExtensions(word, synonymsOnly, false);
	}
	
	/** stores the cache.
	 */
	public void storeCache(){
		cache.storeCache();
	}
}
