package carisma.check.processanalysis.texttools.wortschatz;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Iterator;

import carisma.check.processanalysis.texttools.wortschatz.WSCache.WSCacheType;
import carisma.core.Carisma;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordExtension;
import carisma.processanalysis.textmodel.WordKind;
import carisma.processanalysis.texttools.Normalizer;
import carisma.processanalysis.texttools.Spellchecker;

import com.stibocatalog.hunspell.Hunspell;

import de.uni_leipzig.wortschatz.webservice.client.baseform.BaseformClient;

/**
 * This class implements the normalization (i.e. getting the base form) of words
 * by asking the web service of the project "Wortschatz" of the
 * "Universitaet Leipzig". Furthermore a spell checking is done first, because
 * "Wortschatz" does not do this by itself and may of little help for misspelled
 * words.
 * 
 * The spell checking is done with the help of the tool "hunspell". This is a
 * command line tool written in a native language. So we must use a java wrapper
 * to make it work for us.
 * 
 * @author Christian Wessel
 * 
 */
public class WSNormalizer implements Normalizer, Spellchecker {
	
	private static final String PROPERTY_NORMALIZER_DICTIONARY_PATH = "carisma.check.processanalysis.texttools.wortschatz.dictionary.path";
	private static final String PROPERTY_NORMALIZER_DICTIONARY_USER = "carisma.check.processanalysis.texttools.wortschatz.dictionary.user";
	private static final String PROPERTY_NORMALIZER_DICTIONARY_PWD = "carisma.check.processanalysis.texttools.wortschatz.dictionary.pwd";
	private static final String PROPERTY_NORMALIZER_DICTIONARY_CORPUS = "carisma.check.processanalysis.texttools.wortschatz.dictionary.corpus";
	private static final String PROPERTY_NORMALIZER_DICTIONARY_DICT = "carisma.check.processanalysis.texttools.wortschatz.dictionary.dict";

	private String username = null;
	private String password = null;
	private String corpus = null;
	private String dictionary = null;
	private WSCache cache = null;
	
	// flag if the cache should be used	
	public enum NormalizerMode{
		USE_ONLY_CACHE,
		NOT_USE_CACHE,
		USE_CACHE_AND_NET
	};
	
	private NormalizerMode mode;

	// TODO: Hier gibt es ein Problem: Der Pfad muss absolut sein, damit die Lib
	// die Dateien findet. Wie schafft man das mit Java?
		/* 
		 * Anmerkung: Absoluter Pfad bringt keine Besserung, dbuerger 
		 * new File("resources/dict/"+dictionary).getAbsolutePath()
		 * 
		 *  Problem ist das die Dateien nicht gelesen werden koennen... canRead() ist false
		 */
	private String dictPath = null;

	/**
	 * The default constructor. Does nothing for now.
	 * @throws WrongElementException 
	 */
	public WSNormalizer(NormalizerMode mode) throws WrongElementException {
		this.mode = mode;
		Carisma.getInstance().getPreferenceManager().setDefaultPreferenceValue(PROPERTY_NORMALIZER_DICTIONARY_PATH, System.getProperty("user.home")+System.getProperty("file.separator")+"dictionary"+System.getProperty("file.separator"));
		Carisma.getInstance().getPreferenceManager().setDefaultPreferenceValue(PROPERTY_NORMALIZER_DICTIONARY_USER, "anonymous"); 
		Carisma.getInstance().getPreferenceManager().setDefaultPreferenceValue(PROPERTY_NORMALIZER_DICTIONARY_PWD, "anonymous"); 
		Carisma.getInstance().getPreferenceManager().setDefaultPreferenceValue(PROPERTY_NORMALIZER_DICTIONARY_CORPUS, "de"); 
		Carisma.getInstance().getPreferenceManager().setDefaultPreferenceValue(PROPERTY_NORMALIZER_DICTIONARY_DICT, "de_DE"); 
				
		this.dictPath = (String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_NORMALIZER_DICTIONARY_PATH);
		this.username = (String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_NORMALIZER_DICTIONARY_USER);
		this.password = (String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_NORMALIZER_DICTIONARY_PWD);
		this.corpus = (String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_NORMALIZER_DICTIONARY_CORPUS);
		this.dictionary = (String)Carisma.getInstance().getPreferenceManager().getPreferenceValue(PROPERTY_NORMALIZER_DICTIONARY_DICT);
		
		if (mode != NormalizerMode.NOT_USE_CACHE) {
			this.cache = new WSCache(WSCacheType.NORMALIZER_CACHE);			
		}
	}

	/**
	 * This method connects to the "Wortschatz" web service to get the base form
	 * of the given Word.
	 * 
	 * @param word
	 *            The word for which the base from has to be retrieved.
	 * @return a new instance of Word containing the base form. If an error
	 *         occurred null is returned. If no base form could be found 'null'
	 *         is returned.
	 */
	@Override
	public final Word getBaseform(final Word word) {
		String baseform = "";
		String entry;
		
		// Workaround um Zahlen auszufiltern, diese verursachen
		// TimeoutExceptions beim WS-Service
		boolean number = true;
		try {
			Integer.parseInt(word.getContent());
		} catch (NumberFormatException nfe) {
			number = false;
		}
		if (number) {
			return new Word();
		}
		boolean success;
		do {
			try {
				if(this.cache != null){
					entry = cache.getNorm(word.toString());				
				}
				else{
					entry = null;
				}
				if(entry != null){
					return new Word(entry);				// if a word was found return it
				} else if(this.mode == NormalizerMode.USE_ONLY_CACHE){
					return null;						// if the internet should not be used return null
				}
				BaseformClient baseformClient = new BaseformClient();
				baseformClient.setCorpus(corpus);
				baseformClient.setUsername(username);
				baseformClient.setPassword(password);
				baseformClient.addParameter("Wort", word.getContent());
				baseformClient.execute(); //TH: wie erkennt man hier eine Exception?
				String[][] result = baseformClient.getResult();
					// Das Ergebnis ist ein zweidimensionales Array aus Strings
					// Es enth�lt nur eine Zeile, sodass i = 0
					// Es enth�lt zwei Spalten, sodass
					// j = 0: Basisform
					// j = 1: ???
					// Bei W�rten zu denen keine Basisform gefunden werden kann, ist das
					// Ergebnis leer
				if (result.length > 0) {
					if (result[0].length > 0) {
						baseform = result[0][0];
					}
				}
				if (cache != null) {
					cache.putNorm(word.toString(), baseform);
				}
				success = true;
			} catch (Exception e) {
				e.printStackTrace();
				System.out.println("Problemwort: " + word.getContent());
	//			return null;
				success = false;
			} 
		} while(!success);

		return new Word(baseform);
	}

	@Override
	public final boolean isMisspelled(final Word word) {
		boolean misspelled = false;

		try {
			File test = new File(dictPath + dictionary + ".aff");
			System.out.println("Can file read? " + test.canRead());
			Hunspell.Dictionary dict = Hunspell.getInstance().getDictionary(dictPath + dictionary);	
			misspelled = dict.misspelled(word.getContent());
		} catch (FileNotFoundException e) {
			System.out.println("Dictionary not found");
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			System.out.println("Encoding unsupported");
			e.printStackTrace();
		} catch (UnsatisfiedLinkError e) {
			System.out.println("Unsatisfied link");
			e.printStackTrace();
		} catch (UnsupportedOperationException e) {
			System.out.println("Unsupported operation");
			e.printStackTrace();
		}

		return misspelled;
	}

	@Override
	public final ArrayList<String> getCorrectSpelledCandidates(final Word word) {
		ArrayList<String> suggestions = new ArrayList<String>();

		try {
			Hunspell.Dictionary dict = Hunspell.getInstance().getDictionary(dictPath + dictionary); 

			// Debug
			System.out.println(word.getContent());
			// Debug end

			Iterator<String> iter = dict.suggest(word.getContent()).iterator();

			while (iter.hasNext()) {
				suggestions.add(iter.next());
			}
		} catch (FileNotFoundException e) {
			System.out.println("Dictionary not found");
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			System.out.println("Encoding unsupported");
			e.printStackTrace();
		} catch (UnsatisfiedLinkError e) {
			System.out.println("Unsatified link");
			e.printStackTrace();
		} catch (UnsupportedOperationException e) {
			System.out.println("Unsupported operation");
			e.printStackTrace();
		} catch (NullPointerException e) {
			// Wird nur gefangen, da hunspell mit W�rten f�r die er keine
			// Kandidaten anbieten kann eine Exception generiert
			System.out.println("no suggestions found");
		}

		return suggestions;
	}

	public final String getUsername() {
		return username;
	}

	public final void setUsername(final String username) {
		this.username = username;
	}

	public final String getPassword() {
		return password;
	}

	public final void setPassword(final String password) {
		this.password = password;
	}

	public final String getCorpus() {
		return corpus;
	}

	public final void setCorpus(final String corpus) {
		this.corpus = corpus;
	}

	public final String getDictionary() {
		return dictionary;
	}

	public final void setDictionary(final String dictionary) {
		this.dictionary = dictionary;
	}

	public final String getDictPath() {
		return dictPath;
	}

	public final void setDictPath(final String dictPath) {
		this.dictPath = dictPath;
	}
	
	public void storeCache(){
		if(cache != null){
			cache.storeCache();
		}
	}
	
	// nur debug
//	public static void main(String[] args) throws WrongElementException{
//		WSNormalizer norm = new WSNormalizer(NormalizerMode.NOT_USE_CACHE);
//		System.out.println(norm.getBaseform(new Word("Schlangen")));
//		System.out.println(norm.getBaseform(new Word("1999")));
//		
//		norm = new WSNormalizer(NormalizerMode.USE_CACHE_AND_NET);
//		System.out.println(norm.getBaseform(new Word("Schlangen")));
//		System.out.println(norm.getBaseform(new Word("1999")));
//		norm.cache.storeCache();
//
//		norm = new WSNormalizer(NormalizerMode.USE_ONLY_CACHE);
//		System.out.println(norm.getBaseform(new Word("Schlangen")));
//		System.out.println(norm.getBaseform(new Word("1999")));
//	}
	
}
