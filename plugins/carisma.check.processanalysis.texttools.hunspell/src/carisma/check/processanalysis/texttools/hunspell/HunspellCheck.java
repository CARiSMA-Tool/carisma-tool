package carisma.check.processanalysis.texttools.hunspell;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;
import carisma.processanalysis.textmodel.Text;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.texttools.Spellchecker;

import com.stibocatalog.hunspell.Hunspell;

public class HunspellCheck implements CarismaCheck, Spellchecker {

	private String dictionary = "de_DE";
	private String dictPath = "resources/dict/";
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		
		ProcessDescription processDescription = null;
		
		try {
			 processDescription = (ProcessDescription) host.getFromRegister(ProcessDescription.CARISMA_REGISTRY_KEY);
		} catch (RegisterNotInUseException e) {
			e.printStackTrace();
			return false;
		}
		
		for(ProcessEntity e : processDescription.getEntities()) {
			for( Text t : e.getTexts()) {
				for( Word w : t.getWordList()) {
					if(isMisspelled(w)) {
						ArrayList<String> suggestions = getCorrectSpelledCandidates(w);
						
						//Keine Entsprechung gefunden --> Abbruch
						if( suggestions.size() == 0 ) {
							host.appendLineToReport("No suggestion found for " + w.getContent() + ". Aborting.");
							return false;
						}
						
						//TODO:Es wird eine GUI gebraucht, welche die Liste der Vorschläge
						//anzeigt, sodass der Benutzer das korrekte Wort auswählen kann.
						//Die Auswahl muss dann in w gespeichert werden.
					}
				}
			}
		}
		
		try {
			host.removeFromRegister(ProcessDescription.CARISMA_REGISTRY_KEY);
			host.putToRegister(ProcessDescription.CARISMA_REGISTRY_KEY, processDescription);
		} catch (RegisterInUseException e1) {
			host.appendLineToReport("Unable to put process descxription to registry, Aborting");
			e1.printStackTrace();
			return false;
		} catch (RegisterNotInUseException e) {
			e.printStackTrace();
			return false;
		}
		
		return true;
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
			// Wird nur gefangen, da hunspell mit Woerten fuer die er keine
			// Kandidaten anbieten kann eine Exception generiert
			System.out.println("no suggestions found");
		}

		return suggestions;
	}
}
