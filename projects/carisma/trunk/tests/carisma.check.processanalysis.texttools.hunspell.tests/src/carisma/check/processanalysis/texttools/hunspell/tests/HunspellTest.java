package carisma.check.processanalysis.texttools.hunspell.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;

import org.junit.Test;

import carisma.check.processanalysis.texttools.hunspell.HunspellCheck;
import carisma.processanalysis.textmodel.Word;


public class HunspellTest {

	@Test
	public void test() {
		HunspellCheck check = new HunspellCheck();
		
		//Das Wort sollte als nicht korrekt erkannt werden
		boolean misspelled = check.isMisspelled(new Word("Tset"));
		assertTrue(misspelled);
		
		//Das Wort sollte als korrekt erkannt werden
		misspelled = check.isMisspelled(new Word("Test"));
		assertFalse(misspelled);
		
		//Bei korrekten Woertern sollte die Liste leer sein
		ArrayList<String> suggestions = check.getCorrectSpelledCandidates(new Word("Test"));
		assertEquals(suggestions.size(), 0);
		
		//Bei falsch geschriebenen Woerten sollte eine Liste mit Kandidaten zurueck gegeben werden
		suggestions = check.getCorrectSpelledCandidates(new Word("Tset"));		
		boolean isFilled = (suggestions.size() > 0);
		assertTrue(isFilled);
		
		// Der Spellchecker sollte hier warnen und Verbesserungen liefern
		//model.addEntity(new ProcessEntity("", "", null, "Loschen"));
	}
}
