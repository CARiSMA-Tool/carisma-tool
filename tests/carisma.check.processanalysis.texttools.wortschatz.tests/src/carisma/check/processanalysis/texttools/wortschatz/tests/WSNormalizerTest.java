package carisma.check.processanalysis.texttools.wortschatz.tests;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

import carisma.check.processanalysis.texttools.wortschatz.WSNormalizer;
import carisma.check.processanalysis.texttools.wortschatz.WrongElementException;
import carisma.processanalysis.textmodel.Word;

public class WSNormalizerTest {
	/*
	@Test
	public final void test() {
		System.out.println("WSNormalizerTest Start");

		// Start the test
		try{
			WSNormalizer normalizer = new WSNormalizer(WSNormalizer.NormalizerMode.NOT_USE_CACHE);

			//Voelliger Unsinn --> Hierzu sollte kein Wortstamm zu finden sein
			Word baseform = normalizer.getBaseform(new Word("asdfsdf"));
			assertNull(baseform);

			//Korrektes Wort --> Das sollte schon der Wortstamm sein
			baseform = normalizer.getBaseform(new Word("Name"));
			assertNull(baseform);

			//Der Normalizer sollte hier als Basisform "Schlange" liefern
			baseform = normalizer.getBaseform(new Word("Schlangen"));
			assertNotNull(baseform);
			assertTrue(baseform.getContent().equals("Schlange"));
			
		} catch (WrongElementException w) {
			fail(w.getMessage());
		}


//		 System.out.println("Aschenputtel-Test start (ca. 500 W�rter)");
//		
//		 TextFileImporter importer = new
//		 TextFileImporter("tests/carisma/check/riskfinder/test/aschenputtel.txt");
//		 DataModel model2 = importer.doimport();
//		
//		
//		long startTime = System.currentTimeMillis();
//		for (ProcessEntity curEntity : model2.getEntities()) {
//			for (Text curText : curEntity.getTexts())
//				for (Word curWort : curText.getWordList()) {
//					String wort = curWort.getContent();
//					String norm1 = normalizer.getBaseform(new Word(wort)).getContent();
//					String norm2 = normalizer.getBaseform(new Word(norm1)).getContent();
//					String norm3 = normalizer.getBaseform(new Word(norm2)).getContent();
//					if(!norm1.equals(norm2) && !norm2.equals(norm3))
//						System.out.println(wort + " " + norm1 + " " + norm2 + " " + norm3);
//				}
//		}
//		 
//		 long duration = System.currentTimeMillis() - startTime;
//		 System.out.println("Laufzeit f�r Aschenputtel: " + duration / 1000 +
//		 " s");
		
		// System.out.println("BSI-Test start  (ca. 900 W�rter)");
		// importer = new
		// TextFileImporter("tests/carisma/check/riskfinder/test/bsi-text.txt");
		// DataModel model3 = importer.doimport();
		//
		// startTime=System.currentTimeMillis();
		// expander.calcExtensions(model3, false);
		// duration=System.currentTimeMillis()-startTime;
		// System.out.println("Laufzeit f�r BSI-Text: "+duration/1000+" s");
		
		System.out.println("WSNormalizerTest end");
	}
	*/
	@Test
	public final void testLocalCache() {
		System.out.println("WSNormalizerTest.testLocalCache starts");
		
		// Start the test
		try{
			WSNormalizer normalizer = new WSNormalizer(WSNormalizer.NormalizerMode.USE_ONLY_CACHE);
			Word resultWord = normalizer.getBaseform(new Word(""));
			assertNotNull(resultWord);
			assertTrue(resultWord.getContent().equals(""));
		} catch (WrongElementException wee) {
			wee.printStackTrace();
			fail(wee.getMessage());
		} catch (Exception e) {
			e.printStackTrace();
			fail(e.getMessage());
		}
		
		System.out.println("WSNormalizerTest.testLocalCache ends");
	}
}
