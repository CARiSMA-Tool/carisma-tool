//package carisma.check.processanalysis.texttools.wortschatz.tests;
//
//import static org.junit.Assert.assertFalse;
//import static org.junit.Assert.assertTrue;
//import static org.junit.Assert.fail;
//
//import java.util.ArrayList;
//
//import org.junit.Test;
//
//import carisma.check.processanalysis.texttools.wortschatz.WSExpander;
//import carisma.check.processanalysis.texttools.wortschatz.WrongElementException;
//import carisma.processanalysis.textmodel.Word;
//import carisma.processanalysis.textmodel.WordExtension;
//import carisma.processanalysis.textmodel.WordExtensionKind;
//
//public class WSExpanderTest {
//
//	@Test
//	public final void testGetWordExtensions() {
//		System.out.println("WSExpanderTest Start");
//
//		try{
//			//Cache explizit ausschalten
//			WSExpander expander = new WSExpander(WSExpander.ExpanderMode.NOT_USE_CACHE);
//			
//			// Voelliger Unsinn -> duerfte nichts finden
//			ArrayList<WordExtension> test = expander.getWordExtensions(new Word("asdfsdf"), false, false);
//			assertTrue(test.isEmpty());
//
//			// Liefert mit WS keine Synonyme aber Kookurrenzen
//			test = expander.getWordExtensions(new Word("Benutzername"), false, false);
//
//			//Die Ergebnisliste sollte nicht leer sein
//			assertFalse(test.size() == 0);
//			
//			//und nur Kookurrenzen enthalten
//			for(WordExtension ext : test) {
//				assertTrue(ext.getKind() == WordExtensionKind.COOCCURRENCE);
//			}
//
//			// Liefert beides -> sollte nur nicht "unknown" sein
//			test = expander.getWordExtensions(new Word("Name"), false, false);
//			
//			//Die Ergebnisliste sollte nicht leer sein
//			assertFalse(test.size() == 0);
//			
//			//und keine "unknown"-Elemente enthalten
//			for(WordExtension ext : test) {
//				assertTrue(ext.getKind() != WordExtensionKind.UNKNOWN);
//			}
//
//			//Mit Zahlen scheint WS ein Problem zu haben -> Sollte also vom WSExpander vorher erkannt werden
//			//und eine leere Liste als Ergebnis zurückliefern
//			test = expander.getWordExtensions(new Word("1999"), false, false);
//			assertTrue(test.isEmpty());
//			
//		} catch (WrongElementException w){
//			fail(w.getMessage());
//		}
//		
//		System.out.println("WSExpanderTest end");
//
//		// System.out.println("Aschenputtel-Test start (ca. 500 Woerter)");
//		//
//		// TextFileImporter importer = new
//		// TextFileImporter("tests/carisma/check/riskfinder/test/aschenputtel.txt");
//		// DataModel model2 = importer.doimport();
//		//
//		//
//		// long startTime = System.currentTimeMillis();
//		// WSExpander expander = new WSExpander();
//		// expander.calcExtensions(model2, false);
//		// long duration = System.currentTimeMillis() - startTime;
//		// System.out.println("Laufzeit f�r Aschenputtel: " + duration / 1000 +
//		// " s");
//		//
//		// System.out.println("BSI-Test start  (ca. 900 W�rter)");
//		// importer = new
//		// TextFileImporter("tests/carisma/check/riskfinder/test/bsi-text.txt");
//		// DataModel model3 = importer.doimport();
//		//
//		// startTime=System.currentTimeMillis();
//		// expander.calcExtensions(model3, false);
//		// duration=System.currentTimeMillis()-startTime;
//		// System.out.println("Laufzeit f�r BSI-Text: "+duration/1000+" s");
//	}
//	
////
////	@Test
////	public final void testDumpCache() {
////		try {
////			WSExpander expander = new WSExpander(WSExpander.USE_CACHE_AND_NET);
////			System.out.println(expander.cache.expCache.getSize());
////			for(String key : expander.cache.expCache.map.keySet()){
////				ArrayList<WordExtension> value = expander.cache.expCache.getList(key);
////				System.out.println(key + " : " + value);
////			}
////		} catch (WrongElementException e) {
////			// TODO Auto-generated catch block
////			e.printStackTrace();
////		}
////	}
//
//}
