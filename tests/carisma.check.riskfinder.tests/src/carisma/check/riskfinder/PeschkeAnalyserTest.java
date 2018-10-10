//package carisma.check.riskfinder;
//
//import static org.junit.Assert.assertEquals;
//import static org.junit.Assert.assertTrue;
//
//import java.util.ArrayList;
//
//import org.junit.Test;
//
//import carisma.check.processanalysis.texttools.wortschatz.WSExpander;
//import carisma.check.processanalysis.texttools.wortschatz.WrongElementException;
//import carisma.check.riskfinder.PatternCatalog.BSIOnto;
//import carisma.check.riskfinder.PatternCatalog.LAWOnto;
//import carisma.check.riskfinder.PatternCatalog.MARiskOnto;
//import carisma.processanalysis.loader.misc.TextFileImporter;
//import carisma.processanalysis.textmodel.ProcessDescription;
//import carisma.processanalysis.textmodel.ProcessEntity;
//import carisma.processanalysis.textmodel.WordExtension;
//import carisma.processanalysis.textmodel.WordExtensionKind;
//
//public class PeschkeAnalyserTest {
//
//	@Test
//	public final void test() {
//		System.out.println("Minitest Start");
//		// Patternkatalog vorbereiten
//		PatternCatalog catalog = new PatternCatalog();
//		catalog.createFromOntology("resources/junit_testfile_short.owl", BSIOnto.ALL, MARiskOnto.ALL, LAWOnto.All);
//		System.out.println(catalog);
//
//		// Entity bauen:
//		ProcessEntity entity = new ProcessEntity("", "", null, "Verschluesselung und Authentikation");
//
//		PeschkeAnalyser analyser = new PeschkeAnalyser();
//		
//		StopwordFilter stopwords = new StopwordFilter();
//		stopwords.loadFromFile("resources/stopwords.ger");
//		
//		ArrayList<StringFilter> filters = new ArrayList<StringFilter>();
//		filters.add(stopwords);
//
////		stopwords.loadFromFile("tests/carisma/check/riskfinder/test/stopwords.ger");
//
//		AnalyserResult result = analyser.getRelevantPatterns(catalog, entity, filters);
//
//		System.out.println(result);
//
//		System.out.println("Relevante Pattern: " + result.getAllRelevantPatternNamesAndTitles());
//		System.out.println("Relevante Worte: " + result.getAllRelevantWords());
//
//		assertEquals(1, result.size());
//		assertEquals("BSIGrundschutz_G4.33", result.iterator().next().getPattern().getName());
//		assertEquals("BSIGrundschutz_G4.33", result.getAllRelevantPatternNames().iterator().next());
//		assertEquals("Authentikation(500.0)", result.iterator().next().getRelevantWords().iterator().next().toString());
//		assertEquals("Authentikation(500.0)", result.getAllRelevantWords().iterator().next().toString());
//
//		System.out.println("Minitest Ende\n");
//
//		System.out.println("Richtiger Test Start");
//		// Patternkatalog vorbereiten
//		catalog = new PatternCatalog();
//		catalog.createFromOntology("resources/ontology1.owl", BSIOnto.ALL, MARiskOnto.ALL, LAWOnto.All);
//		// System.out.println(catalog);
//
//		ArrayList<ProcessEntity> entityList = new ArrayList<ProcessEntity>();
//		entityList.add(new ProcessEntity("", "", null, "asdfsdf"));
//		entityList.add(new ProcessEntity("", "", null, "L�schen")); // Test f�r Umlaute, sollte u.a. B 1.15 finden
//		entityList.add(new ProcessEntity("", "", null, "wasnQuatch")); // Test f�r Wordextension, sollte B 1.7 finden
//		entityList.get(entityList.size() - 1).getTexts().get(0).getWordList().get(0).addExtension(new WordExtension("Kryptokonzept",
//				WordExtensionKind.SYNONYME));
//		// aus Peschke-DA:
//		entityList.add(new ProcessEntity("", "", null, "Kunde ben�tigt Cloud-Service"));
//		entityList.add(new ProcessEntity("", "", null, "Dienstleister erh�lt Serviceanfrage"));
//		entityList.add(new ProcessEntity("", "", null, "Eingabe personenbezogener Daten")); // rot
//		entityList.add(new ProcessEntity("", "", null, "Verschl�sselung und Authentikation")); // rot
//		entityList.add(new ProcessEntity("", "", null, "Pr�fung der Daten"));
//		entityList.add(new ProcessEntity("", "", null, "nicht in Ordnung"));
//		entityList.add(new ProcessEntity("", "", null, "Cloud verweigert Dienstleistung"));
//		entityList.add(new ProcessEntity("", "", null, "in Ordnung"));
//		entityList.add(new ProcessEntity("", "", null, "Cloud erbringt Dienstleistung"));
//		entityList.add(new ProcessEntity("", "", null, "Antwort per E-Mail")); // gelb
//		entityList.add(new ProcessEntity("", "", null, "Berechtigung")); // ?
//		entityList.add(new ProcessEntity("", "", null, "Kontrolle")); // ?
//		entityList.add(new ProcessEntity("", "", null, "Kontrolle von Einstellungen")); // ?
//		ProcessEntity mailEnt = new ProcessEntity("", "", null, "Brieffach"); // Pattern
//																			// werden
//																			// nur
//																			// �ber
//																			// Expansion
//																			// nach
//																			// "E-Mail"
//																			// gefunden
//
//		WSExpander expander = null;
//		try {
//				// nutzt nur die Internet-Verbindung
//			expander = new WSExpander(WSExpander.ExpanderMode.NOT_USE_CACHE);
//			boolean mailExpRes = expander.calcExtensions(mailEnt, false, true);
//			assertTrue(mailExpRes);
//			entityList.add(mailEnt); // ?
//
//		} catch (WrongElementException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		
//
//		
//		analyser = new PeschkeAnalyser();
//
//		for (ProcessEntity curEntity : entityList) {
////			System.out.println("N�chste entity: " + curEntity.getTexts());
//			System.out.println("[" + curEntity.getTexts().get(0).getEntityText() + "]");
//
//			result = analyser.getRelevantPatterns(catalog, curEntity, filters);
//
//			// System.out.println(result);
//
//			System.out.println("Relevante Pattern: " + result.getAllRelevantPatternNamesAndTitles());
//			System.out.println("Relevante Worte: " + result.getAllRelevantWords());
//
//			System.out.println("Es wurden " + result.size() + " relevante Pattern identifiziert\n");
//		}
//
//		// assertEquals(1, result.size());
//		// assertEquals("BSIGrundschutz_G4.33",
//		// result.iterator().next().getPattern().getName());
//		// assertEquals("BSIGrundschutz_G4.33",
//		// result.getAllRelevantPatternNames().iterator().next());
//		// assertEquals("Authentikation",
//		// result.iterator().next().getRelevantWords().iterator().next());
//		// assertEquals("Authentikation",
//		// result.getAllRelevantWords().iterator().next());
//
//		System.out.println("Richtiger Test Ende");
//
//		System.out.println("Test mit Textdatei:");
//		catalog = new PatternCatalog();
//		catalog.createFromOntology("resources/ontology1.owl", BSIOnto.ALL, MARiskOnto.ALL, LAWOnto.All);
//
//		TextFileImporter importer = new TextFileImporter("resources/rf_activities.txt");
//		ProcessDescription fileModel = importer.doImport();
//		System.out.println(fileModel.getEntities().size() + " Aktivit�ten gelesen");
//		if (expander != null) {
//			expander.calcExtensions(fileModel, false, true);
//		}
//
//		analyser = new PeschkeAnalyser();
//
//		for (ProcessEntity curEntity : fileModel.getEntities()) {
//			System.out.println("N�chste entity: " + curEntity.getTexts());
//
//			result = analyser.getRelevantPatterns(catalog, curEntity, filters);
//
//			// System.out.println(result);
//
//			System.out.println("Relevante Pattern: " + result.getAllRelevantPatternNamesAndTitles());
//			System.out.println("Relevante Worte: " + result.getAllRelevantWords());
//
//			System.out.println("Es wurden " + result.size() + " relevante Pattern identifiziert\n");
//		}
//
//		System.out.println("Ende Textdateitest");
//
//	}
//
//}
