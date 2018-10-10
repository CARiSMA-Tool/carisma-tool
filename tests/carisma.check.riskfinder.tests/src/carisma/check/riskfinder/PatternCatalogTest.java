//package carisma.check.riskfinder;
//
//import static org.junit.Assert.assertEquals;
//import static org.junit.Assert.assertNotNull;
//
//import java.io.File;
//import java.io.IOException;
//
//import javax.naming.NameParser;
//
//import org.junit.Test;
//
//import carisma.check.riskfinder.PatternCatalog.BSIOnto;
//import carisma.check.riskfinder.PatternCatalog.LAWOnto;
//import carisma.check.riskfinder.PatternCatalog.MARiskOnto;
//
//import static org.junit.Assert.assertNull;
//import com.csvreader.CsvReader;
//import com.csvreader.CsvWriter;
//
//public class PatternCatalogTest {
//
//	@Test
//	public final void test() {
//		PatternCatalog catalog = new PatternCatalog();
//
////		out.println(System.getProperty("user.dir"));
//		
//		catalog.createFromOntology("resources/Entire_Law_Ontology.owl", BSIOnto.ALL, MARiskOnto.ALL, LAWOnto.All);
//		assertNotNull(catalog);
//				
//		assert ("[(BSIGrundschutz_G4.33 : ??? : Authentikation ist total wichtig), (BSIGrundschutz_G1.1 : testtitel : testtext)]".equals(catalog.toString())
//				|| 
//				"[(BSIGrundschutz_G1.1 : testtitel : testtext), (BSIGrundschutz_G4.33 : ??? : Authentikation ist total wichtig)]".equals(catalog.toString())
//				);
//		
//		Pattern pat = catalog.getPatternByName("B_3.103");
//		Pattern expected = new Pattern("B_3.103", "Server unter Windows NT", 
//				"\nB 3.103 Server unter Windows NT\nBeschreibung\n\nDieser Baustein ist 2009 mit der 11. " 
//		+ "Ergänzungslieferung entfallen.\n\nDie letzte Version des Bausteins, die mit der 10. Ergänzungslieferung veröffentlicht wurde, " 
//		+ "kann weiterhin unter den Hilfsmitteln zum IT-Grundschutz auf den BSI-Webseiten abgerufen werden.\n\n");
//		assertNotNull(pat);
//		String isValue = pat.toString();
//		assertEquals("(B_3.103 : Server unter Windows NT : \nB 3.103 Server unter Windows NT\nBeschreibung\n\nDieser Baustein ist 2009 mit der 11. " 
//		+ "Ergänzungslieferung entfallen.\n\nDie letzte Version des Bausteins, die mit der 10. Ergänzungslieferung veröffentlicht wurde, " 
//		+ "kann weiterhin unter den Hilfsmitteln zum IT-Grundschutz auf den BSI-Webseiten abgerufen werden.\n\n)", isValue);
////		assertEquals(expected, pat);
//	}
//	
//	/** Test the getPatternByName(String):Pattern   method with input 'null'.
//	 * 
//	 */
//	@Test
//	public final void getPatternByNameNullTest() {
//		PatternCatalog catalog = new PatternCatalog();
//		catalog.createFromOntology("resources/Entire_Law_Ontology.owl", BSIOnto.ALL, MARiskOnto.ALL, LAWOnto.All);
//		assertNull(catalog.getPatternByName(null));
//	}
//	
//	/** Test the getPatternByName(String):Pattern   method with an nonexisting Pattern to look for.
//	 * 
//	 */
//	@Test
//	public final void getPatternByNameNonExistingPatternTest() {
//		PatternCatalog catalog = new PatternCatalog();
//		catalog.createFromOntology("resources/Entire_Law_Ontology.owl", BSIOnto.ALL, MARiskOnto.ALL, LAWOnto.All);
//		assertNull(catalog.getPatternByName("NonExistingPattern_for_su3e"));
//	}
//
//	
//	/** Test whether the the method returns the correct Pattern.
//	 * Input is the Name of an existing Pattern.
//	 */
//	@Test
//	public final void getPatternByNameExistingPatternTest() {
//		PatternCatalog catalog = new PatternCatalog();
//		catalog.createFromOntology("resources/Entire_Law_Ontology.owl", BSIOnto.ALL, MARiskOnto.ALL, LAWOnto.All);
//		Pattern actual = catalog.getPatternByName("G_5.46");
//		assertNotNull(actual);
//		assertEquals("(G_5.46 : Maskerade unter WfW : \nG 5.46 Maskerade unter WfW\n" 
//				+ "Diese Gefährdung ist 2009 mit der 11. Ergänzungslieferung entfallen.\n\n\n)", actual.toString());
//	}
//	
//	/**
//	 * This test test the createMARiskFromOntology() method.
//	 * Implemented by KR.
//	 */
//	@Test
//	public final void testCreateMARiskFromOntology() {
//		PatternCatalog catalog = new PatternCatalog();
//		File file = new File("resources/Entire_Law_Ontology.owl");
//		catalog.createMARiskFromOntology(file, MARiskOnto.ALL);
//	}
//	
//	/**
//	 * This test test the createLawFromOntology() method.
//	 * Implemented by KR.
//	 */
//	@Test
//	public final void testCreateLawFromOntology() {
//		PatternCatalog catalog = new PatternCatalog();
//		File file = new File("resources/Entire_Law_Ontology.owl");
//		catalog.createLawFromOntology(file, LAWOnto.All);
//	}
//
//	/**
//	 * Hilfsfunktion, um die Tabelle der BSI-Measure fuer Cloudat zu erstellen
//	 * @throws IOException
//	 */
//	@Test
//	public final void testcreateBSIFromOntology() throws IOException {
//		PatternCatalog catalog = new PatternCatalog();
//		File file = new File("resources/Entire_Law_Ontology.owl");
//		catalog.createBSIFromOntology(file, BSIOnto.MEASURE);
//		System.out.println("bla");
//
//		CsvWriter writer = new CsvWriter("bsimeasures.csv");
//				
//		for(Pattern curPattern : catalog){
////			System.out.println(curPattern);
//			System.out.println(curPattern.getName());
//			System.out.println(curPattern.getTitle());
//			
//			String title = curPattern.getName();
//			String[] nameSplit = title.split("\\.");
//			nameSplit[1] = ("000" + nameSplit[1]);
//			nameSplit[1] = nameSplit[1].substring(nameSplit[1].length() - 3, nameSplit[1].length());
////			System.out.println(nameSplit[1]);
//						
//			writer.write(nameSplit[0] + "." + nameSplit[1]);
//			writer.write(curPattern.getTitle());
//			
//			// zwei Verantwortlichkeiten
//			String respPrefix = "Verantwortlich für Initiierung";
//			String execPrefix = "Verantwortlich für Umsetzung";
//			String respInit = "";
//			String respExec = "";
//			String[] lines = curPattern.getText().split("\\n");
//			for(String curLine : lines){
//				if(curLine.startsWith(respPrefix)){
//					respInit = curLine.substring(respPrefix.length() + 2, curLine.length());
////					System.out.println(respInit);
//				}else if(curLine.startsWith(execPrefix)){
////					respExec = curLine;
//					respExec = curLine.substring(execPrefix.length() + 2, curLine.length());
//					System.out.println(respExec);
//				}
//			}
//			writer.write(respInit);
//			writer.write(respExec);
//			
//			
//			writer.endRecord();
//			
////			System.out.println(curPattern.getText());
//		}
//		
//		writer.close();
//	}
//}
