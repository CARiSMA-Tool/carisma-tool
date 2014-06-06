package carisma.xutils.regulatory.importer.bsi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.Map;

import org.junit.Test;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.utils.GenericOntologyHelper;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.importer.bsi.datamodel.BSICatalogue;
import carisma.xutils.regulatory.importer.bsi.datamodel.BSICategory;
import carisma.xutils.regulatory.importer.bsi.datamodel.BSIEntry;
import carisma.xutils.regulatory.importer.bsi.datamodel.BSIEntry.EntryStatus;
import carisma.xutils.regulatory.importer.bsi.datamodel.DataRoot;



/**
 * JUnit test-case for the BSIExtractor.
 * @author Klaus Rudack
 *
 */
public class BSItest {

	
	/**
	 * this test tests the extract(File sourceFolder) method with a null-file.
	 */
	@Test
	public final void testExtractNullFile() {
		BSIExtractor be = new BSIExtractor();
		RegulatoryOntologyHelper roh = be.extract(null);
		assertNotNull(roh);
		assertEquals(18, roh.getOntology().getAxiomCount());
	}
	
	/**
	 * this test tests the extract(File sourceFolder) method with a wrong sourcefolder-file.
	 */
	@Test
	public final void testExtractWrongSourceFolderFile() {
		String path = "resources/wrongFolder";
		File sourceFolder = new File(path);
		assertTrue(sourceFolder.exists());
		BSIExtractor be = new BSIExtractor();
		RegulatoryOntologyHelper roh = be.extract(sourceFolder);
		assertNotNull(roh);
		assertEquals(18, roh.getOntology().getAxiomCount());
	}
	
	
	/**
	 * this test tests the extract(File sourceFolder) method of the BSIExtractor.
	 */
	@Test
	public final void testExtract() {
		String sourcePath =  "resources/bsisource";
		File sourceFolder = new File(sourcePath);
		assertNotNull(sourceFolder);
		assertTrue(sourceFolder.exists());
		BSIExtractor be = new BSIExtractor();
		RegulatoryOntologyHelper roh1 = be.extract(sourceFolder);
		RegulatoryOntologyHelper roh2 = new RegulatoryOntologyHelper();
		assertNotNull(roh1);
		assertNotNull(roh2);
		assertFalse(roh1.equals(roh2));
		assertEquals(25, roh1.getOntology().getAxiomCount());
	}
	
	
	/**
	 * this test tests the extract(File sourceFolder, RegulatoryOntologyHelper ontologyContainer) method.
	 * it calls the method with extract(null, null)
	 */
	@Test
	public final void testExtractNullNull() {
		BSIExtractor be = new BSIExtractor();
		be.extract(null, null);
		//hier kann weiter nichts getestet werden, ausser ob das Programm durchlaeuft.
	}
	
	/**
	 * this test tests the extract(File sourceFolder, RegulatoryOntologyHelper ontologyContainer) method.
	 * it calls the method with extract(File, null);
	 */
	@Test
	public final void testExtractFileNull() {
		String sourcePath =  "resources/bsisource";
		File sourceFolder = new File(sourcePath);
		BSIExtractor be = new BSIExtractor();
		be.extract(sourceFolder, null);
		//hier kann weiter nichts getestet werden, ausser ob das Programm durchlaeuft.
	}
	
	
	/**
	 * this test tests the extract(File sourceFolder, RegulatoryOntologyHelper ontologyContainer) method.
	 * it calls the method with extract(null, RegulatoryOntologyHelper);
	 */
	@Test
	public final void testExtractNullRegulatoryOntologyHelper() {
		BSIExtractor be = new BSIExtractor();
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper();
		assertNull(roh.getOntology());
		be.extract(null, roh);
		assertNull(roh.getOntology());
	}
	
	/**
	 * this test tests the extract(File sourceFolder, RegulatoryOntologyHelper ontologyContainer) method.
	 * it calls the method with extract(File, RefulatoryOntologyHelper)
	 */
	@Test
	public final void testExtractCorrect() {
		OWLNamedIndividual owlNI = null;
		String sourcePath =  "resources/bsisource";
		File sourceFolder = new File(sourcePath);
		GenericOntologyHelper goh = new GenericOntologyHelper();
		BSIExtractor be = new BSIExtractor();
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		assertNotNull(roh);
		assertNull(roh.getOntology());
		roh.createNewRegulatoryOntology();
		assertNotNull(roh.getOntology());
		owlNI = goh.getIndividualById("G_2.66");
		assertNull(owlNI);
		be.extract(sourceFolder, roh);
		assertNotNull(roh.getOntology());
		assertEquals(25, roh.getOntology().getAxiomCount());
		owlNI = goh.getIndividualById("G_2.66");
		assertNotNull(owlNI);
	}
	
	/**
	 * this test tests the loadBsiCataloogue method with an incorrect source.
	 */
	@Test
	public final void testLoadBsiCatalogueFalseSource() {
		String sourcePathFalse =  "resources/";
		File sourceFolderFalse = new File(sourcePathFalse);
		BSIExtractor be = new BSIExtractor();
		assertNull(be.getCatalogueRoot());
		assertFalse(be.loadBsiCatalogue(sourceFolderFalse));
		assertNull(be.getCatalogueRoot().getBsiCatalogue());
	}
	
	
	/**
	 * this test tests the loadBsiCatalogue method of the BSIExtractor.
	 */
	@Test
	public final void testLoadBsiCatalogue() {
		String sourcePathCorrect =  "resources/bsisource";
		File sourceFolderCorrect = new File(sourcePathCorrect);
		BSIExtractor be = new BSIExtractor();
		assertNull(be.getCatalogueRoot());
		assertTrue(be.loadBsiCatalogue(sourceFolderCorrect));
		assertEquals(2, be.getCatalogueRoot().getBsiCatalogue().getCategories().size());
	}
	
	/**
	 * this test tests the createBSIIndividuals() method with null as DataRoot.
	 */
	@Test
	public final void testCreateBSIIndividualsNoDataRoot() {
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper();
		roh.createNewRegulatoryOntology();
		assertNotNull(roh);
		BSIExtractor be = new BSIExtractor();
		assertNull(be.createBSIIndividuals(null, roh));
	}
	
	/**
	 * this test tests the createBSIIndividuals() method with an empty DataRoot.
	 */
	@Test
	public final void testCreateBSIIndividualsEmptyDataRoot() {
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper();
		roh.createNewRegulatoryOntology();
		assertNotNull(roh);
		BSIExtractor be = new BSIExtractor();
		DataRoot dr = new DataRoot();
		assertNull(be.createBSIIndividuals(dr, roh));
	}
	
	/**
	 * this test tests the createBSIIndividuals() method with an empty catalog.
	 */
	@Test
	public final void testCreateBSIIndividualsEmptyCatalog() {
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper();
		roh.createNewRegulatoryOntology();
		assertNotNull(roh);
		BSIExtractor be = new BSIExtractor();
		BSICatalogue bsiC = new BSICatalogue();
		DataRoot dr = new DataRoot();
		dr.setBsiCatalogue(bsiC);
		assertNotNull(be.createBSIIndividuals(dr, roh));
		assertEquals(0, be.createBSIIndividuals(dr, roh).size());

	}
	
	/**
	 * this test tests the createBSIIndividuals() method with null as RegulatoryOntologyHelper.
	 */
	@Test
	public final void testCreateBSIIndividualsNoRegulatoryOntologyHelper() {
		BSICategory bsiCat = new BSICategory("sample_name", "B", "sample_URL");
		BSIEntry bsiE = new BSIEntry("sample_title2", "B2", EntryStatus.valid, "sample_text2", "sample_URL2");
		bsiCat.addEntry(bsiE);
		BSICatalogue bsiC = new BSICatalogue();
		bsiC.addCategory(bsiCat);
		DataRoot dr = new DataRoot();
		dr.setBsiCatalogue(bsiC);
		BSIExtractor be = new BSIExtractor();
		assertNull(be.createBSIIndividuals(dr, null));
	}
	
	/**
	 * this test tests the createBSIIndividuals() method.
	 */
	@Test
	public final void testCreateBSIIndividuals() {
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper();
		roh.createNewRegulatoryOntology();
		assertNotNull(roh);
		BSIExtractor be = new BSIExtractor();
		BSICategory bsiCat = new BSICategory("sample_name", "B", "sample_URL");
		BSIEntry bsiE = new BSIEntry("sample_title2", "B2", EntryStatus.valid, "sample_text2", "sample_URL2");
		bsiCat.addEntry(bsiE);
		BSICatalogue bsiC = new BSICatalogue();
		bsiC.addCategory(bsiCat);
		DataRoot dr = new DataRoot();
		dr.setBsiCatalogue(bsiC);
		Map<String, OWLNamedIndividual> idBsiEntryMapping = be.createBSIIndividuals(dr, roh);
		assertEquals(1,  idBsiEntryMapping.size());
		String iD = (String) idBsiEntryMapping.keySet().toArray()[0];
		assertTrue(idBsiEntryMapping.get(iD).toString().contains("B2"));
	}
	
	/**
	 * this test tests if umlaute in a text-file will be generated correctly.
	 */
	@Test
	public final void testUTF8() {
		String toTest = "";
		String prop = "BSIRule/Content";
		OWLNamedIndividual owlNI = null;
		String sourcePath =  "resources/bsisource";
		File sourceFolder = new File(sourcePath);
		GenericOntologyHelper goh = new GenericOntologyHelper();
		BSIExtractor be = new BSIExtractor();
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		assertNotNull(roh);
		assertNull(roh.getOntology());
		roh.createNewRegulatoryOntology();
		assertNotNull(roh.getOntology());
		owlNI = goh.getIndividualById("G_2.66");
		assertNull(owlNI);
		be.extract(sourceFolder, roh);
		assertNotNull(roh.getOntology());
		assertEquals(25, roh.getOntology().getAxiomCount());
		owlNI = goh.getIndividualById("G_2.66");
		assertNotNull(owlNI);
		try {
			toTest = goh.getAnnotation(owlNI, prop).toString();
		} catch (NoSuchPropertyException e) {
			e.printStackTrace();
			fail("OWLNamdIndividual has no property: " + prop);
		}
		assertTrue(toTest.contains("ü"));
	}
	
//	/**
//	 * this method checks if a given string is UTF8-formated.
//	 * @param string the string to check
//	 * @return true if the given string is UTF8-formated, false otherwise
//	 */
//	private boolean check2(final String string) {
//		char[] ch = {'a', 'b', 'c', 'd', 'e', 'f', 'g'};
//		char[] test = string.toCharArray();
//		for (char testChar : test) {
//			boolean  returnvalue = false;
//			for (int i = 0; i < ch.length; i++) {
//				if (testChar == ch[i]) {
//					returnvalue = true;
//				}
//			}
//			if (!returnvalue) {
//				return false;
//			}
//		}
//		return true;
//	}
	
}
