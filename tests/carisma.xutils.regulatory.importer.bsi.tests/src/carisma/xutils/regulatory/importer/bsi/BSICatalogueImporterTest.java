package carisma.xutils.regulatory.importer.bsi;



import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;


import org.apache.xerces.util.AttributesProxy;
import org.junit.Test;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

import com.sun.org.apache.xerces.internal.parsers.AbstractSAXParser;

import carisma.xutils.regulatory.importer.bsi.datamodel.BSICatalogue;
import carisma.xutils.regulatory.importer.bsi.datamodel.BSICategory;




/**
 * JUnit test-case for the BSICatalogueImporter.
 * @author Klaus Rudack
 *
 */
public class BSICatalogueImporterTest {

	/**
	 * this test tests the startElement() method with level = 1.
	 */
	@Test
	public final void testStartElement() {
		String uri = "";
		String localName = "";
		String qName = "a";
//		AttributesProxy atts = null;
//		TODO KR: wie bekomme ich hier die richtige  klasse?
//		com.sun.org.apache.xerces.internal.parsers.AbstractSAXParser$AttributesProxy
		Attributes atts = new AttributesImpl(); //Damit der Test laeuft bis die richtige Implementation genutzt wird.
		BSICatalogueImporter bsiCI = new BSICatalogueImporter();
		BSICatalogue bsiC;
		bsiCI.setLevel(1);
		try {
			bsiCI.startElement(uri, localName, qName, atts);
		} catch (SAXException e) {
			e.printStackTrace();
			fail("SAXException has been thrown in startElement(..)!");
		}
		bsiC = bsiCI.getCurrentCatalogue();
	}
	
	
	/**
	 * this test tests the doImport() method.
	 */
	@Test
	public final void testDoImport() {
		String firstCat = "Übergreifende Aspekte";
		String secondCat = "Organisatorische Mängel";
		String firstCatID = "G 2";
		String secondCatID = "B 1";
		String filename = "resources/bsisource";
		File file = new File(filename);
		assertTrue(file.exists());
		BSICatalogue bsiC = null;
		BSICatalogueImporter bsiCI = new BSICatalogueImporter();
		assertNotNull(bsiCI);
		try {
			bsiCI.doImport(file);
		} catch (IOException e) {
			e.printStackTrace();
			fail("IOException has been thrown in doImport(..)!");
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			fail("PaserConfigurationException has been thrown in doImport(..)!");			
		} catch (SAXException e) {
			e.printStackTrace();
			fail("SAXException has been thrown in doImport(..)!");
		}
		bsiC = bsiCI.getCurrentCatalogue();
		assertNotNull(bsiC);
		assertNotNull(bsiC.getCategories());
		assertEquals(2, bsiC.getCategories().size());
		BSICategory bsiCat1 = (BSICategory) bsiC.getCategories().toArray()[0];
		BSICategory bsiCat2 = (BSICategory) bsiC.getCategories().toArray()[1];
		assertNotNull(bsiCat1);
		assertNotNull(bsiCat2);
		System.out.println(bsiCat1.getName());
		System.out.println(bsiCat2.getName());
		assertTrue((bsiCat1.getName().equals(firstCat)) || (bsiCat2.getName().equals(firstCat)));
		assertTrue((bsiCat1.getName().equals(secondCat)) || (bsiCat2.getName().equals(secondCat)));
		assertTrue((bsiCat1.getId().equals(firstCatID)) || (bsiCat2.getId().equals(firstCatID)));
		assertTrue((bsiCat1.getId().equals(secondCatID)) || (bsiCat2.getId().equals(secondCatID)));
		assertNotNull(bsiCat1.getEntries());
		assertNotNull(bsiCat2.getEntries());
		assertEquals(1, bsiCat1.getEntries().size());
		assertEquals(1, bsiCat2.getEntries().size());
	}

}
