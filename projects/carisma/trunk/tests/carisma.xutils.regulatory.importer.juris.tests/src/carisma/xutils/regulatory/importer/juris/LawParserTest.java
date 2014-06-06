package carisma.xutils.regulatory.importer.juris;

import static org.junit.Assert.*;

import java.util.List;

import org.junit.Test;

public class LawParserTest {

	@Test
	public void testGetParagraphNumbers() {
		String first = "23, 24, 27 und 28, 31 bis 37";
		String second = "83, 46 bis 56, 33, 55, 37 und 88, 98 bis 101";
		String third = "15a bis 16c";
		String fourth = "16a bis 16y";
		List<String> ergebnis = JurisFileParser.getParagraphNumbers(first);
		assertEquals(11, ergebnis.size());
		ergebnis = JurisFileParser.getParagraphNumbers(second);
		assertEquals(20, ergebnis.size());
		ergebnis = JurisFileParser.getParagraphNumbers(third);
		assertEquals(5, ergebnis.size());
		ergebnis = JurisFileParser.getParagraphNumbers(fourth);
		assertEquals(25, ergebnis.size());
	}
	
	@Test
	public void testExtractParagraphs() {
		assertEquals("16c", JurisFileParser.extractParagraph("16c", true));
		assertEquals("123", JurisFileParser.extractParagraph("123", false));
		assertNotSame("16c", JurisFileParser.extractParagraph("16c", false));
		assertEquals("123", JurisFileParser.extractParagraph("123", true));
	}
	
	@Test
	public void testGetIntegersWithHigherRange() {
		char[] alphabet = 	{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
				 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};
		String[] higherRange = new String[]{"16x", "17b"};
		assertEquals(4, JurisFileParser.getIntegersWithHigherRange(higherRange, alphabet));
	}
	
	@Test
	public void testGetIntegersWithSameRange() {
		char[] alphabet = 	{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
				 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};
		String[] sameRange = new String[]{"17c", "17f"};
		assertEquals(4, JurisFileParser.getIntegersWithSameRange(sameRange, alphabet, 0));
	}
}
