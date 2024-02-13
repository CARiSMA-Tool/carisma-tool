package carisma.tool.evolution.uml2.umlchange.datatype;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import carisma.evolution.uml2.umlchange.datatype.ElementDescription;
import carisma.evolution.uml2.umlchange.datatype.GrammarAlternative;
import carisma.evolution.uml2.umlchange.datatype.GrammarBlock;
import carisma.evolution.uml2.umlchange.datatype.SimpleElementDescription;


public class GrammarBlockTest {

	/**
	 * Constant String for the name of a class.
	 */
	private static final String BLA = "BLA";
	
	/**
	 * Constant String for the the key 'name'.
	 */
	private static final String NAME = "name";
	
	/**
	 * MetaClassName 'Class'.
	 */
	private static final String CLASS = "Class";
	
	@Test
	public void testSingleAlternative() {
		String grammar = "{Class(" + NAME + "=" + BLA + "),Stereotype(name=blubb,someKey=Zing)}";
		GrammarBlock parsedGrammar = new GrammarBlock(grammar);
		assertEquals(grammar, parsedGrammar.getGrammarString());
		assertEquals(1, parsedGrammar.getAlternatives().size());
		GrammarAlternative theAlt = parsedGrammar.getAlternatives().get(0);
		assertEquals(grammar, theAlt.getGrammarString());
		assertEquals(2, theAlt.getDescriptions().size());
		ElementDescription first = theAlt.getDescriptions().get(0);
		ElementDescription second = theAlt.getDescriptions().get(1);
		assertEquals("Class(" + NAME + "=" + BLA + ")", first.getGrammarString());
		assertEquals("Stereotype(" + NAME + "=blubb,someKey=Zing)", second.getGrammarString());
		assertTrue(first instanceof SimpleElementDescription);
		assertTrue(second instanceof SimpleElementDescription);
		SimpleElementDescription firstSED = (SimpleElementDescription) first;
		SimpleElementDescription secondSED = (SimpleElementDescription) second;
		assertEquals(CLASS, firstSED.getMetaclassName());
		assertEquals("Stereotype", secondSED.getMetaclassName());
		assertEquals(0, firstSED.getContainedElements().size());
		assertEquals(0, secondSED.getContainedElements().size());
		assertEquals(1, firstSED.getKeyValuePairs().size());
		assertEquals(2, secondSED.getKeyValuePairs().size());
		assertEquals(BLA, firstSED.getKeyValuePairs().get(NAME));
		assertEquals("blubb", secondSED.getKeyValuePairs().get(NAME));
		assertEquals("Zing", secondSED.getKeyValuePairs().get("someKey"));
	}

	@Test
	public void testTwoAlternatives() {
		String grammar = "{Class(name=BLA)},{Stereotype(name=blubb,someKey=Zing)}";
		GrammarBlock parsedGrammar = new GrammarBlock(grammar);
		assertEquals(grammar, parsedGrammar.getGrammarString());
		assertEquals(2, parsedGrammar.getAlternatives().size());
		GrammarAlternative firstAlt = parsedGrammar.getAlternatives().get(0);
		GrammarAlternative secondAlt = parsedGrammar.getAlternatives().get(1);
		assertEquals("{Class(name=BLA)}", firstAlt.getGrammarString());
		assertEquals("{Stereotype(name=blubb,someKey=Zing)}", secondAlt.getGrammarString());		
		assertEquals(1, firstAlt.getDescriptions().size());
		assertEquals(1, secondAlt.getDescriptions().size());
		ElementDescription first = firstAlt.getDescriptions().get(0);
		ElementDescription second = secondAlt.getDescriptions().get(0);
		assertEquals("Class(name=BLA)", first.getGrammarString());
		assertEquals("Stereotype(name=blubb,someKey=Zing)", second.getGrammarString());
		assertTrue(first instanceof SimpleElementDescription);
		assertTrue(second instanceof SimpleElementDescription);
		SimpleElementDescription firstSED = (SimpleElementDescription) first;
		SimpleElementDescription secondSED = (SimpleElementDescription) second;
		assertEquals(CLASS, firstSED.getMetaclassName());
		assertEquals("Stereotype", secondSED.getMetaclassName());
		assertEquals(0, firstSED.getContainedElements().size());
		assertEquals(0, secondSED.getContainedElements().size());
		assertEquals(1, firstSED.getKeyValuePairs().size());
		assertEquals(2, secondSED.getKeyValuePairs().size());
		assertEquals(BLA, firstSED.getKeyValuePairs().get(NAME));
		assertEquals("blubb", secondSED.getKeyValuePairs().get(NAME));
		assertEquals("Zing", secondSED.getKeyValuePairs().get("someKey"));
	}

	@Test
	public void testOneAlternativeWithContent() {
		String grammar = "{Class(name=BLA,contents=<Operation(name=someOp),Property(name=someAtt)>)}";
		GrammarBlock parsedGrammar = new GrammarBlock(grammar);
		assertEquals(grammar, parsedGrammar.getGrammarString());
		assertEquals(1, parsedGrammar.getAlternatives().size());
		GrammarAlternative firstAlt = parsedGrammar.getAlternatives().get(0);
		assertEquals(grammar, firstAlt.getGrammarString());
		assertEquals(1, firstAlt.getDescriptions().size());
		ElementDescription first = firstAlt.getDescriptions().get(0);
		assertEquals("Class(name=BLA,contents=<Operation(name=someOp),Property(name=someAtt)>)", first.getGrammarString());
		assertTrue(first instanceof SimpleElementDescription);
		SimpleElementDescription firstSED = (SimpleElementDescription) first;
		assertEquals(CLASS, firstSED.getMetaclassName());
		assertEquals(1, firstSED.getKeyValuePairs().size());
		assertEquals(BLA, firstSED.getKeyValuePairs().get(NAME));
		assertEquals(2, firstSED.getContainedElements().size());
		assertEquals(BLA, firstSED.getKeyValuePairs().get(NAME));
		assertEquals(null, firstSED.getKeyValuePairs().get("contents"));
		SimpleElementDescription firstContained = firstSED.getContainedElements().get(0);
		SimpleElementDescription secondContained = firstSED.getContainedElements().get(1);
		
		assertEquals("Operation(name=someOp)", firstContained.getGrammarString());
		assertEquals("Operation", firstContained.getMetaclassName());
		assertEquals(1, firstContained.getKeyValuePairs().size());
		assertEquals(0, firstContained.getContainedElements().size());
		assertEquals("someOp", firstContained.getKeyValuePairs().get(NAME));
		
		assertEquals("Property(name=someAtt)", secondContained.getGrammarString());
		assertEquals("Property", secondContained.getMetaclassName());
		assertEquals(1, secondContained.getKeyValuePairs().size());
		assertEquals(0, secondContained.getContainedElements().size());
		assertEquals("someAtt", secondContained.getKeyValuePairs().get(NAME));
		
	}
	
	@Test
	public void testOneAlternativeThreeLevelContent() {
		String grammar = "{Class(name=BLA,contents=<Operation(name=someOp,contents=<Property(name=someAtt," 
				+ "contents=<NotPossible(name=NIX)>,visibility=public)>)>)}";
		GrammarBlock parsedGrammar = new GrammarBlock(grammar);
		assertEquals(grammar, parsedGrammar.getGrammarString());
		assertEquals(1, parsedGrammar.getAlternatives().size());
		GrammarAlternative firstAlt = parsedGrammar.getAlternatives().get(0);
		assertEquals(grammar, firstAlt.getGrammarString());
		assertEquals(1, firstAlt.getDescriptions().size());
		ElementDescription firstLayer = firstAlt.getDescriptions().get(0);
		assertEquals("Class(name=BLA,contents=<Operation(name=someOp,contents=<Property(name=someAtt," 
				+ "contents=<NotPossible(name=NIX)>,visibility=public)>)>)", firstLayer.getGrammarString());
		assertTrue(firstLayer instanceof SimpleElementDescription);
		SimpleElementDescription firstSED = (SimpleElementDescription) firstLayer;
		assertEquals(CLASS, firstSED.getMetaclassName());
		assertEquals(1, firstSED.getContainedElements().size());
		assertEquals(1, firstSED.getKeyValuePairs().size());
		assertEquals(BLA, firstSED.getKeyValuePairs().get(NAME));
		assertEquals(null, firstSED.getKeyValuePairs().get("contents"));
		
		SimpleElementDescription secondLayer = firstSED.getContainedElements().get(0);
		
		assertEquals("Operation(name=someOp,contents=<Property(name=someAtt,contents=<NotPossible(name=NIX)>,visibility=public)>)", 
				secondLayer.getGrammarString());
		assertEquals("Operation", secondLayer.getMetaclassName());
		assertEquals(1, secondLayer.getKeyValuePairs().size());
		assertEquals(1, secondLayer.getContainedElements().size());
		assertEquals("someOp", secondLayer.getKeyValuePairs().get(NAME));

		SimpleElementDescription thirdLayer = secondLayer.getContainedElements().get(0);
		assertEquals("Property(name=someAtt,contents=<NotPossible(name=NIX)>,visibility=public)", thirdLayer.getGrammarString());
		assertEquals("Property", thirdLayer.getMetaclassName());
		assertEquals(2, thirdLayer.getKeyValuePairs().size());
		assertEquals(1, thirdLayer.getContainedElements().size());
		assertEquals("someAtt", thirdLayer.getKeyValuePairs().get(NAME));
		assertEquals("public", thirdLayer.getKeyValuePairs().get("visibility"));
		
		SimpleElementDescription lastLayer = thirdLayer.getContainedElements().get(0);
		assertEquals("NotPossible(name=NIX)", lastLayer.getGrammarString());
		assertEquals("NotPossible", lastLayer.getMetaclassName());
		assertEquals(1, lastLayer.getKeyValuePairs().size());
		assertEquals(0, lastLayer.getContainedElements().size());
		assertEquals("NIX", lastLayer.getKeyValuePairs().get(NAME));
	}
	
	@Test
	public void testForgotRoundBrackets() {
		String grammarString = "{name=" + BLA + "}";
		GrammarBlock block = new GrammarBlock(grammarString);
		assertEquals(grammarString, block.getGrammarString());
		assertEquals(1, block.getAlternatives().size());
		GrammarAlternative alt = block.getAlternatives().get(0);
		assertEquals(grammarString, alt.getGrammarString());
//		assertEquals(1, alt.getDescriptions().size());
		SimpleElementDescription sed = (SimpleElementDescription) alt.getDescriptions().get(0);
		assertEquals("name=" + BLA, sed.getGrammarString());
		assertEquals(BLA, sed.getKeyValuePairs().get(NAME));
	}
}
