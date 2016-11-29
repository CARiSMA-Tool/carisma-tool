package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import carisma.evolution.uml2.umlchange.UMLchangeSyntax;

public class UMLchangeSyntaxTest {

	@SuppressWarnings("static-method")
	@Test
	public final void testExt() {
		assertTrue("refID=Stereotype".matches(UMLchangeSyntax.REGEX_EXT_VALUE));
		assertTrue("refID=Stereotype.Tag".matches(UMLchangeSyntax.REGEX_EXT_VALUE));
		
		assertFalse("Stereotype.Tag".matches(UMLchangeSyntax.REGEX_EXT_VALUE));
		assertFalse("refID=Stereotype.Tag.SubTag".matches(UMLchangeSyntax.REGEX_EXT_VALUE));
		assertFalse("refID=Stereotype1.Tag".matches(UMLchangeSyntax.REGEX_EXT_VALUE));
		assertFalse("refID=Stereotype.Tag1".matches(UMLchangeSyntax.REGEX_EXT_VALUE));	
	}
	
	@SuppressWarnings("static-method")
	@Test
	public final void testConstraint() {
		assertTrue("someRef=AND(otherRef),NOT(falseRef)".matches(UMLchangeSyntax.REGEX_CONSTRAINT_VALUE));
		assertTrue("someRef=AND(otherRef),NOT(falseRef)".matches(UMLchangeSyntax.REGEX_CONSTRAINT_VALUE));
		assertFalse("someRef=ANDotherRef,NOTfalseRef".matches(UMLchangeSyntax.REGEX_CONSTRAINT_VALUE));
		assertFalse("someRef=AND(otherRef),NOT(falseRef),BUT(someRef)".matches(UMLchangeSyntax.REGEX_CONSTRAINT_VALUE));
	}
	
	@SuppressWarnings("static-method")
	@Test
	public final void testNew() {
		assertTrue("refID={Class(name=.)}".matches(UMLchangeSyntax.REGEX_NEW_VALUE));
		assertTrue("refID={Class(name=newName)}".matches(UMLchangeSyntax.REGEX_NEW_VALUE));
		
		assertFalse("refID={Class(contents=bla)}".matches(UMLchangeSyntax.REGEX_NEW_VALUE));
		assertTrue("refID={Class(contents=<bla>)}".matches(UMLchangeSyntax.REGEX_NEW_VALUE));
		
		assertTrue("refID={Class(contentsbla=bla)}".matches(UMLchangeSyntax.REGEX_NEW_VALUE));
		assertTrue("refID={Class(blacontents=bla)}".matches(UMLchangeSyntax.REGEX_NEW_VALUE));	
	}
	
	@SuppressWarnings("static-method")
	@Test
	public final void testKeyValuePairs() {
		assertTrue("contents=<blabla>".matches(UMLchangeSyntax.REGEX_KEYVALUEPAIR));
		assertFalse("contents=blabla".matches(UMLchangeSyntax.REGEX_KEYVALUEPAIR));
		assertTrue("name=blabla".matches(UMLchangeSyntax.REGEX_KEYVALUEPAIR));
		assertTrue("blacontents=blabla".matches(UMLchangeSyntax.REGEX_KEYVALUEPAIR));
	}
}
