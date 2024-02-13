/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.tool.evolution.uml2.umlchange.datatype;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.junit.Test;

import carisma.evolution.uml2.umlchange.datatype.ParserUtils;



public class ParserUtilsTest {

	@Test
	public void testExtract() {
		ArrayList<String> parts = new ArrayList<>();
		parts.add("name=Klasse");
		assertEquals(parts,ParserUtils.extract("name=Klasse", ','));
		parts.add("contents=<Bla(name=Blubb)>");
		assertEquals(parts,ParserUtils.extract("name=Klasse,contents=<Bla(name=Blubb)>", ','));
	}

	@Test
	public void testFindMetaclassName() {
		assertEquals("Class",ParserUtils.findMetaclassName("Class(Stuff,MoreStuff)"));
		assertEquals("Test",ParserUtils.findMetaclassName("Test(Key=Value,OtherKey=Stuff)"));
		assertEquals("",ParserUtils.findMetaclassName("ClassStuff,MoreStuff)"));
		assertEquals("",ParserUtils.findMetaclassName("Class[Stuff,MoreStuff)"));
		assertEquals("Test",ParserUtils.findMetaclassName("Test(Key=Value,OtherKey=Stuff,contents=<Stereotype(name=blub)>)"));
	}

	@Test
	public void testFindNamespace() {
		assertEquals("Here",ParserUtils.findNamespace("@Here"));
		assertEquals("Here",ParserUtils.findNamespace("blabla,@Here"));
		assertEquals("There",ParserUtils.findNamespace("blabla,@There,agasehga"));
		assertEquals("There_Bla  fsgaz",ParserUtils.findNamespace("blabla,@There_Bla  fsgaz,agasehga"));
		assertNotSame("WOHOO", ParserUtils.findNamespace("@wohoo"));
		assertNotSame("WOHOO", ParserUtils.findNamespace(""));
		assertEquals("mainPackage::SubPackage", ParserUtils.findNamespace("copySomething ={@mainPackage::SubPackage(name=NewName),@mainPackage::OtherPackage(name=OtherNewName)"));
		//m�sste das dann nicht "mainPackage::SubPackage, mainPackage::OtherPackage" sein?
	}

	@Test
	public void testFindKeyValuePairs() {
		HashMap<String, String> keyValuePairs = new HashMap<>();
		keyValuePairs.put("name", "Klasse");
		keyValuePairs.put("visibility", "public");
		assertEquals(keyValuePairs,ParserUtils.findKeyValuePairs("(name=Klasse,visibility=public,free)"));
		keyValuePairs.put("contents", "<Attribute(name2=Ding)>");
		assertEquals(3,ParserUtils.findKeyValuePairs(
				"(name=Klasse,visibility=public,free,contents=<Attribute(name2=Ding)>)").size());
		assertEquals(keyValuePairs,ParserUtils.findKeyValuePairs(
				"(name=Klasse,visibility=public,free,contents=<Attribute(name2=Ding)>)"));		
	}

	@Test
	public void testReferenceValues() {
		List<String> testValues = new ArrayList<>();
		String ref = "someRef";
		testValues.add("someRef={Alt1},{Alt2}");
		testValues.add("notTheRef={Alt3},{Alt4}");
		testValues.add("someRef={Alt5}");
		testValues.add("someRef{Wrong}");
		assertEquals("{Alt1},{Alt2},{Alt5}",ParserUtils.getMatchingValues(ref, testValues));
		
		testValues = new ArrayList<>();
		ref = "someRef";
		testValues.add("someRef={Alt1},{Alt2}");
		testValues.add("notTheRef={Alt3},{Alt2}");
		testValues.add("someRef={Alt5}");
		testValues.add("someRef{Alt5}");
		assertEquals("{Alt1},{Alt2},{Alt5}",ParserUtils.getMatchingValues(ref, testValues));
		
	}
}
