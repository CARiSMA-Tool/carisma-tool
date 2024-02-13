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

import org.junit.Test;

import carisma.evolution.uml2.umlchange.datatype.SimpleElementDescription;


public class SimpleElementDescriptionTest {

	@Test
	public void testSimpleElementDescription() {
		SimpleElementDescription testSimple =
			new SimpleElementDescription("Attribute(name=Bla,type=Blubb)");
		assertEquals("Attribute",testSimple.getMetaclassName());
		assertEquals(2, testSimple.getKeyValuePairs().size());
		SimpleElementDescription testContent =
			new SimpleElementDescription("Method(name=Bla,type=Blubb,contents=<Attribute(name=This)>)");
		assertEquals(2, testContent.getKeyValuePairs().size());
		assertEquals(1, testContent.getContainedElements().size());
		SimpleElementDescription thisElement = new SimpleElementDescription("Attribute(name=This)");
		SimpleElementDescription thatElement = new SimpleElementDescription("Attribute(name=That)");
		assertEquals(thisElement, testContent.getContainedElements().get(0));
		SimpleElementDescription testMoreContent =
			new SimpleElementDescription("Method(name=Bla,type=Blubb,contents=<Attribute(name=This),Attribute(name=That)>)");
		assertEquals(2, testMoreContent.getKeyValuePairs().size());
		assertEquals(2, testMoreContent.getContainedElements().size());
		assertEquals(thisElement, testMoreContent.getContainedElements().get(0));
		assertEquals(thatElement, testMoreContent.getContainedElements().get(1));
	}


}
