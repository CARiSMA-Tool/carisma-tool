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
package carisma.check.activity2petrinet.petriNet;

/**
 * This class provides a static method to test the petri net.
 * @author Kubi Mensah
 *
 */
public final class PetriTest {
	
	/**
	 * Hide constructor.
	 */
	private PetriTest() {
	}
	
	/**
	 * @param args should be empty
	 */
	public static void main(final String[] args) {
		//creating a petri net for testing purposes-------------------------------------------------
		PetriNet petriNetTest = new PetriNet("petriNetTest", "www.test-url.com");
		
		petriNetTest.addPlace(new Place("place0", new Graphics(10, 20, 13, 13), "start"));
		petriNetTest.addPlace(new Place("place1", new Graphics(130, 20, 13, 13) , "mid0"));
		petriNetTest.addPlace(new Place("place2", new Graphics(250, 20, 13, 13), "mid1"));
		petriNetTest.addPlace(new Place("place3", new Graphics(370, 20, 13, 13) , "end"));
		
		petriNetTest.addTransition(new Transition("transition0", new Graphics(70, 20, 48, 25), "t0"));
		petriNetTest.addTransition(new Transition("transition1", new Graphics(190, 20, 48, 25), "t1"));
		petriNetTest.addTransition(new Transition("transition2", new Graphics(310, 20, 48, 25), "t2"));
		
		petriNetTest.addArc(new Arc("arc0", "place0", "transition0"));
		petriNetTest.addArc(new Arc("arc1", "transition0", "place1"));
		petriNetTest.addArc(new Arc("arc2", "place1", "transition1"));
		petriNetTest.addArc(new Arc("arc3", "transition1", "place2"));
		petriNetTest.addArc(new Arc("arc4", "place2", "transition2"));
		petriNetTest.addArc(new Arc("arc5", "transition2", "place3"));
		//-------------------------------------------------------------------------------------------
		
		
		Export export = new Export(petriNetTest);
		export.exportToPNML(System.getProperty("user.dir") + "/petriTest.pnml");

	}

}
