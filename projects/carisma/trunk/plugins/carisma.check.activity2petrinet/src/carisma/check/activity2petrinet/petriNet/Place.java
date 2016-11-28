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
 * This class models a place of a petri net.
 * @author Kubi Mensah 
 *
 */
public class Place extends PetriNetObject {
	
	@Override
	public boolean equals(Object obj) {
		return this == obj; //TODO: reicht das so, oder koennen Referenzen unterschiedlich sein?
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	/**
	 * The number of tokens on this place at initialization of the net.
	 **/
	private int initialMarking = 0;
	
	
	/**
	 * Constructs a place of a petri net with the given parameters.
	 * @param type should be set to 'place'
	 * @param id the id of the place
	 * @param graphics the graphics object of the place
	 * @param name the name of the place
	 */
	public Place(final String type, final String id, final Graphics graphics, final String name) {
		super(type, id, graphics, name);
		
	}
	
	/**
	 * Constructs a place of a petri net with the given parameters.
	 * @param id the id of the place
	 * @param graphics the graphics object of the place
	 * @param name the name of the place
	 */
	public Place(final String id, final Graphics graphics, final String name) {
		super("place", id, graphics, name);
		
	}


	/**
	 * Constructs a place of a petri net with the given parameters.
	 * @param type should be set to 'place'
	 * @param id the id of the place
	 * @param graphics the graphics object of the place
	 * @param name the name of the place
	 * @param initialMarking The number of initial tokens
	 */
	public Place(final String type, final String id, final Graphics graphics, final String name, final int initialMarking) {
		super(type, id, graphics, name);
		this.initialMarking = initialMarking;
	}
	
	/**
	 * Constructs a place of a petri net with the given parameters.
	 * @param id the id of the place
	 * @param graphics the graphics object of the place
	 * @param name the name of the place
	 * @param initialMarking The number of initial tokens
	 */
	public Place(final String id, final Graphics graphics, final String name, final int initialMarking) {
		super("place", id, graphics, name);
		this.initialMarking = initialMarking;
	}

	@Override
	public String toString(){ 
		return this.getId();
	}

	public int getInitialMarking() {
		return this.initialMarking;
	}

	public void setInitialMarking(int initialMarking) {
		this.initialMarking = initialMarking;
	}
}
