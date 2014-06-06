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
 * This class models a transition of a petri net.
 * @author Kubi Mensah
 *
 */
public class Transition extends PetriNetObject {
	
	/**
	 * Constructs a transtion of a petri net with the given parameters.
	 * @param type should be set to 'transition'
	 * @param id the id of the transition
	 * @param graphics the graphics object of the transition
	 * @param name the name of the transition
	 */
	public Transition(final String type, final String id, final Graphics graphics, final String name) {
		super(type, id, graphics, name);
		
	}
	
	/**
	 * Constructs a transtion of a petri net with the given parameters.
	 * @param id the id of the transition
	 * @param graphics the graphics object of the transition
	 * @param name the name of the transition
	 */
	public Transition(final String id, final Graphics graphics, final String name) {
		super("transition", id, graphics, name);
		
	}
}
