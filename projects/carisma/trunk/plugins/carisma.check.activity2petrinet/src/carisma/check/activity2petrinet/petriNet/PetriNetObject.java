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
 * This class provides for common attributes of petri net objects.
 * @author Kubi Mensah
 *
 */
public abstract class PetriNetObject {
	
	/**
	 * The type of the petri net object.
	 */
	private String type;
	
	/**
	 * The Id of the petri net object.
	 */
	private String id;
	
	/**
	 * The graphics object contained by the petri net object.
	 */
	private Graphics graphics;
	
	/**
	 * The name of the petri net object.
	 */
	private String name;
	
	/**
	 * Constructs a petri net object with the given parameters.
	 * @param type the object type
	 * @param id the object id
	 * @param graphics the graphics object
	 * @param name the object name
	 */
	public PetriNetObject(final String type, final String id, final Graphics graphics, final String name) {
		this.setType(type);
		this.setId(id);
		this.setGraphics(graphics);
		this.setName(name);
	}

	/**
	 * Sets the type.
	 * @param type the object type
	 */
	public final void setType(final String type) {
		this.type = type;
	}

	/**
	 * Returns the type.
	 * @return type
	 */
	public final String getType() {
		return this.type;
	}

	/**
	 * Sets the Id. 
	 * @param id the object ID
	 */
	public final void setId(final String id) {
		this.id = id;
	}

	/**
	 * Returns the ID.
	 * @return the object id
	 */
	public final String getId() {
		return this.id;
	}

	/**
	 * Sets the graphics object.
	 * @param graphics the graphics object
	 */
	public final void setGraphics(final Graphics graphics) {
		this.graphics = graphics;
	}

	/**
	 * Returns the graphics object.
	 * @return graphics the graphics object
	 */
	public final Graphics getGraphics() {
		return this.graphics;
	}

	/**
	 * Sets the name.
	 * @param name the object name
	 */
	public final void setName(final String name) {
		this.name = name;
	}

	/**
	 * Returns the name.
	 * @return name the object name
	 */
	public final String getName() {
		return this.name;
	}

}
