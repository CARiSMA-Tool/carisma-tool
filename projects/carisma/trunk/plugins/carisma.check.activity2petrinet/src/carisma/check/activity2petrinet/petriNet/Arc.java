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
 * This class models an arc in a petri net.
 * @author Kubi Mensah
 *
 */
public class Arc {
	/**
	 * The type of this petri net object.
	 */
	private String type;
	/**
	 * The ID of this arc.
	 */
	private String id;
	/**
	 * The ID of the source Node. 
	 */
	private String source;
	/**
	 * The ID of the target NOde.
	 */
	private String target;

	
	
	/**
	 * Contructs a new arc object.
	 * @param id The ID of this arc.
	 * @param source The ID of the source Node.
	 * @param target The ID of the target NOde.
	 */
	public Arc(final String id, final String source, final String target) {
		this.setSource(source);
		this.setTarget(target);
		this.setId(id);
		this.setType("arc");
		
	}
	
	/**
	 * Contructs a new arc object.
	 * @param id The ID of this arc.
	 * @param source The ID of the source Node.
	 * @param target The ID of the target NOde.
	 * @author thumberg
	 */
	public Arc(final String id, final PetriNetObject source, final PetriNetObject target) {
		this.setSource(source.getId());
		this.setTarget(target.getId());
		this.setId(id);
		this.setType("arc");
	}

	/**
	 * Sets the source node ID.
	 * @param source ID of the source node
	 */
	public final void setSource(final String source) {
		this.source = source;
	}

	/**
	 * Returns the ID of the source node.
	 * @return ID of the source node
	 */
	public final String getSource() {
		return this.source;
	}

	/**
	 * Sets the target node ID.
	 * @param target ID of the target node
	 */
	public final void setTarget(final String target) {
		this.target = target;
	}

	/**
	 * Returns the ID of the target node.
	 * @return the ID of the target node
	 */
	public final String getTarget() {
		return this.target;
	}

	/**
	 * Sets the type of this object.
	 * @param type the type of the object
	 */
	public final void setType(final String type) {
		this.type = type;
	}

	
	/**
	 * Returns the type of this object.
	 * @return the type of the object
	 */
	public final String getType() {
		return this.type;
	}

	/** 
	 * Sets the ID of this arc object.
	 * @param id ID of the arc
	 */
	public final void setId(final String id) {
		this.id = id;
	}

	/**
	 * Retunrs the ID of this arc object.
	 * @return ID of this arc object
	 */
	public final String getId() {
		return this.id;
	}

}
