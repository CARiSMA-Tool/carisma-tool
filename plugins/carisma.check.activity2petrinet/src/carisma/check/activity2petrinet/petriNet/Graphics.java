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
 * This class provides the graphical coordinates for petri net objects.
 * @author Kubi Mensah
 *
 */
public class Graphics {
	/**
	 * The x coordinate for the position of the object.
	 */
	private int positionX;
	/**
	 * The y coordinate for the position of the object.
	 */
	private int positionY;
	/**
	 * The x coordinate for the dimension of the object.
	 */
	private int dimensionX;
	/**
	 * The y coordinate for the dimension of the object.
	 */
	private int dimensionY;
	
	/** 
	 * This constructs a graphic object given the x and y coordinates for the position and dimension of the petri net obect.
	 * @param pX x coordinate of the position
	 * @param pY y coordinate of the position
	 * @param dX x coordinate of the dimension
	 * @param dY y coordinate of the dimension
	 */
	public Graphics(final int pX, final int pY, final int dX, final int dY) {
		setPositionX(pX);
		setPositionY(pY);
		setDimensionX(dX);
		setDimensionY(dY);
	
	}

	/**
	 * Sets the x coordinate for the position of the object.
	 * @param positionX the x coordinate for the position of the object
	 */
	public final void setPositionX(final int positionX) {
		this.positionX = positionX;
	}

	/**
	 * Returns the x coordinate for the position of the object.
	 * @return returns the x coordinate for the position of the object
	 */
	public final int getPositionX() {
		return this.positionX;
	}

	/**
	 * Sets the y coordinate for the position of the object.
	 * @param positionY the y coordinate for the position of the object
	 */
	public final void setPositionY(final int positionY) {
		this.positionY = positionY;
	}

	/**
	 * Returns the y coordinate for the position of the object.
	 * @return the y coordinate for the position of the object.
	 */
	public final int getPositionY() {
		return this.positionY;
	}

	/**
	 * Sets the x coordinate for the Dimension of the object.
	 * @param dimensionX the x coordinate for the Dimension of the object.
	 */
	public final void setDimensionX(final int dimensionX) {
		this.dimensionX = dimensionX;
	}

	/**
	 * Returns the x coordinate for the Dimension of the object.
	 * @return the x coordinate for the Dimension of the object.
	 */
	public final int getDimensionX() {
		return this.dimensionX;
	}

	/**
	 * Sets the y coordinate for the Dimension of the object.
	 * @param dimensionY the y coordinate for the Dimension of the object.
	 */
	public final void setDimensionY(final int dimensionY) {
		this.dimensionY = dimensionY;
	}

	/**
	 * Returns the y coordinate for the Dimension of the object.
	 * @return the y coordinate for the Dimension of the object.
	 */
	public final int getDimensionY() {
		return this.dimensionY;
	}
}
