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
package carisma.modeltype.bpmn2.extended.util;

/**
 * 
 * @author Marcel Michel
 *
 * @param <T>
 * @param <V>
 */
public class Tupel<T, V> {
	
	/** object1. */
	private T object1;
	
	/** object 2. */
	private V object2;

	/**
	 * 
	 * @param o1 object1
	 * @param o2 object2
	 */
	public Tupel(final T o1, final V o2) {
		this.object1 = o1;
		this.object2 = o2;
	}

	/**
	 * 
	 * @return object1
	 */
	public final T getO1() {
		return this.object1;
	}

	/**
	 * 
	 * @return object2
	 */
	public final V getO2() {
		return this.object2;
	}
}