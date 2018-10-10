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
package carisma.bpmn2.marisk.util;

import java.util.List;

/**
 * 
 * @author Marcel Michel
 * @param <T>
 *
 */
public class SoDEntity<T> {

	/**
	 * SeparatedActivities1.
	 */
	private List<T> separatedActivities1;
	
	/**
	 * SeparatedActivities2.
	 */
	private List<T> separatedActivities2;
	
	/**
	 * Constructor.
	 * @param activities1 SeparatedActivites1
	 * @param activities2 SeparatedActivites2
	 * @param <T1> extends <T>
	 */
	@SuppressWarnings("unchecked")
	public <T1 extends T> SoDEntity(final List<T1> activities1, final List<T1> activities2) {
		this.separatedActivities1 = (List<T>) activities1;
		this.separatedActivities2 = (List<T>) activities2;
	}
	
	/**
	 * Getter for separatedActivities1.
	 * @return seperatedActivities1
	 */
	public final List<T> getSeparatedActivities1() {
		return this.separatedActivities1;
	}

	/**
	 * Getter for separatedActivities2.
	 * @return seperatedActivities2
	 */
	public final List<T> getSeparatedActivities2() {
		return this.separatedActivities2;
	}
}
