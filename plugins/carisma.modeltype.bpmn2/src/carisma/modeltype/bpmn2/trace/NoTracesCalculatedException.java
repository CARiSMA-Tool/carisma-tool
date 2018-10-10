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
package carisma.modeltype.bpmn2.trace;

/**
 * NoTracesCalculatedException.
 * @author Marcel Michel
 */
public class NoTracesCalculatedException extends Exception {

	/**
	 * Serial Version UID.
	 */
	private static final long serialVersionUID = 7509902327512059388L;

	/**
	 * Constructor.
	 * @param desc The description
	 */
	public NoTracesCalculatedException(final String desc) {
		super(desc);
	}
	

}
