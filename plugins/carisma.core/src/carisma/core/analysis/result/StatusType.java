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
package carisma.core.analysis.result;

import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlEnumValue;

/**
 * @author buerger
 * @see AnalysisResultMessage
 * Enumeration representing a kind of result message
 */
@XmlEnum
public enum StatusType {

	/**
	 * Errors indicate that something went wrong, e.g. a check failed.
	 */
	@XmlEnumValue("ERROR")
	ERROR("ERROR"),
	/**
	 * Warnings are items the user should look at.
	 */
	@XmlEnumValue("WARNING")
	WARNING("WARNING"),
	/**
	 * Information that is helps the user to understand the analysis.
	 */
	@XmlEnumValue("INFO")
	INFO("INFO");

	/**
	 * The name of the status type.
	 */
	private String name;

	/**
	 * Constructor.
	 * @param name name of the status type
	 */
	StatusType(final String name) {
		this.name = name;
	}

	/**
	 * Returns the maximum of two status types.
	 * @param s1 a status type
	 * @param s2 another status type
	 * @return the maximum of both given status types
	 */
	public static StatusType max(final StatusType s1, final StatusType s2) {
		if ((s1 == ERROR) || (s2 == ERROR)) {
			return ERROR;
		}
		if ((s1 == WARNING) || (s2 == WARNING)) {
			return WARNING;
		}
		return INFO;
	}

	@Override
	public String toString() {
		return this.name;
	}

}
