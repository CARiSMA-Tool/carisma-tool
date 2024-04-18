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
package carisma.profile.umlsec.umlsec4ids;

import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.CarismaProfileDescriptor;


public enum UMLsec4IDS {
		
	BASEFREE ("basefree"),
	BASE ("base"),
	TRUST ("trust"),
	TRUSTPLUS ("trustplus"),
	X509 ("X.509"),
	X509TLS ("X.509TLS"),
	OWNER ("Owner"),
	CONSUMER ("Consumer"),
	DATAUSAGECONTROL ("datausagecontrol"),
	DATAPROVENANCETRACKING ("dataprovenancetracking"),
	VERIFIED ("verified"),
	ISOLATED ("isolated"),
	ENCRYPTION ("encryption"),
	CERTIFIED ("certified");
	
	private static final String PROFILE_NAME = "umlsec4ids";
	private static final String PROFILE_VERSION = "1";
	private static final String PROFILE_URI = "platform:/plugin/carisma.profile.umlsec.umlsec4ids/profile/umlsec4ids.profile.uml";

	public static final CarismaProfileDescriptor DESCRIPTOR = new CarismaProfileDescriptor(PROFILE_NAME, PROFILE_VERSION, PROFILE_URI);

	
	private final String readableName;
	
	private UMLsec4IDS(final String newName) {
		readableName = newName;
	}
	
	@Override
	public String toString() {
		return readableName;
	}
	
	public static UMLsec4IDS getValue(final String name) {
		for (UMLsec4IDS type : UMLsec4IDS.values()) {
			if (type.toString().equalsIgnoreCase(name)) {
				return type;
			}
		}
		return null;
	}

	/**
	 * Checks whether the given stereotype corresponds to the one represented by this enum literal.
	 * @param stereotype
	 * @return
	 */
	public boolean isEqual(Stereotype stereotype) {
		return contains(stereotype) && stereotype.getName().equalsIgnoreCase(this.readableName);
	}
	
	/**
	 * Checks whether the given Stereotype is a UMLsec stereotype.
	 * @param stereotype
	 * @return
	 */
	public static boolean contains(Stereotype stereotype) {
		return (stereotype.getProfile().getDefinition().getNsURI().contains(DESCRIPTOR.getProfileName()));
	}
	
}
