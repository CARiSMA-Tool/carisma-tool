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
package carisma.profile.umlchange;

import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.CarismaProfileDescriptor;


public enum UMLchange { 

	CHANGE ("change"),
	CHANGEALL ("change-all"),
	ADD ("add"),
	ADDALL ("add-all"),
	DEL ("del"),
	DELALL ("del-all"),
	SUBST ("subst"),
	SUBSTALL ("subst-all"),
	OLD ("old"),
	KEEP ("keep"),
	EDIT ("edit"),
	COPY ("copy"),
	MOVE ("move");

	/**
	 * Short name of UMLchange profile.
	 */
	private static final String PROFILE_NAME = "UMLchange";
	private static final String PROFILE_VERSION = "2";
	private static final String PROFILE_URI = "platform:/plugin/carisma.profile.umlchange/profile/UMLchange.profile.uml";

	public static final CarismaProfileDescriptor DESCRIPTOR = new CarismaProfileDescriptor(PROFILE_NAME, PROFILE_VERSION, PROFILE_URI);

	/**
	 * The stereotype name of the UMLchange stereotype.
	 */
	private final String readableName;

	private UMLchange(final String newName) {
		this.readableName = newName;
	}
	
	@Override
	public String toString() {
		return this.readableName;
	}
	
	/**
	 * Returns the UMLchange value for the given stereotype name.
	 * @param name - name of wanted UMLchange stereotype
	 * @return - the UMLchange value or null if name is not a UMLchange stereotype
	 */
	public static UMLchange getValue(final String name) {
		for (UMLchange type : UMLchange.values()) {
			if (type.toString().equals(name)) {
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
		return contains(stereotype) && stereotype.getName().equals(this.readableName);
	}
	
	/**
	 * Checks whether the given Stereotype is a UMLchange stereotype.
	 * @param stereotype
	 * @return
	 */
	public static boolean contains(Stereotype stereotype) {
		return (stereotype.getProfile().getDefinition().getNsURI().contains(PROFILE_NAME));
	}
}
