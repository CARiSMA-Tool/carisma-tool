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
package carisma.profile.umlsec.rabac;

import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.CarismaProfileDescriptor;


public enum UMLsecRABAC {
	
	SECURE_DEPENDENCY ("secure dependency"),
	DATA_SECURITY ("data security"),
	SECURE_LINKS ("secure links"),
	NO_UP_FLOW ("no up-flow"),
	NO_DOWN_FLOW ("no down-flow"),
	GUARDED_ACCESS ("guarded access"),
	RBAC ("rbac"),
	CRITICAL ("critical"),
	SEND ("send"),
	SECRECY ("secrecy"),
	INTEGRITY ("integrity"),
	HIGH ("high"),
	ENCRYPTED ("encrypted"),
	WIRE ("wire"),
	INTERNET ("Internet"),
	LAN ("LAN"),
	POS_DEVICE ("POS device"),
	ISSUER_NODE ("issuer node"),
	SMART_CARD ("smart card"),
	PROVABLE ("provable"),
	FAIR_EXCHANGE ("fair exchange"),
	SEPERATION_OF_DUTY ("seperation of duty"),
	ALLOWED_USERS ("allowed users"),
	SAP_TRANSACTION ("SAP Transaction"),
	USED_BY ("used-by"),
	PROTECTED_ACTION ("protected action"),
	AUTHORIZED_STATUS ("authorized-status"),
	LOCKED_STATUS ("locked-status"),
	IDENTIFIABLE ("identifiable"),
	GUARDED ("guarded"),
	CALL ("call"),
	REQUIRES ("requires"),
	
	ABAC("abac"),
	ABACREQUIRE ("abacRequire"),
	ABACATTRIBUTE ("abacAttribute");
	
	public static final String TAG_CRITICAL_HIGH = "high";
	public static final String TAG_CRITICAL_SECRECY = "secrecy";
	public static final String TAG_CRITICAL_INTEGRITY = "integrity";
	
	private static final String PROFILE_NAME = "RABAC";
	private static final String PROFILE_VERSION = "1";
	
	public static final CarismaProfileDescriptor DESCRIPTOR = new CarismaProfileDescriptor(PROFILE_NAME, PROFILE_VERSION, RabacActivator.UML_URI);

	
	private final String readableName;
	
	private UMLsecRABAC(final String newName) {
		this.readableName = newName;
	}
	
	@Override
	public String toString() {
		return this.readableName;
	}
	
	public static UMLsecRABAC getValue(final String name) {
		for (UMLsecRABAC type : UMLsecRABAC.values()) {
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
