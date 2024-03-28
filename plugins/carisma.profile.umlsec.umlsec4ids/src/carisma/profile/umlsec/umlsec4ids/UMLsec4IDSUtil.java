/*******************************************************************************
 * Copyright (c) 2022 RGSE group, University of Koblenz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {Alexander Peikert}
 *******************************************************************************/
package carisma.profile.umlsec.umlsec4ids;

import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.CarismaProfileDescriptor;
import carisma.profile.common.AbstractUMLsecUtil;


/**
 * The implementation of the UMLsec profile.
 * Provides easy access to the profile's stereotypes
 * and to applications of those.
 * @author Alexander Peikert
 *
 */
public final class UMLsec4IDSUtil extends AbstractUMLsecUtil {
	
	private static final String PROFILE_NAME = "umlsec4ids";
	private static final String PROFILE_VERSION = "1";
	private static final String PROFILE_URI = "platform:/plugin/carisma.profile.umlsec.umlsec4ids/profile/umlsec4ids.profile.uml";

	public static final CarismaProfileDescriptor DESCRIPTOR = new CarismaProfileDescriptor(PROFILE_NAME, PROFILE_VERSION, PROFILE_URI);

 
    /** Hide constructor.
     */
    private UMLsec4IDSUtil() {
    }
	
	public static boolean checkIfProfileContains(Stereotype stereo) {
		return UMLsec4IDS.contains(stereo);
	}
	

}