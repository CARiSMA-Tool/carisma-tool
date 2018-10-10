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
package carisma.check.staticcheck.securelinks;

import java.util.List;

import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Node;

import carisma.core.util.EObjectUtil;


public final class Messages {
	private Messages() {
	}
	
	static String nodesNotConnected(final Node sourceNode, final Node targetNode, final Dependency dep) {
		return "Even though "
				+ EObjectUtil.getTypeAndName(dep)
				+ " has requirements, there is no CommunicationPath between "
				+ EObjectUtil.getTypeAndName(sourceNode)
				+ " and "
				+ EObjectUtil.getTypeAndName(targetNode)
				+ ".";
	}
	
	static String secureLinksViolated(
			final String attacker,
			final CommunicationPath commPath,
			final List<String> violations) {
		StringBuffer bfr = new StringBuffer();
		for (String ability : violations) {
			bfr.append(ability);
			bfr.append("/");
		}
		bfr.deleteCharAt(bfr.lastIndexOf("/"));
		return attacker
				+ " attacker can "
				+ bfr.toString()
				+ " at "
				+ EObjectUtil.getTypeAndName(commPath)
				+ ".";
	}
}
