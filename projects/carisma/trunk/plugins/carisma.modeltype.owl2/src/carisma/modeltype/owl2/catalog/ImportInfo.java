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
package carisma.modeltype.owl2.catalog;

import java.net.URI;

import org.semanticweb.owlapi.model.IRI;

public class ImportInfo {

	private IRI importLocation;
	private URI physicalLocation;
	
	public IRI getImportLocation() {
		return importLocation;
	}
	public void setImportLocation(IRI importLocation) {
		this.importLocation = importLocation;
	}
	public URI getPhysicalLocation() {
		return physicalLocation;
	}
	public void setPhysicalLocation(URI physicalLocation) {
		this.physicalLocation = physicalLocation;
	}
}
