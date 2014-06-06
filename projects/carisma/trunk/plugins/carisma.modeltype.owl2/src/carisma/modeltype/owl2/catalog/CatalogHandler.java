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

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.protege.xmlcatalog.CatalogUtilities;
import org.protege.xmlcatalog.XMLCatalog;
import org.protege.xmlcatalog.entry.Entry;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.util.SimpleIRIMapper;

/**
 * Parses and Handles XML-Catalog files. 
 * @author Marcel Michel
 */
public class CatalogHandler {

	private XMLCatalog catalog = null;
	private File workspaceroot = null;
	
	public CatalogHandler(File workspaceroot) {
        try {
        	this.workspaceroot = workspaceroot;
        	if (getCatalogFile().exists()) {
        		catalog = CatalogUtilities.parseDocument(getCatalogFile().toURI().toURL());
        	}
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public List<SimpleIRIMapper> getImportIRIMappers() {
		List<SimpleIRIMapper> list = new ArrayList<SimpleIRIMapper>();
		ImportVisitor importVisitor = new ImportVisitor();
		if (catalog != null) {
			for (Entry e : catalog.getEntries()) {
				e.accept(importVisitor);
			}
			for (ImportInfo i : importVisitor.getImports()) {
				list.add(new SimpleIRIMapper(i.getImportLocation(),
		                IRI.create(i.getPhysicalLocation())));
			}
		}
		return list;
	}
	
	private File getCatalogFile() {
		//TODO: Return list with all catalogs
		File catalogFile = new File(workspaceroot + "/" + "catalog-v001.xml");
			return catalogFile;
	}
	
}
