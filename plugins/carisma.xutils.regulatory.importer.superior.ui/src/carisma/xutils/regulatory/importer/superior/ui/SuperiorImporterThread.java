/*******************************************************************************
 * Copyright (c) 2012 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.xutils.regulatory.importer.superior.ui;

import carisma.xutils.regulatory.importer.superior.SuperiorImporter;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;

/**
 * This class extends the Thread class to execute the superior importer as a thread.
 * @author jkowald
 */
public class SuperiorImporterThread extends Thread {
	
	/**
	 * A SuperiorImporter instance.
	 */
	private SuperiorImporter importer;
	
	/**
	 * The path to the folder where the ontology will be saved.
	 */
	private String ontologySavepath = "";
	
	/**
	 * Constructor.
	 * @param logInput A LogViewEntrySet instance
	 * @param newSourceFolderPath The folder which contains the input data
	 * @param ontologyFilename The name of the ontology file
	 */
	public SuperiorImporterThread(
			final LogViewEntrySet logInput, 
			final String newSourceFolderPath,
			final String ontologyFilename) {
		ontologySavepath = ontologyFilename;
		this.importer = new SuperiorImporter(logInput, newSourceFolderPath, ontologyFilename);
	}
	
	/**
	 * This method contains the functionality of the thread.
	 */
	public final void run() {
		if (importer != null) {
			importer.createRegulatoryOntology();
			importer.saveOntology(ontologySavepath);
		} else {
			System.err.println("parseRegulatoryDocuments method called without instantiating the class SuperiorImporter!");
		}
	}
}
