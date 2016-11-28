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
package carisma.check.exporter_for_schema_generation;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.State;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;

/** Exports {preCondition} command {postCondition} Triple, w.r.t. the authorized-status stereotype .
 * 
 * @author bberghoff
 *
 */
public class LockedStatusTripleExporter implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.hoartriple.exporter.lockedstatus";
	public static final String PARAM_OUTPUT_FOLDER = "carisma.check.lockedexporter.output";
	public static final String PARAM_USE_QUALIFIED_NAMES = "carisma.check.lockedexporter.qualified";
	public static final String CHECK_NAME = "UMLsec locked-status Triple Exporter";
	
	/**
     * condition which is required before the command 'command' is executed.
     */
	private String preCondition;
	/**
	 *  the Command to be executed.
	 */
	private String command;
	
	/**
	 * postContition condition which required after the command 'command' has been executed.
	 */
	private String postCondition;
	
	/** instance of the current host plug-in.
	 */
	private AnalysisHost host = null;
	
	/** Writes the {T} S {Q} Triple, w.r.t. the locked-status stereotype, into a File.
	 * 
	 * @param model 
	 * @param tripleFile Folder where the File is saved to
	 * @param qualified uses the qualified-name of the elements if qualified is true, else the XMI-ID. 
	 * @return only true if no Exception is caught
	 */
	public final boolean export(final Package model, final File tripleFile, final boolean qualified) {
		boolean result = true;
		for (State s : UMLHelper.getAllElementsOfType(model, State.class)) {
			if (UMLsecUtil.hasStereotype(s, UMLsec.LOCKED_STATUS)) {
				String fileName = s.getName() + "_LockedStatus";
				File output;
				if (qualified) {
					this.preCondition = s.getQualifiedName();
				} else {
					this.preCondition = ((XMLResource) this.host.getAnalyzedModel()).getID(s);
				}
				this.postCondition = this.preCondition;
				this.command = TripleConstants.SET_STATUS;
				output = new File(tripleFile, fileName);
				try (Writer w = new OutputStreamWriter(new FileOutputStream(output), "UTF-8")){
					w.write(triple());
				}  catch (UnsupportedEncodingException e) {
					this.host.appendLineToReport("UnsupportetdEncoding at " + tripleFile.getPath());
					result = false;
				} catch (FileNotFoundException e) {
					this.host.appendLineToReport("FileNotFound  " + fileName);
					result = false;
				} catch (IOException e) {
					this.host.appendLineToReport("IOException");
					result = false;
				}
			}
		}
		return result;
	}
	
	/** Little Helper method.
	 * 
	 * @return A String for the output of the triple.
	 */
	private String triple() {
		return ("{state={" + this.preCondition + "}} " + TripleConstants.SEPERATOR + this.command + TripleConstants.SEPERATOR + "{state={" + this.postCondition + "}}");
	}

	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost newHost) {
		
		if (newHost != null) {
			this.host = newHost;			
		} else {
			this.host = new DummyHost(true);
		}
		Resource currentModel = this.host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
		    this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (!(currentModel.getContents().get(0) instanceof Package)) {
		    this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			return false;
		}
		
		Package model = (Package) currentModel.getContents().get(0);
		

/*		for (State s : UMLHelper.getAllElementsOfType(model, State.class)) {
			System.out.println(((XMLResource) currentModel).getID(s) + "     " + s.getLabel());
		}
	*/	
		FolderParameter cx = null;
		File value = null;
		try {
			cx = (FolderParameter) parameters.get(PARAM_OUTPUT_FOLDER);
			value = cx.getValue();
			if (!value.mkdirs()) {
			    this.host.appendLineToReport("Couldn't create Folder");
			}
		} catch (ClassCastException e) {
		    this.host.appendLineToReport("Couldn't create Folder");
			try {
			    String canonicalPath;
				if(value != null){
					canonicalPath = value.getCanonicalPath();
				}
				else{
					System.err.println("carisma.check.exporter_for_schema_generation.LockedStatusTripleExporter cx.getValue() is null.");
					canonicalPath = "";
				}
				this.host.appendLineToReport("FolderParameter  " + canonicalPath);
			} catch (IOException e1) {
				Logger.log(LogLevel.ERROR, "" , e1);
			}
		}
		
		return export(model, value, ((BooleanParameter) parameters.get(PARAM_USE_QUALIFIED_NAMES)).getValue());
	}

	@Override
	public String getCheckID() {
		return CHECK_ID;
	}

	@Override
	public String getName() {
		return CHECK_NAME;
	}
}