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
import carisma.core.checks.CarismaCheck;
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
public class LockedStatus implements CarismaCheck {
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
					preCondition = s.getQualifiedName();
				} else {
					preCondition = ((XMLResource) host.getAnalyzedModel()).getID(s);
				}
				postCondition = preCondition;
				command = TripleConstants.SET_STATUS;
				Writer w = null;
				try {
					output = new File(tripleFile, fileName);
					w = new OutputStreamWriter(new FileOutputStream(output), "UTF-8");
					w.write(triple());
				}  catch (UnsupportedEncodingException e) {
					host.appendLineToReport("UnsupportetdEncoding at " + tripleFile.getPath());
					result = false;
				} catch (FileNotFoundException e) {
					host.appendLineToReport("FileNotFound  " + fileName);
					result = false;
				} catch (IOException e) {
					host.appendLineToReport("IOException");
					result = false;
				} finally {
				    try {
				        if (w != null) {
				            w.close();
				        }
				    } catch (IOException e) {
	                    host.appendLineToReport("IOException");
	                    result = false;
				    }
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
		return ("{state={" + preCondition + "}} " + TripleConstants.SEPERATOR + command + TripleConstants.SEPERATOR + "{state={" + postCondition + "}}");
	}

	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost newHost) {
		
		if (newHost != null) {
			host = newHost;			
		} else {
			host = new DummyHost(true);
		}
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
		    host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (!(currentModel.getContents().get(0) instanceof Package)) {
		    host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			return false;
		}
		
		Package model = (Package) currentModel.getContents().get(0);
		

/*		for (State s : UMLHelper.getAllElementsOfType(model, State.class)) {
			System.out.println(((XMLResource) currentModel).getID(s) + "     " + s.getLabel());
		}
	*/	
		FolderParameter cx = null;
		try {
			cx = (FolderParameter) parameters.get("carisma.check.lockedexporter.output");
			if (!cx.getValue().mkdirs()) {
			    host.appendLineToReport("Couldn't create Folder");
			}
		} catch (ClassCastException e) {
		    host.appendLineToReport("Couldn't create Folder");
			try {
			    host.appendLineToReport("FolderParameter  " + cx.getValue().getCanonicalPath());
			} catch (IOException e1) {
				Logger.log(LogLevel.ERROR, "" , e1);
			}
		}
		
		return export(model, cx.getValue(), ((BooleanParameter) parameters.get("carisma.check.lockedexporter.qualified")).getValue());
	}
}