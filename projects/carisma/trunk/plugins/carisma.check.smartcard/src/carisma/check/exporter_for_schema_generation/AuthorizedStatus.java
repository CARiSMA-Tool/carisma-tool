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
import org.eclipse.uml2.uml.Stereotype;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;

/** Exports {T} S {Q} Triple, w.r.t. the authorized-status stereotype .
 * 
 * @author bberghoff
 *
 */
public class AuthorizedStatus implements CarismaCheck {
	/** 
	 * Precondition: State which has <<AuthorizedStatus>> applied to. 
	 */
	private String preCondition;
	
	/**
	 * Command.
	 */
	private String command;
	
	/**
	 * Postcondition.
	 */
	private String postCondition;
	
	/**
	 * Host which provides the model to be tested and which manages the output.
	 */
	private AnalysisHost host = null;
	
	/** Writes the {T} S {Q} Triple, w.r.t. the authorized-status stereotype, into a File.
	 * 
	 * @param model checked model.
	 * @param tripleFile Folder where the File is saved to
	 * @param useQualifiedName if true the output is in form of the qualifiedNames of the States, else its the XMI-ID 
	 * @return only true if no Exception is caught
	 */
	public final boolean export(final Package model, final File tripleFile, final boolean useQualifiedName) {
		boolean result = true;
		for (State state : UMLHelper.getAllElementsOfType(model, State.class)) {
			if (UMLsecUtil.hasStereotype(state, UMLsec.AUTHORIZED_STATUS)) {
				host.appendLineToReport("Hat AUTHORIZED_STATUS-------" + state.getQualifiedName()); 
				String fileName = state.getName() + "_AuthorizedStatus";
				File output;
				if (useQualifiedName) {
					preCondition = state.getQualifiedName();
				} else {	
					preCondition = ((XMLResource) host.getAnalyzedModel()).getID(state);		
				}
				Stereotype appliedAuthorizedStatus = state.getAppliedStereotype("UMLsec::authorized-status");
				String permission = (String) state.getValue(appliedAuthorizedStatus, "permission");
				postCondition = permission;
				command = TripleConstants.SET_STATUS;
				Writer writer = null;
				try {
					output = new File(tripleFile, fileName);

					writer = new OutputStreamWriter(new FileOutputStream(output), "UTF-8");
					writer.write(triple(permission));
					host.appendLineToReport(output.getCanonicalPath());
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
					if (writer != null) { 
						try {
							writer.close();
						} catch (IOException e) {
							host.appendLineToReport("IOException");
							result = false;
						}
					}
				}
			}
		}
		return result;
	}
	
	/** Create output.
	 * @param permission permission specified in the TaggedValue "permission" in <<AuthorizedStatus>>.
	 * @return return formated output.
	 */
	private String triple(final String permission) {
		return ("{state!={" + preCondition + "} " + TripleConstants.AND + " "  + permission + " " + TripleConstants.IS_NOT_ELEMENT + " A.permissions} " 
				+ TripleConstants.SEPERATOR + command + TripleConstants.SEPERATOR + "{statusWord = Error_notPrivilege_" + postCondition + "}");
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

		FolderParameter cx = null;
		try {
			cx = (FolderParameter) parameters.get("carisma.check.authorizedexporter.output");
			if (!cx.getValue().mkdirs()) {
	            host.appendLineToReport("Couldn't create Folder");
			}
		} catch (Exception e) {
			host.appendLineToReport("Couldn't create Folder");
			try {
				host.appendLineToReport("FolderParameter  " + cx.getValue().getCanonicalPath());
			} catch (IOException e1) {
				host.appendLineToReport("Error while Accessing the Value from the FolderParameter: \"" + cx.getValue() + "\"");
			}
		}
		return export(model, cx.getValue(), ((BooleanParameter) parameters.get("carisma.check.authorizedexporter.qualified")).getValue());
	}
}