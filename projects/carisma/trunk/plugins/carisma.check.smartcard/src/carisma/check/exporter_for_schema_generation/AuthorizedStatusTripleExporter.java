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
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;

/** Exports {T} S {Q} Triple, w.r.t. the authorized-status stereotype .
 * 
 * @author bberghoff
 *
 */
public class AuthorizedStatusTripleExporter implements CarismaCheckWithID {
	
	public static final String CHECK_ID = "carisma.check.hoartriple.exporter.authorized-status";
	public static final String PARAM_OUTPUT_FOLDER = "carisma.check.authorizedexporter.output";
	public static final String PARAM_USE_QUALIFIED_NAMES = "carisma.check.authorizedexporter.qualified";
	public static final String CHECK_NAME = "UMLsec authorized-status Triple Exporter";
	
	/** 
	 * Precondition: State which has <<AuthorizedStatusTripleExporter>> applied to. 
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
				this.host.appendLineToReport("Hat AUTHORIZED_STATUS-------" + state.getQualifiedName()); 
				String fileName = state.getName() + "_AuthorizedStatus";
				File output;
				if (useQualifiedName) {
					this.preCondition = state.getQualifiedName();
				} else {	
					this.preCondition = ((XMLResource) this.host.getAnalyzedModel()).getID(state);		
				}
				Stereotype appliedAuthorizedStatus = state.getAppliedStereotype("UMLsec::authorized-status");
				String permission = (String) state.getValue(appliedAuthorizedStatus, "permission");
				this.postCondition = permission;
				this.command = TripleConstants.SET_STATUS;
				
				output = new File(tripleFile, fileName);

				try(Writer writer = new OutputStreamWriter(new FileOutputStream(output), "UTF-8")){
					writer.write(triple(permission));
					this.host.appendLineToReport(output.getCanonicalPath());
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
	
	/** Create output.
	 * @param permission permission specified in the TaggedValue "permission" in <<AuthorizedStatusTripleExporter>>.
	 * @return return formated output.
	 */
	private String triple(final String permission) {
		return ("{state!={" + this.preCondition + "} " + TripleConstants.AND + " "  + permission + " " + TripleConstants.IS_NOT_ELEMENT + " A.permissions} " 
				+ TripleConstants.SEPERATOR + this.command + TripleConstants.SEPERATOR + "{statusWord = Error_notPrivilege_" + this.postCondition + "}");
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

		FolderParameter cx = null;
		File value = null;
		try {
			cx = (FolderParameter) parameters.get(PARAM_OUTPUT_FOLDER);
			value = cx.getValue();
			if (!value.mkdirs()) {
	            this.host.appendLineToReport("Couldn't create Folder");
			}
		} catch (Exception e) {
			this.host.appendLineToReport("Couldn't create Folder");
			try {
				String canonicalPath;
				if(value!=null){
					canonicalPath = value.getCanonicalPath();
				}
				else {
					System.err.println("carisma.check.exporter_for_schema_generation.AuthorizedStatusTripleExporter: cx.getValue() is null.");
					canonicalPath = "";
				}
				this.host.appendLineToReport("FolderParameter  " + canonicalPath);
			} catch (IOException e1) {
				this.host.appendLineToReport("Error while Accessing the Value from the FolderParameter: \"" + value + "\"");
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