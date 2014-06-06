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
package carisma.clean.popup.actions;

import java.io.IOException;
import java.util.HashMap;

import org.eclipse.core.resources.IFile;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Profile;
import org.eclipse.uml2.uml.ProfileApplication;

import carisma.core.Carisma;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;

/**
 * Plugin for removing all UMLsec references from
 * a model.
 * @author Daniel Warzecha
 *
 */
public class RemoveUMLsec implements IObjectActionDelegate {

	private IFile selectedFile;
	private Shell shell;
	
	/**
	 * Constructor for Action1.
	 */
	public RemoveUMLsec() {
		super();
	}

	/**
	 * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
	 */
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		shell = targetPart.getSite().getShell();
	}

	/**
	 * @see IActionDelegate#run(IAction)
	 */
	public void run(IAction action) {
		if (selectedFile != null && !Carisma.getInstance().getCheckRegistry().findChecks("UML2").isEmpty()) {
			removeUMLsec(selectedFile);
		} else {
			Logger.log(LogLevel.WARNING, "No plugins found!");
		}
	}

	/**
	 * @see IActionDelegate#selectionChanged(IAction, ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		if (selection instanceof IStructuredSelection) {
            IStructuredSelection structuredSelection = (IStructuredSelection) selection;
            selectedFile = (IFile) structuredSelection.getFirstElement();
        }
	}
	/**
	 * Removes all UMLsec references from the given model file.
	 * @param modelfile - the file to clean
	 */
	public void removeUMLsec(final IFile modelfile) {
		try {
			if (modelfile != null && modelfile.exists()) {
				Resource modelresource = loadModel(modelfile);
				Model model = (Model) modelresource.getContents().get(0);
				removeUMLsec(model);
				String modifiedFilename = modifyFilename(modelfile, "_noUMLsec");
				saveCleanedModel(modelresource, modifiedFilename);
				modelresource.unload();
			}
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
		}
	}
	/**
	 * The given model is modified by removing all
	 * UMLsec stereotype applications and UMLsec
	 * profile applications.
	 * @param model - model to clean
	 */
	private static void removeUMLsec(Model model) {
		if (UMLHelper.isProfileApplied(model, UMLsec.DESCRIPTOR)) {
			for (StereotypeApplication umlsecApp : UMLsecUtil.getStereotypeApplications(model)) {
				umlsecApp.getExtendedElement().unapplyStereotype(umlsecApp.getAppliedStereotype());
			}
			for (ProfileApplication profileApp : model.getAllProfileApplications()) {
				Profile appliedProfile = profileApp.getAppliedProfile();
				if (appliedProfile.getName().contains(UMLsec.DESCRIPTOR.getProfileName())) {
					profileApp.getApplyingPackage().unapplyProfile(appliedProfile);
				}
			}
		}		
	}
	/**
	 * Loads the model from the given file.
	 * @param file - the model file
	 * @return - the model resource
	 * @throws IOException - if the file does not exist
	 */
	private static Resource loadModel(final IFile file) throws IOException {
		URI uri = URI.createFileURI(file.getFullPath().toOSString());
		ResourceSet resourceSet = new ResourceSetImpl();
		Resource resource = resourceSet.getResource(uri, true);
		resource.load(new HashMap<String, Object>());
		return resource;
	}
	/**
	 * Saves the given model resource under the given filename.
	 * Doing this the URI of the resource is changed according to the
	 * filename.
	 * @param modelresource - the model resource to save
	 * @param saveFilename - the filename to use
	 */
	private static void saveCleanedModel(Resource modelresource, final String saveFilename) {
		if (!saveFilename.isEmpty()) {
			String oldname = modelresource.getURI().lastSegment();
			String newpath = modelresource.getURI().toFileString().replaceFirst(oldname, saveFilename);
			URI newUri = URI.createFileURI(newpath);
			modelresource.setURI(newUri);
		}
		try {
			modelresource.save(new HashMap<String, Object>());
		} catch (IOException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
		}
	}
	/**
	 * Given a file and a suffix, this returns
	 * the filename appended with a suffix.
	 * For example, given an old filename FILE.EXT,
	 * FILEsuffix.EXT is returned
	 * @param file - the file to use
	 * @param suffix - the suffix to append
	 * @return - the modified filename
	 */
	public String modifyFilename(final IFile file, final String suffix) {
		if (file != null) {
			String modifiedName = getPureFilename(file) + suffix + getPureFileExtension(file);
			return modifiedName;			
		}
		return "";
	}
	/**
	 * Returns the filename without its extension.
	 * @param file - the file to use
	 * @return - the filename without extension
	 */
	public String getPureFilename(final IFile file) {
		String oldfilename = file.getName();
		String extension = getPureFileExtension(file);
		return oldfilename.substring(0, oldfilename.lastIndexOf(extension));
	}
	/**
	 * Returns the file's extension prepended with a "." 
	 * @param file - the file to use
	 * @return - "." + the file's extension
	 */
	public String getPureFileExtension(final IFile file) {
		return "." + file.getFileExtension();
	}
	
}
