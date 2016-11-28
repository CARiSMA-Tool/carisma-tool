/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************
 */
package carisma.ui.eclipse.editors;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.ui.eclipse.editors.descriptions.EditorDescriptor;

/**
 * Abstract class, implements EditorDescriptor, is extended by several concrete EditorDescriptors, e.g. TopcasedEditorDescriptor.
 */
public abstract class AbstractEditorDescriptor implements EditorDescriptor {

	/**
	 * Checks if EditorDescriptor is in the Editor Registry.
	 * @return if EditorDescriptor is in the Editor Registry
	 */
	@Override
	public final boolean isAvailable() {
		IEditorRegistry editorRegistry = PlatformUI.getWorkbench()
				.getEditorRegistry();
		if (editorRegistry.findEditor(getId()) != null) {
			return true;
		}
		return false;
	}

	/**
	 * Opens the IFile.
	 * @param modelIFile - the model IFile
	 * @return true if file is opened successfully
	 */
	@Override
	public boolean forceOpen(final IFile modelIFile) {
		if (modelIFile != null) {
			IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			try {
				page.openEditor(
					new FileEditorInput(modelIFile),
					getId());
				return true;
			} catch (PartInitException e) {
				Logger.log(LogLevel.ERROR, e.getMessage(), e);
			}
		}
		return false;
	}
}
