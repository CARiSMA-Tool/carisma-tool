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

package carisma.ui.eclipse.rcp;

import org.eclipse.core.commands.HandlerEvent;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;


/**
 * Handler to remove the saved editor name in an analysis file. The selected
 * resource is in an Navigator (Resource, Project, etc).
 */
public class ForgetEditorNavigatorHandler extends ForgetEditorHandler {

	/**
	 * this Handler.
	 */
	private ForgetEditorNavigatorHandler handler;

	@Override
	final ForgetEditorHandler getHandler() {
		return this.handler;
	}
	
	/**
	 * Synthetic method for access in anonymous class
	 */
	@Override
	protected void fireHandlerChanged(HandlerEvent handlerEvent) {
		super.fireHandlerChanged(handlerEvent);
	}

	/**
	 * Adds listeners for this handler to listen if an selection in the
	 * Navigator has changed.
	 */
	@Override
	protected final void installListener() {
		if (getAlreadyInstalledSelectionListener()) {
			return;
		}

		ISelectionService selectionService = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getSelectionService();
		this.handler = this;

		selectionService.addSelectionListener(new ISelectionListener() {
			@Override
			public void selectionChanged(final IWorkbenchPart part,
					final ISelection selection) {
				fireHandlerChanged(new HandlerEvent(ForgetEditorNavigatorHandler.this.handler, true, false));
			}
		});

		setAlreadyInstalledSelectionListener(true);
	}

	/**
	 * @return the selection file in the navigator.
	 */
	@Override
	final IFile getSelectedFile() {
		return HandlerUtilz.getSelectedNavigatorFile();
	}

}
