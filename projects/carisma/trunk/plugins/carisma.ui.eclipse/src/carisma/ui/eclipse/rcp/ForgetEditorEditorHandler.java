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
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;


/**
 * Handler to remove the saved editor name in an analysis file. The selected
 * resource is in an AdfEditor
 */
public class ForgetEditorEditorHandler extends ForgetEditorHandler {

	/**
	 * this Handler.
	 */
	private ForgetEditorEditorHandler handler;

	@Override
	final ForgetEditorHandler getHandler() {
		return handler;
	}

	/**
	 * 
	 * @return the selected file
	 */
	final IFile getSelectedFile() {
		return HandlerUtilz.getSelectedEditorFile();
	}

	/**
	 * Adds listeners for this handler to listen if an selection in AdfEditor has changed.
	 */
	protected final void installListener() {
		if (getAlreadyInstalledSelectionListener()) {
			return;
		}
		IPartService partService = PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getPartService();
		handler = this;

		partService.addPartListener(new IPartListener() {

			@Override
			public void partActivated(final IWorkbenchPart part) {
				fireHandlerChanged(new HandlerEvent(handler, true, false));
			}

			@Override
			public void partOpened(final IWorkbenchPart part) {

				fireHandlerChanged(new HandlerEvent(handler, true, false));
			}

			@Override
			public void partDeactivated(final IWorkbenchPart part) {
				fireHandlerChanged(new HandlerEvent(handler, true, false));
			}

			@Override
			public void partClosed(final IWorkbenchPart part) {
				fireHandlerChanged(new HandlerEvent(handler, true, false));
			}

			@Override
			public void partBroughtToTop(final IWorkbenchPart part) {
				fireHandlerChanged(new HandlerEvent(handler, true, false));
			}
		});

		setAlreadyInstalledSelectionListener(true);
	}

}
