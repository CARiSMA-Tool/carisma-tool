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
package carisma.ui.eclipse;

import java.io.ByteArrayOutputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.json.*;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import carisma.core.Carisma;
import carisma.core.analysis.Analysis;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.result.AnalysisResult;
import carisma.core.checks.CheckRegistry;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.models.ModelManager;
import carisma.core.models.ModelTypeRegistry;
import carisma.core.util.Utils;
import carisma.ui.eclipse.editors.EditorRegistry;
import carisma.ui.eclipse.logging.EclipseLogPrinter;
import carisma.ui.eclipse.preferences.Constants;
import carisma.ui.eclipse.views.AnalysisResultsView;

/**
 * The activator class controls the plug-in life cycle.
 */
public class CarismaGUI extends AbstractUIPlugin {

	/**
	 * The plug-in ID.
	 */
	public static final String PLUGIN_ID = "carisma.ui.eclipse"; //$NON-NLS-1$
	/**
	 * 
	 */
	public static final String IMG_SUCCESSFUL_ID = "image.successful";
	/**
	 * 
	 */
	public static final String IMG_WARNING_ID = "image.warning";
	/**
	 * 
	 */
	public static final String IMG_ERROR_ID = "image.error";
	/**
	 * 
	 */
	public static final String IMG_INFO_ID = "image.info";
	/**
	 * 
	 */
	public static final String IMG_SUCCESSWARNING_ID = "image.succwarning";
	/**
	 * 
	 */
	public static final String IMG_SUCCESSERROR_ID = "image.succerror";
	/**
	 * 
	 */
	public static final String IMG_RUNNING_ID = "image.running";
	/**
	 * 
	 */
	public static final String IMG_UP = "image.up";
	/**
	 * 
	 */
	public static final String IMG_DOWN = "image_down";

	/**
	 * 
	 */
	// The shared instance
	public static CarismaGUI INSTANCE;
	/**
	 * 
	 */
	private EditorRegistry editorRegistry;

	/**
	 * 
	 */

	/**
	 * The Carisma constructor.
	 */
	public CarismaGUI() {
		INSTANCE = this;
		this.editorRegistry = new EditorRegistry();
		Logger.setExternalLogPrinter(new EclipseLogPrinter());
		TrayDialog.setDialogHelpAvailable(true);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.
	 * BundleContext )
	 */
	/**
	 * @param context
	 * @throws Exception
	 *             an Exception
	 */
	public final void start(final BundleContext context) throws Exception {
		super.start(context);
		try {
			showViews();
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "Could not show Analysis result view");
		}
		this.editorRegistry.initialize();
	}

	/**
	 * 
	 */
	private void showViews() {
		showAnalysisResultsView();
	}

	/**
	 * 
	 */
	public final void showAnalysisResultsView() {
		INSTANCE.getWorkbench().getDisplay().asyncExec(new Runnable() {

			@Override
			public void run() {
				try {
					IWorkbenchPage page = INSTANCE.getWorkbench().getActiveWorkbenchWindow().getActivePage();
					AnalysisResultsView analysisResultsView = (AnalysisResultsView) page
							.showView(AnalysisResultsView.ID);
					analysisResultsView.update();
				} catch (PartInitException e) {
					Logger.log(LogLevel.ERROR, "Could not initialize, \"" + AnalysisResultsView.ID + "\" correctly", e);
				}
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.
	 * BundleContext )
	 */
	/**
	 * @param context
	 *            BundleContext
	 * @throws Exception
	 */
	public final void stop(final BundleContext context) throws Exception {
		INSTANCE = null;
		super.stop(context);
	}

	@Override
	protected final void initializeImageRegistry(final ImageRegistry registry) {
		Bundle bundle = Platform.getBundle(PLUGIN_ID);
		IPath path = new Path("icons/successful.png");
		URL url = FileLocator.find(bundle, path, null);
		ImageDescriptor desc = ImageDescriptor.createFromURL(url);
		registry.put(IMG_SUCCESSFUL_ID, desc);

		path = new Path("icons/error.png");
		url = FileLocator.find(bundle, path, null);
		desc = ImageDescriptor.createFromURL(url);
		registry.put(IMG_ERROR_ID, desc);

		path = new Path("icons/info.png");
		url = FileLocator.find(bundle, path, null);
		desc = ImageDescriptor.createFromURL(url);
		registry.put(IMG_INFO_ID, desc);

		path = new Path("icons/warning.png");
		url = FileLocator.find(bundle, path, null);
		desc = ImageDescriptor.createFromURL(url);
		registry.put(IMG_WARNING_ID, desc);

		path = new Path("icons/successfulwarning.png");
		url = FileLocator.find(bundle, path, null);
		desc = ImageDescriptor.createFromURL(url);
		registry.put(IMG_SUCCESSWARNING_ID, desc);

		path = new Path("icons/successfulerror.png");
		url = FileLocator.find(bundle, path, null);
		desc = ImageDescriptor.createFromURL(url);
		registry.put(IMG_SUCCESSERROR_ID, desc);

		path = new Path("icons/running.png");
		url = FileLocator.find(bundle, path, null);
		desc = ImageDescriptor.createFromURL(url);
		registry.put(IMG_RUNNING_ID, desc);

		path = new Path("icons/up.png");
		url = FileLocator.find(bundle, path, null);
		desc = ImageDescriptor.createFromURL(url);
		registry.put(IMG_UP, desc);

		path = new Path("icons/down.png");
		url = FileLocator.find(bundle, path, null);
		desc = ImageDescriptor.createFromURL(url);
		registry.put(IMG_DOWN, desc);
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in
	 * relative path.
	 * 
	 * @param path
	 *            the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(final String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}

	/**
	 * 
	 * @return List<AnalysisResult>
	 */
	public final List<AnalysisResult> getAnalysisResults() {
		return Carisma.getInstance().getAnalysisResults();
	}

	/**
	 * 
	 * @return this CheckRegistry
	 */
	public final CheckRegistry getCheckRegistry() {
		return Carisma.getInstance().getCheckRegistry();
	}

	/**
	 * 
	 * @return this model type registry
	 */
	public final ModelTypeRegistry getModelTypeRegistry() {
		return Carisma.getInstance().getModelTypeRegistry();
	}

	/**
	 * 
	 * @return this editor registry
	 */
	public final EditorRegistry getEditorRegistry() {
		return this.editorRegistry;
	}

	/**
	 * 
	 * @return modelManager
	 */
	public final ModelManager getModelManager() {
		return Carisma.getInstance().getModelManager();
	}

	/**
	 * 
	 * @param analysis
	 *            the analysis to be run
	 */
	public final void runAnalysis(final Analysis analysis) {
		Carisma.getInstance().runAnalysis(analysis, new EclipseUIConnector());
	}

	/**
	 * 
	 */
	public final void reset() {
		Carisma.getInstance().reset();
		showAnalysisResultsView();
	}

	/**
	 * 
	 * @param analysisResult
	 *            the analysis result
	 * 
	 */
	public final void openReport(final AnalysisResult analysisResult) {
		IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IContainer container = (IContainer) analysisResult.getAnalysis().getIFile().getParent();
		IFile file = null;
		if (container instanceof IFolder) {
			IFolder folder = (IFolder) container;
			file = folder.getFile("report-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".txt");

		} else if (container instanceof IProject) {
			IProject project = (IProject) container;
			file = project.getFile("report-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".txt");
		}

		else {
			Logger.log(LogLevel.ERROR, "Analyzed file is not part of a project.");
			return;
		}
		try {
			if (!(file.exists())) {
				file.create(Utils.createInputStreamFromString(analysisResult.getReport()), true, null);
			}

			IEditorDescriptor desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(file.getName());
			try {
				page.openEditor(new FileEditorInput(file), desc.getId());
			} catch (PartInitException e) {
				Logger.log(LogLevel.ERROR, "Could not start editor, \"" + desc.getId() + "\".", e);
			}
		} catch (CoreException e) {
			Logger.log(LogLevel.ERROR, "", e);
		}
	}

	/*
	 * This class handles the xml output. The Marshaller gets the class
	 * "AnalysisResult" as context.
	 * 
	 * @param analysisResult the analysis result
	 * 
	 * 
	 */

	public final void saveXml(final AnalysisResult analysisResult) {
		IContainer container = (IContainer) analysisResult.getAnalysis().getIFile().getParent();
		IFile file = null;
		if (container instanceof IFolder) {
			IFolder folder = (IFolder) container;
			file = folder
					.getFile("xml-output-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".xml");

		} else if (container instanceof IProject) {
			IProject project = (IProject) container;
			file = project
					.getFile("xml-output-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".xml");
		}

		else {
			Logger.log(LogLevel.ERROR, "Analyzed file is not part of a project.");
			return;
		}
		if (!(file.exists())) {

			try {

				ByteArrayOutputStream out = new ByteArrayOutputStream();

				JAXBContext context = JAXBContext.newInstance(carisma.core.analysis.result.AnalysisResult.class);
				Marshaller m = context.createMarshaller();
				m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

				m.marshal(analysisResult, out);

				String store = new String(out.toByteArray(), StandardCharsets.UTF_8);

				file.create(Utils.createInputStreamFromString(store), true, null);

				// JSONObject fromXml = XML.toJSONObject(store);
				// String jsonPrint = fromXml.toString(1);
				// System.out.println(jsonPrint);

				// carisma.core.analysis.result.exp.dbexport.exportXml(jsonPrint);

			} catch (Exception e) {
				System.out.println(e.getMessage());
			}

		}
	}

	public final void exportToDb(final AnalysisResult analysisResult) {
		
		InputDialog documentId = new InputDialog(null, "Please enter a id for this document.", null, null, null);
		documentId.open();
		String id = documentId.getValue().toString();
		IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IContainer container = (IContainer) analysisResult.getAnalysis().getIFile().getParent();
		IFile file = null;
		if (container instanceof IFolder) {
			IFolder folder = (IFolder) container;
			file = folder.getFile(
					"DB-output-status-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".text");

		} else if (container instanceof IProject) {
			IProject project = (IProject) container;
			file = project.getFile(
					"DB-output-status-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".text");
		}

		else {
			Logger.log(LogLevel.ERROR, "Analyzed file is not part of a project.");
			return;
		}
		if (!(file.exists())) {

			try {

				ByteArrayOutputStream out = new ByteArrayOutputStream();

				JAXBContext context = JAXBContext.newInstance(carisma.core.analysis.result.AnalysisResult.class);
				Marshaller m = context.createMarshaller();
				m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

				m.marshal(analysisResult, out);

				String store = new String(out.toByteArray(), StandardCharsets.UTF_8);

				JSONObject fromXml = XML.toJSONObject(store);
				String jsonPrint = fromXml.toString(1);
				System.out.println(jsonPrint);

				carisma.core.analysis.result.exp.dbexport.exportXml(jsonPrint, id);
				file.create(Utils.createInputStreamFromString(analysisResult.getReport()), true, null);

				IEditorDescriptor desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(file.getName());

				try {
					page.openEditor(new FileEditorInput(file), desc.getId());
				} catch (PartInitException e) {
					Logger.log(LogLevel.ERROR, "Could not start editor, \"" + desc.getId() + "\".", e);
				}

			} catch (Exception e) {
				System.out.println(e.getMessage());
			}

		}
	}

	@Override
	protected final void initializeDefaultPreferences(final IPreferenceStore store) {
		store.setDefault(Constants.EDITOR_ID, Constants.TEXT_EDITOR_ID);
		store.setDefault(Constants.PERSPECTIVE_ID, Perspective.ID);
		store.setDefault(Constants.PREF_ANALYSE, false);
		store.setDefault(Constants.EDITOR_SELECTION_ART, Constants.MANUALLY);
	}

	@Override
	protected final void initializeDefaultPluginPreferences() {
		IPreferenceStore store = this.getPreferenceStore();
		store.setDefault(Constants.EDITOR_ID, Constants.TEXT_EDITOR_ID);
		store.setDefault(Constants.PERSPECTIVE_ID, Perspective.ID);
		store.setDefault(Constants.PREF_ANALYSE, false);
		store.setDefault(Constants.EDITOR_SELECTION_ART, Constants.MANUALLY);
	}
}
