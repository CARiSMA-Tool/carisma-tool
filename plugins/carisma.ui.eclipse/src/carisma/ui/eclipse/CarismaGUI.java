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



import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

import org.apache.commons.text.StringEscapeUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import carisma.core.Carisma;
import carisma.core.analysis.Analysis;
import carisma.core.analysis.result.AnalysisResult;
import carisma.core.checks.CheckRegistry;
import carisma.core.io.util.StringInputStream;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.models.ModelManager;
import carisma.core.models.ModelTypeRegistry;
import carisma.ui.eclipse.editors.EditorRegistry;
import carisma.ui.eclipse.logging.EclipseLogPrinter;
import carisma.ui.eclipse.preferences.Constants;
import carisma.ui.eclipse.views.AnalysisResultsView;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Marshaller;

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
	private final EditorRegistry editorRegistry;

	/**
	 * Extension points
	 */
	public static final String ANALYSIS_POPUP_MENU = "carisma.ui.eclipse.analysis.popup.menu";
	public static final String EDITOR_DESCRIPTION = "carisma.ui.eclipse.editor.description";

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
	@Override
	public final void start(final BundleContext context) throws Exception {
		super.start(context);
		try {
			showViews();
		} catch (final Exception e) {
			Logger.log(LogLevel.ERROR, "Could not show Analysis result view");
		}
		this.editorRegistry.initialize();
	}

	/**
	 *
	 */
	private static void showViews() {
		showAnalysisResultsView();
	}

	/**
	 *
	 */
	public static final void showAnalysisResultsView() {
		INSTANCE.getWorkbench().getDisplay().asyncExec(() -> {
			try {
				final var page = INSTANCE.getWorkbench().getActiveWorkbenchWindow().getActivePage();
				final var analysisResultsView = (AnalysisResultsView) page
						.showView(AnalysisResultsView.ID);
				analysisResultsView.update();
			} catch (final PartInitException e) {
				Logger.log(LogLevel.ERROR, "Could not initialize, \"" + AnalysisResultsView.ID + "\" correctly", e);
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
	@Override
	public final void stop(final BundleContext context) throws Exception {
		INSTANCE = null;
		super.stop(context);
	}

	@Override
	protected final void initializeImageRegistry(final ImageRegistry registry) {
		final var bundle = Platform.getBundle(PLUGIN_ID);
		IPath path = new Path("icons/successful.png");
		var url = FileLocator.find(bundle, path, null);
		var desc = ImageDescriptor.createFromURL(url);
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
	public static final List<AnalysisResult> getAnalysisResults() {
		return Carisma.getInstance().getAnalysisResults();
	}

	/**
	 *
	 * @return this CheckRegistry
	 */
	public static final CheckRegistry getCheckRegistry() {
		return Carisma.getInstance().getCheckRegistry();
	}

	/**
	 *
	 * @return this model type registry
	 */
	public static final ModelTypeRegistry getModelTypeRegistry() {
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
	public static final ModelManager getModelManager() {
		return Carisma.getInstance().getModelManager();
	}

	/**
	 *
	 * @param analysis
	 *            the analysis to be run
	 */
	public static final void runAnalysis(final Analysis analysis) {
		Carisma.getInstance().runAnalysis(analysis, new EclipseUIConnector());
	}

	/**
	 *
	 */
	public static final void reset() {
		Carisma.getInstance().reset();
		showAnalysisResultsView();
	}

	/**
	 *
	 * @param analysisResult
	 *            the analysis result
	 *
	 */

	/*
	 *
	 * Currently changed to output HTML-Files
	 */
	public static final void openReport(final AnalysisResult analysisResult) {
		final var page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		final var container = analysisResult.getAnalysis().getIFile().getParent();
		IFile file = null;
		if (container instanceof IFolder) {
			final var folder = (IFolder) container;
			file = folder.getFile("report-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".html"); // changedfrom
			// txt

		} else if (container instanceof IProject) {
			final var project = (IProject) container;
			file = project
					.getFile("report-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".html"); // changedfrom
			// txt
		} else {
			Logger.log(LogLevel.ERROR, "Analyzed file is not part of a project.");
			return;
		}

		// new...
		final var htmlOpen = "<!DOCTYPE html>\n"
				+ "<html lang=\"de\">\n"
				+ "<head>\n"
				+ "\t<meta charset=\"utf-8\">\n"
				+ "\t<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
				+ "\t<title>CARiSMA Report</title>\n"
				+ "</head>\n"
				+ "<body>\n"
				+ "\t<p>\n\t";
		final var htmlClose="</p>\n"
				+ "</body>\n"
				+ "</html>";
		var htmlBody = StringEscapeUtils.escapeHtml4(analysisResult.getReport());
		htmlBody = htmlBody.replace("\t", "&emsp;").replaceAll("[\\r\\n]", "<br/>"+System.lineSeparator()+"\t");
		final var html = (htmlOpen + htmlBody + htmlClose);

		try {
			if (!(file.exists())) {
				file.create(StringInputStream.createInputStreamFromString(html), true, null);
			}

			final var desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(file.getName());
			try {
				page.openEditor(new FileEditorInput(file), desc.getId());
			} catch (final PartInitException e) {
				Logger.log(LogLevel.ERROR, "Could not start editor, \"" + desc.getId() + "\".", e);
			}
		} catch (final CoreException e) {
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

	public static final void saveXml(final AnalysisResult analysisResult) {
		final var container = analysisResult.getAnalysis().getIFile().getParent();
		IFile file = null;
		if (container instanceof IFolder) {
			final var folder = (IFolder) container;
			file = folder
					.getFile("xml-output-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".xml");

		} else if (container instanceof IProject) {
			final var project = (IProject) container;
			file = project
					.getFile("xml-output-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".xml");
		} else {
			Logger.log(LogLevel.ERROR, "Analyzed file is not part of a project.");
			return;
		}
		if (!(file.exists())) {

			try (var out = new ByteArrayOutputStream()){

				final var context = JAXBContext.newInstance(carisma.core.analysis.result.AnalysisResult.class);
				final var m = context.createMarshaller();
				m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
				m.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
				m.marshal(analysisResult, out);

				final var store = new String(out.toByteArray(), StandardCharsets.UTF_8);

				try(final InputStream is = new ByteArrayInputStream(store.getBytes(StandardCharsets.UTF_8))){
					file.create(is, true, null);
				}

			} catch (final Exception e) {
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
		final var store = getPreferenceStore();
		store.setDefault(Constants.EDITOR_ID, Constants.TEXT_EDITOR_ID);
		store.setDefault(Constants.PERSPECTIVE_ID, Perspective.ID);
		store.setDefault(Constants.PREF_ANALYSE, false);
		store.setDefault(Constants.EDITOR_SELECTION_ART, Constants.MANUALLY);
	}

	static String getString(final InputStream is) {

		final var result = new ByteArrayOutputStream();
		final var buffer = new byte[1024];
		int length;
		try {
			while ((length = is.read(buffer)) != -1) {
				result.write(buffer, 0, length);
			}
			return result.toString(StandardCharsets.UTF_8);
		} catch (final IOException e) {
			Logger.log(LogLevel.ERROR, "", e);
			return null;
		}

	}
}
