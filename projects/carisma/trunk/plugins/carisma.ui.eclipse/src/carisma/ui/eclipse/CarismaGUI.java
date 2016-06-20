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

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringBufferInputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.TrayDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.json.JSONObject;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import carisma.core.Carisma;
import carisma.core.analysis.Analysis;
import carisma.core.analysis.Analyzer;
import carisma.core.analysis.AutomatedAnalysis;
import carisma.core.analysis.result.AnalysisResult;
import carisma.core.checks.CheckRegistry;
import carisma.core.io.content.JSON;
import carisma.core.io.content.PLAIN;
import carisma.core.io.content.XML_DOM;
import carisma.core.io.implementations.db.mongodb.restapi.MongoDBDynamicConfiguration;
import carisma.core.io.implementations.db.mongodb.restapi.MongoDBRestAPI;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.models.ModelManager;
import carisma.core.models.ModelTypeRegistry;
import carisma.core.util.Utils;
import carisma.ui.eclipse.editors.EditorRegistry;
import carisma.ui.eclipse.logging.EclipseLogPrinter;
import carisma.ui.eclipse.preferences.Constants;
import carisma.ui.eclipse.views.AnalysisResultsView;
import carisma.core.analysis.AnalysisUtil;

import static carisma.ui.eclipse.preferences.pages.VisiOn.*;

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
				m.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
				m.marshal(analysisResult, out);

				String store = new String(out.toByteArray(), StandardCharsets.UTF_8);

				InputStream is = new ByteArrayInputStream(store.getBytes(StandardCharsets.UTF_8));
				// file.create(Utils.createInputStreamFromString(store), true,
				// null);
				file.create(is, true, null);

//				JSONObject fromXml = XML.toJSONObject(store);
//				String jsonPrint = fromXml.toString(1);
//				System.out.println(jsonPrint);

				// carisma.core.analysis.result.exp.dbexport.exportXml(jsonPrint);

				out.close();
			} catch (Exception e) {
				System.out.println(e.getMessage());
			}

		}
	}

	public final void exportToDb(final AnalysisResult analysisResult) {

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
		if (!(file.exists())) { //TODO: is this necessary? What is if the file has been deleted in the DB but not local?

			try {

				ByteArrayOutputStream out = new ByteArrayOutputStream();

				JAXBContext context = JAXBContext.newInstance(carisma.core.analysis.result.AnalysisResult.class);
				Marshaller m = context.createMarshaller();
				m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

				m.marshal(analysisResult, out);

				URL urlXmlToXml = new URL("platform:/plugin/"+PLUGIN_ID+"/xslt/carisma_results_to_xml.xsl");
				URL urlXmlToHtml = new URL("platform:/plugin/"+PLUGIN_ID+"/xslt/carisma_results_to_html.xsl");
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(urlXmlToXml.openStream()));
				StringBuilder contentCmltoXml = new StringBuilder();
				String line;
				while((line = reader.readLine())!= null){
					contentCmltoXml.append(line.replaceAll("carisma_results_to_html.xsl", FileLocator.toFileURL(urlXmlToHtml).getPath()));
				}
				
				TransformerFactory transformerFactory = TransformerFactory.newInstance();
				Transformer xmlTransformer = transformerFactory.newTransformer(new StreamSource(new StringReader(contentCmltoXml.toString())));
				Transformer htmlTransformer = transformerFactory.newTransformer(new StreamSource(urlXmlToHtml.openStream()));
				
				StringWriter writerXml = new StringWriter();
				StringWriter writerHtml = new StringWriter();
			    StreamResult streamResultXml = new StreamResult(writerXml);
			    StreamResult streamResultHtml = new StreamResult(writerHtml);
				StreamSource streamSourceXml = new StreamSource(new ByteArrayInputStream(out.toByteArray()));
				StreamSource streamSourceHtml = new StreamSource(new ByteArrayInputStream(out.toByteArray()));
				xmlTransformer.transform(streamSourceXml, streamResultXml);
				htmlTransformer.transform(streamSourceHtml, streamResultHtml);
				
				IPreferenceStore preferencesStore = CarismaGUI.INSTANCE.getPreferenceStore();
				
				String user = preferencesStore.getString(KEY_USER);
				String secret = preferencesStore.getString(KEY_SECRET);
				String url = preferencesStore.getString(KEY_URL);
				
				MongoDBRestAPI db = new MongoDBRestAPI(user,secret,url);
				
				XML_DOM contentXml = new XML_DOM(writerXml.toString());
				PLAIN contentHtml = new PLAIN(writerHtml.toString());
				
				
				String carisma_collection = preferencesStore.getString(KEY_CARISMA_COLLECTION);
				String carisma_document = preferencesStore.getString(KEY_CARISMA_DOCUMENT);
				String carisma_field = preferencesStore.getString(KEY_CARISMA_FIELD);
				MongoDBDynamicConfiguration carisma_configuration = new MongoDBDynamicConfiguration(url, carisma_collection, carisma_document, carisma_field);
				boolean success = db.write(carisma_configuration, contentXml);
				
				db = new MongoDBRestAPI(user,secret,url);
				
				String pla_collection = preferencesStore.getString(KEY_PLA_COLLECTION);
				String pla_document = preferencesStore.getString(KEY_PLA_DOCUMENT);
				String pla_field = preferencesStore.getString(KEY_PLA_FIELD);
				MongoDBDynamicConfiguration pla_configuration = new MongoDBDynamicConfiguration(url, pla_collection, pla_document, pla_field);
				success &= db.write(pla_configuration, contentHtml);
				
				if(!success){
					Shell activeShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
					ErrorDialog.openError(activeShell, "Vision Database Export", "Export to VisiOn Database failed", new Status(IStatus.ERROR, "carisma.core.io",db.getResponseMessage().toString())); 
				}
				
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

	/* Automated analysis menu entry */

	public final void startAutomatedAnalysis(final AnalysisResult analysisResult) {

		IContainer container = (IContainer) analysisResult.getAnalysis().getIFile().getParent();
		String report = "";

		try {

			ByteArrayOutputStream out = new ByteArrayOutputStream();

			JAXBContext context = JAXBContext.newInstance(carisma.core.analysis.result.AnalysisResult.class);
			Marshaller m = context.createMarshaller();
			m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
			m.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
			m.marshal(analysisResult, out);

			report = new String(out.toByteArray(), StandardCharsets.UTF_8);

			System.out.println("STRING Report:");
			System.out.println(report);
			
			out.close();
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		AutomatedAnalysis ana = new AutomatedAnalysis(report, container);
		Analyzer a = new Analyzer();

		a.runAnalysis(ana.getAnalysis(), new EclipseUIConnector());

		/*
		 * the option to try it on the disk (ignoring the Resources of eclipse)
		 */
		// get model path in workspace
		String path = ResourcesPlugin.getWorkspace().getRoot().getLocation().toString();
		// get model name
		String name = ana.getAnalysis().getName();
		// get path of new analysis on disk
		String anaPath = ana.getPathstring();
		// merge the paths

		// String analysisPath = path + anaPath + name;
		String analysisPath = name ;
		System.out.println(name);
		// System.out.println(container.toString() + "/" + name);
		// System.out.println(analysisPath + ".adf");
		// store the created analysis in the right project on the workspace.


		
		/*
		 * Version with the Resource plugins, doesnt works
		 * 
		 */
		 try {
			System.out.println(ana.getAnalysis().getIFile().getContents());
		} catch (CoreException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}

		
		 InputStream stream = new ByteArrayInputStream(analysisPath.getBytes(StandardCharsets.UTF_8));

	
		try {
			IFile file = null;
			if (container instanceof IFolder) {
				IFolder folder = (IFolder) container;
				file = folder.getFile(analysisPath + ".adf");
				if (!file.exists()) {
					try {
						file.create(stream, IResource.NONE, null);
					} catch (CoreException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}

			} else if (container instanceof IProject) {
				IProject project = (IProject) container;
				file = project.getFile(analysisPath + ".adf");

				if (!file.exists()) {
					try {
						file.create(stream, IResource.NONE, null);
					} catch (CoreException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				}
				AnalysisUtil.storeAnalysis(ana.getAnalysis(), path  + anaPath + name + ".adf");
				System.out.println("THE PATH IS: " + path +  anaPath + name);
				 
				file.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor() );
			}
		} catch (CoreException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
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

	String getString(InputStream is) {

		ByteArrayOutputStream result = new ByteArrayOutputStream();
		byte[] buffer = new byte[1024];
		int length;
		try {
			while ((length = is.read(buffer)) != -1) {
				result.write(buffer, 0, length);
			}
			return result.toString("UTF-8");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return null;
	}
}
