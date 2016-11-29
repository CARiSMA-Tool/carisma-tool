package carisma.ui.vision.popup.actions;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
import java.util.Map;

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
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;

import carisma.core.analysis.result.AnalysisResult;
import carisma.core.analysis.result.CheckResult;
import carisma.core.io.content.ContentFactory;
import carisma.core.io.content.PLAIN;
import carisma.core.io.content.XML_DOM;
import carisma.core.io.util.StringInputStream;
import carisma.core.io.content.ContentFactory.ContentFormats;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.popup.actions.PopUpAction;
import carisma.ui.vision.VisionActivator;
import carisma.ui.vision.eclipse.preferences.PreferencesConstants;
import carisma.ui.vision.eclipse.preferences.PreferencesObject;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI.MongoDBDestination;

public class VisiOnDBOutput implements PopUpAction {

	@Override
	public boolean perform(final IMenuManager manager, final AnalysisResult analysisResult) {
		/*
		 * initializing output to VisiOn DB.
		 * 
		 */
		Action action3 = new Action() {
			@Override
			public void run() {
				super.run();
				IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
				IContainer container = analysisResult.getAnalysis().getIFile().getParent();
				IFile file = null;
				if (container instanceof IFolder) {
					IFolder folder = (IFolder) container;
					file = folder.getFile(
							"DB-output-status-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".text");

				} else if (container instanceof IProject) {
					IProject project = (IProject) container;
					file = project.getFile(
							"DB-output-status-" + analysisResult.getName() + "-" + analysisResult.getTimestamp() + ".text");
				} else {
					Logger.log(LogLevel.ERROR, "Analyzed file is not part of a project.");
					return;
				}
				if (!(file.exists())) {
					/*
					 * TODO: is this if clause necessary?
					 * 
					 * What is if the file has been deleted in the DB but not local?
					 */

					try {

						ByteArrayOutputStream out = new ByteArrayOutputStream();

						JAXBContext context = JAXBContext.newInstance(carisma.core.analysis.result.AnalysisResult.class);
						Marshaller m = context.createMarshaller();
						m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);

						m.marshal(analysisResult, out);

						String platform = "platform:/plugin/";
						String xmlXsl = "/xslt/carisma_results_to_xml.xsl";
						URL urlXmlToXml = new URL(platform + CarismaGUI.PLUGIN_ID + xmlXsl);
						String htmlXsl = "/xslt/carisma_results_to_html.xsl";
						URL urlXmlToHtml = new URL(platform + CarismaGUI.PLUGIN_ID + htmlXsl);

						InputStream inputStream = urlXmlToXml.openStream();
						InputStreamReader inputStreamReader = new InputStreamReader(inputStream);
						try(BufferedReader reader = new BufferedReader(inputStreamReader)){
							StringBuilder contentXmltoXml = new StringBuilder();
							String line;
							while ((line = reader.readLine()) != null) {
								String path = FileLocator.toFileURL(urlXmlToHtml).getPath();
								String key = "carisma_results_to_html.xsl";
								String replaced = line.replaceAll(key, path);
								contentXmltoXml.append(replaced);
							}
							TransformerFactory transformerFactory = TransformerFactory.newInstance();
	
							StringReader xmlStringReader = new StringReader(contentXmltoXml.toString());
							StreamSource xmlStreamSource = new StreamSource(xmlStringReader);
							Transformer xmlTransformer = transformerFactory.newTransformer(xmlStreamSource);
							StreamSource htmlStreamSource = new StreamSource(urlXmlToHtml.openStream());
							Transformer htmlTransformer = transformerFactory.newTransformer(htmlStreamSource);
	
							StringWriter writerXml = new StringWriter();
							StringWriter writerHtml = new StringWriter();
							StreamResult streamResultXml = new StreamResult(writerXml);
							StreamResult streamResultHtml = new StreamResult(writerHtml);
							StreamSource streamSourceXml = new StreamSource(new ByteArrayInputStream(out.toByteArray()));
							StreamSource streamSourceHtml = new StreamSource(new ByteArrayInputStream(out.toByteArray()));
							xmlTransformer.transform(streamSourceXml, streamResultXml);
							htmlTransformer.transform(streamSourceHtml, streamResultHtml);
	
							PreferencesObject preferencesStore = VisionActivator.INSTANCE.getVisionPreferences();
							Map<String, Object> map = preferencesStore.getObject();
							
							String user = (String) map.get(PreferencesConstants.dbuser);
							String secret = (String) map.get(PreferencesConstants.dbpasswd);
							String url = (String) map.get(PreferencesConstants.dbaddress);
	
							MongoDBRestAPI db = new MongoDBRestAPI(user, secret, url);
	
							XML_DOM contentXml = (XML_DOM) ContentFactory.createContent(writerXml.toString(),
									ContentFormats.F_XML_DOM);
							PLAIN contentHtml = (PLAIN) ContentFactory.createContent(writerHtml.toString(), ContentFormats.F_PLAIN);
	
							String carismaCollection = (String) map.get(PreferencesConstants.carisma_collection);
							String carismaDocument = (String) map.get(PreferencesConstants.carisma_document);
							String carismaField = (String) map.get(PreferencesConstants.carisma_field);
							
							MongoDBDestination carismaConfiguration = new MongoDBDestination(carismaCollection, carismaDocument, carismaField);
							boolean success = db.write(carismaConfiguration, contentXml);
							StringBuilder errorMessageBuilder = new StringBuilder();
							if (!success) {
								String response = db.getResponseMessage().toString();
	
								errorMessageBuilder.append("Export of the XMI failed for the following reason:");
								errorMessageBuilder.append(response);
								errorMessageBuilder.append("\n");
							}
							db = new carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI(user, secret, url);
	
							String plaCollection = (String) map.get(PreferencesConstants.pla_collection);
							String plaDocument = (String) map.get(PreferencesConstants.pla_document);
							String plaField = (String) map.get(PreferencesConstants.pla_field);
							
							MongoDBDestination plaConfiguration = new MongoDBDestination(plaCollection,
									plaDocument, plaField);
							success &= db.write(plaConfiguration, contentHtml);
	
							Shell activeShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
							String dialogTitle = "Vision Database Export";
							if (!success) {
								String response = db.getResponseMessage().toString();
	
								errorMessageBuilder.append("Export of the html report failed for the following reason:");
								errorMessageBuilder.append(response);
								String responseMessage = errorMessageBuilder.toString();
	
								Status status = new Status(IStatus.ERROR, "carisma.core.io", responseMessage);
								ErrorDialog.openError(activeShell, dialogTitle, "Export to VisiOn Database failed", status);
							} else {
								MessageDialog.openConfirm(activeShell, dialogTitle, "Success");
							}
	
							file.create(StringInputStream.createInputStreamFromString(analysisResult.getReport()), true, null);
	
							IEditorDescriptor desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(file.getName());
	
							try {
								page.openEditor(new FileEditorInput(file), desc.getId());
							} catch (PartInitException e) {
								Logger.log(LogLevel.ERROR, "Could not start editor, \"" + desc.getId() + "\".", e);
							}

						}
					}
					catch (Exception e) {
						System.out.println(e.getMessage());
					}
				}
			}
		};

		action3.setText("Export report to VisiOn Database");
		manager.add(action3);// Don't enable action for RABAC transformation input
		// creation
		int forbidden = 0;
		for (CheckResult chkR : analysisResult.getCheckResults()) {
			if (chkR.getName().compareTo("RABACsec: Create transformation input") == 0) { // TODO:
																							// Call
																							// Name
																							// Method
																							// for
																							// compare
				forbidden++;
			} else if (chkR.getName().compareTo("Create Help Document for STS mapping") == 0) { // TODO:
																								// Call
																								// Name
																								// Method
																								// for
																								// compare
				forbidden++;
			}
		}
		if (forbidden == analysisResult.getCheckResults().size()) {
			action3.setEnabled(false);
		}
		
		return true;
	}

}
