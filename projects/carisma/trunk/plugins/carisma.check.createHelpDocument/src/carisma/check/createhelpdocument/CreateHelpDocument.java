package carisma.check.createhelpdocument;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static carisma.ui.eclipse.preferences.pages.VisiOn.*;

import java.io.File;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Package;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Element;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.core.io.content.BASE64;
import carisma.core.io.content.Content;
import carisma.core.io.content.Content.ContentException;
import carisma.core.io.content.JSON;
import carisma.core.io.content.XML_DOM;
import carisma.core.io.implementations.FileIO;
import carisma.core.io.implementations.db.mongodb.restapi.MongoDBDynamicConfiguration;
import carisma.core.io.implementations.db.mongodb.restapi.MongoDBRestAPI;
import carisma.modeltype.uml2.UMLDeploymentHelper;
import carisma.modeltype.uml2.UMLHelper;
import carisma.ui.eclipse.CarismaGUI;

public class CreateHelpDocument implements CarismaCheck {

	AnalysisHost host;

	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {

		this.host = host;
		Resource model = host.getAnalyzedModel();

		if (model.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}

		if (model.getContents().get(0) instanceof Package) {
			Package content = (Package) model.getContents().get(0);

			try {
				BooleanParameter fileOrDB = (BooleanParameter) (parameters.get("carisma.check.createHelpDocument.STSFileOrDB"));
				
				Document doc;
				if(fileOrDB.getValue()){
					doc = loadSTSInputFromFile();
				}
				else{
					doc = loadSTSInputFromDB();
					if(doc == null){
						host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Unable to load STS input from DB"));
						return false;
					}
				}

				/*
				 * Dirty Hack to get the Names of the UML Classes List classes:
				 * contains Strings with the different classes
				 * 
				 * 
				 */

				List<org.eclipse.uml2.uml.Element> UMLelements = UMLHelper.getAllElements(content);
				List<String> classes = new ArrayList<String>(30);
				int classesIndex = 0;
				for (int m = 0; m < UMLelements.size(); m++) {
					org.eclipse.uml2.uml.Element n = UMLelements.get(m);

					if (n instanceof org.eclipse.uml2.uml.internal.impl.ClassImpl) {
						String toString = n.toString();
						if (toString.contains("name")) {
							String[] shortString = toString.split("\\(", 2);
							String[] shorterString = shortString[1].split("\\s+", 2);
							String[] shortestString = shorterString[1].split(",", 2);
							String finalString = shortestString[0].replaceAll(" ", "");
							classes.add(classesIndex, finalString);

							classesIndex++;
						}

					}

				}

				/* return the commitments and store in nList */

				NodeList elements = doc.getElementsByTagName("commitment");

				host.appendToReport("----------------------------");

				HashSet<String> roles = new HashSet<String>();
				HashSet<String> nodeNames = new HashSet<String>();
				HashSet<String> documents = new HashSet<String>();

				for (int temp = 0; temp < elements.getLength(); temp++) {

					Node nNode = elements.item(temp);

					if (nNode.getNodeType() == Node.ELEMENT_NODE) {
						Element eElement = (Element) nNode;
						String commitmentId = eElement.getAttribute("id");

						host.appendToReport("\n");
						host.appendToReport("\n");

						host.appendToReport("\n" + "|Commitment ID = " + commitmentId + " |");

						if (eElement.getElementsByTagName("precondition").item(0) != null) {
							Node preCon = eElement.getElementsByTagName("precondition").item(0);
							NodeList ePreCon = preCon.getChildNodes();

							for (int i = 0; i < ePreCon.getLength(); i++) {
								String sourceAttribute;
								String destinationAttribute;

								Node nodePre = ePreCon.item(i);
								if (nodePre.getNodeType() == Node.ELEMENT_NODE) {
									Element preconditionElement = (Element) nodePre;

									/***********************************************
									 * GET SOURCE ATTRIBUTES FOR ROLES
									 **************************************************************/
									if (preconditionElement.getElementsByTagName("source").item(0) != null) {
										Node source = preconditionElement.getElementsByTagName("source").item(0);
										Element sourceElement = (Element) source;

										if (sourceElement.getElementsByTagName("role").item(0) != null) {
											sourceAttribute = sourceElement.getElementsByTagName("role").item(0)
													.getTextContent();
											host.appendToReport("\n| role = " + sourceAttribute + " | ");
											roles.add(sourceAttribute);

										} else {
											sourceAttribute = sourceElement.getElementsByTagName("agent").item(0)
													.getTextContent();
											host.appendToReport("\n| role = " + sourceAttribute + " | ");
											roles.add(sourceAttribute);

										}

									}
									/********************************************************** END **********************************************************************/

									/***********************************************
									 * GET DESTINATION ATTRIBUTES FOR ROLES
									 **************************************************************/
									if (preconditionElement.getElementsByTagName("destination").item(0) != null) {
										Node destination = preconditionElement.getElementsByTagName("destination")
												.item(0);
										Element destinationElement = (Element) destination;

										if (destinationElement.getElementsByTagName("role").item(0) != null) {
											destinationAttribute = destinationElement.getElementsByTagName("role")
													.item(0).getTextContent();
											host.appendToReport("\n| role = " + destinationAttribute + " | ");
											roles.add(destinationAttribute);

										} else {
											destinationAttribute = destinationElement.getElementsByTagName("agent")
													.item(0).getTextContent();
											host.appendToReport("\n| role = " + destinationAttribute + " | ");
											roles.add(destinationAttribute);

										}

									}

									/********************************************************** END **********************************************************************/

								}

							}

							// getting the documents from STS file

							for (int i = 0; i < ePreCon.getLength(); i++) {

								String sourcedocument;

								Node nodePre = ePreCon.item(i);
								if (nodePre.getNodeType() == Node.ELEMENT_NODE) {
									Element preconditionElement = (Element) nodePre;

									/***********************************************
									 * GET SOURCE ATTRIBUTES FOR DOCUMENTS
									 **************************************************************/
									if (preconditionElement.getElementsByTagName("document").item(0) != null) {
										sourcedocument = preconditionElement.getElementsByTagName("document").item(0)
												.getTextContent();

										host.appendToReport("\n| document = " + sourcedocument + " | ");
										documents.add(sourcedocument.replace(" ", ""));

								}

							}
						}

						// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

						Node post = eElement.getElementsByTagName("postcondition").item(0);
						NodeList ePost = post.getChildNodes();
						for (int i = 0; i < ePost.getLength(); i++) {
							Node nodePost = ePost.item(i);

							String securityRequirement = nodePost.getNodeName();

							String attribute = null;

							try {
								Attr attr = (Attr) nodePost.getAttributes().item(0);
								if (attr != null) {
									attribute = attr.getNodeValue();

									if (nodePost.getNodeType() == Node.ELEMENT_NODE) {
										host.appendToReport(
												"\n| security requirement: " + nodePost.getNodeName() + " | ");
										host.appendToReport("\n| security requirement type: " + attribute + " | ");

										if (securityRequirement.contentEquals("integrity")
												&& attribute.contentEquals("system")) {
											host.appendToReport("\n| CARiSMA check = SecureLinks " + " |"
													+ "\n| UML Diagram = Deployment Diagram | \n");
											host.appendToReport(
													"| Find explanation: Click on 'Help' -> 'Help Contents' -> expand 'CARiSMA' -> expand 'Checks' -> expand 'Static Checks' -> click on 'Secure Links'"
															+ " |");

											Set<Dependency> dependencies = UMLDeploymentHelper
													.getAllDependencies(content);

											for (Dependency d : dependencies) {

												Set<CommunicationPath> comPath = UMLDeploymentHelper
														.getCommunicationPaths(d);

												for (CommunicationPath p : comPath) {
													List<org.eclipse.uml2.uml.Node> nodes = UMLDeploymentHelper
															.getNodes(p);

													for (int g = 0; g < nodes.size(); g++) {
														nodeNames.add(nodes.get(g).getLabel());

													}
												}
											}

											host.appendLineToReport(
													"| nodes in deployment diagram: " + nodeNames.toString() + "|");
											host.appendLineToReport("| roles in STS model: " + roles.toString() + "|");

											// map
											for (String r : roles) {
												for (String n : nodeNames) {
													if (r.equals(n)) {
														host.appendToReport(
																"| Role: " + r + " is mapped to Node: " + n + ". |\n");
													}
												}
											}

										}
									}

									if (securityRequirement.contentEquals("integrity")
											&& (attribute.contentEquals("sender")
													|| attribute.contentEquals("receiver"))) {
										host.appendToReport("\n| CARiSMA check = SecureDependency " + " |"
												+ "\n| UML Diagram = Class Diagram | \n");
										host.appendToReport(
												"| Find explanation: Click on 'Help' -> 'Help Contents' -> expand 'CARiSMA' -> expand 'Checks' -> expand 'Static Checks' -> click on 'Secure Dependency'"
														+ " |");

										// mapping of documents and classes
										for (String d : documents) {
											for (String c : classes) {
												if (d.equalsIgnoreCase(c)) {
													host.appendToReport(
															"| Document: " + d + " is mapped to Class: " + c + ". |\n");
												}
											}
										}

										// mapping of roles and classes
										for (String r : roles) {
											for (String c : classes) {
												if (r.equalsIgnoreCase(c)) {
													host.appendToReport(
															"| Role: " + r + " is mapped to Class: " + c + ". |\n");
												}
											}
										}

									}

									if (securityRequirement.contentEquals("needToKnow")) {

										host.appendToReport("\n| CARiSMA check = RABAC " + " |"
												+ "\n| UML Diagram = Class Diagram & State Machine| \n");
										host.appendToReport(
												"| Find explanation: Click on 'Help' -> 'Help Contents' -> expand 'CARiSMA' -> expand 'Checks' -> click on 'RABAC'"
														+ " |");

										// mapping of documents and classes
										for (String d : documents) {
											for (String c : classes) {
												if (d.equalsIgnoreCase(c)) {
													host.appendToReport(
															"| Document: " + d + " is mapped to Class: " + c + ". |\n");
												}
											}
										}

										// mapping of roles and classes
										for (String r : roles) {
											for (String c : classes) {
												if (r.equalsIgnoreCase(c)) {
													host.appendToReport(
															"| Role: " + r + " is mapped to Class: " + c + ". |\n");
												}
											}
										}

									}

									if (securityRequirement.contentEquals("authenticationDelegation")
											&& (attribute.contentEquals("delegatee")
													|| attribute.contentEquals("delegator"))) {
										host.appendToReport("\n| CARiSMA check = RABAC " + " |"
												+ "\n| UML Diagram = Class Diagram & State Machine | \n");
										host.appendToReport(
												"| Find explanation: Click on 'Help' -> 'Help Contents' -> expand 'CARiSMA' -> expand 'Checks' -> click on 'RABAC'"
														+ " |");

										// mapping of documents and classes
										for (String d : documents) {
											for (String c : classes) {
												if (d.equalsIgnoreCase(c)) {
													host.appendToReport(
															"| Document: " + d + " is mapped to Class: " + c + ". |\n");
												}
											}
										}

										// mapping of roles and classes
										for (String r : roles) {
											for (String c : classes) {
												if (r.equalsIgnoreCase(c)) {
													host.appendToReport(
															"| Role: " + r + " is mapped to Class: " + c + ". |\n");
												}
											}
										}

									}

										if (securityRequirement.contentEquals("confidentiality")
												&& attribute.contentEquals("system")) {
											
											host.appendToReport("\n| CARiSMA check = SecureLinks " + " |"
													+ "\n| UML Diagram = Deployment Diagram | \n");
											host.appendToReport(
													"| Find explanation: Click on 'Help' -> 'Help Contents' -> expand 'CARiSMA' -> expand 'Checks' -> expand 'Static Checks' -> click on 'Secure Links'"
															+ " |");
	
											Set<Dependency> dependencies = UMLDeploymentHelper.getAllDependencies(content);
	
											for (Dependency d : dependencies) {
	
												Set<CommunicationPath> comPath = UMLDeploymentHelper
														.getCommunicationPaths(d);
	
												for (CommunicationPath p : comPath) {
													List<org.eclipse.uml2.uml.Node> nodes = UMLDeploymentHelper.getNodes(p);
	
													for (int g = 0; g < nodes.size(); g++) {
														nodeNames.add(nodes.get(g).getLabel());
	
													}
												}
											}
	
											host.appendLineToReport(
													"| nodes in deployment diagram: " + nodeNames.toString() + "|");
											host.appendLineToReport("| roles in STS model: " + roles.toString() + "|");
	
											// mapping of documents and classes
											for (String d : documents) {
												for (String c : classes) {
													if (d.equalsIgnoreCase(c)) {
														host.appendToReport(
																"| Document: " + d + " is mapped to Class: " + c + ". |\n");
													}
												}
											}
	
											// mapping of roles and classes
											for (String r : roles) {
												for (String c : classes) {
													if (r.equalsIgnoreCase(c)) {
														host.appendToReport(
																"| Role: " + r + " is mapped to Class: " + c + ". |\n");
													}
												}
											}
										}

										if (securityRequirement.contentEquals("confidentiality")
												&& (attribute.contentEquals("sender")
														|| attribute.contentEquals("receiver"))) {
											
											host.appendToReport("\n| CARiSMA check = SecureDependency " + " |"
													+ "\n| UML Diagram = Class Diagram | \n");
											host.appendToReport(
													"| Find explanation: Click on 'Help' -> 'Help Contents' -> expand 'CARiSMA' -> expand 'Checks' -> expand 'Static Checks' -> click on 'Secure Dependency'"
															+ " |");
	
											// mapping of documents and classes
											for (String d : documents) {
												for (String c : classes) {
													if (d.equalsIgnoreCase(c)) {
														host.appendToReport(
																"| Document: " + d + " is mapped to Class: " + c + ". |\n");
													}
												}
											}
	
											// mapping of roles and classes
											for (String r : roles) {
												for (String c : classes) {
													if (r.equalsIgnoreCase(c)) {
														host.appendToReport(
																"| Role: " + r + " is mapped to Class: " + c + ". |\n");
													}
												}
											}
										}

										if (securityRequirement.contentEquals("non-disclosure")) {
											host.appendToReport("\n| CARiSMA check = RABAC " + " |");
											host.appendToReport(
													"| Find explanation: Click on 'Help' -> 'Help Contents' -> expand 'CARiSMA' -> expand 'Checks' -> click on 'RABAC'"
															+ " |");
	
											// mapping of documents and classes
											for (String d : documents) {
												for (String c : classes) {
													if (d.equalsIgnoreCase(c)) {
														host.appendToReport(
																"| Document: " + d + " is mapped to Class: " + c + ". |\n");
													}
												}
											}
	
											// mapping of roles and classes
											for (String r : roles) {
												for (String c : classes) {
													if (r.equalsIgnoreCase(c)) {
														host.appendToReport(
																"| Role: " + r + " is mapped to Class: " + c + ". |\n");
													}
												}
											}
										}

										if (securityRequirement.contentEquals("non-productione")) {
											
											host.appendToReport("\n| CARiSMA check = RABAC " + " |");
											host.appendToReport(
													"| Find explanation: Click on 'Help' -> 'Help Contents' -> expand 'CARiSMA' -> expand 'Checks' -> click on 'RABAC'"
															+ " |");

											// mapping of documents and classes
											for (String d : documents) {
												for (String c : classes) {
													if (d.equalsIgnoreCase(c)) {
														host.appendToReport(
																"| Document: " + d + " is mapped to Class: " + c + ". |\n");
													}
												}
											}

											// mapping of roles and classes
											for (String r : roles) {
												for (String c : classes) {
													if (r.equals(c)) {
														host.appendToReport(
															"| Role: " + r + " is mapped to Class: " + c + ". |\n");
													}
												}
											}
										}
									}
								} catch (Exception e) {

								}
							}
						}
					}
				}
			}
			catch (Exception e) {
				e.printStackTrace();

				return false;

			}
			return true;

		}
		return false;

	}

	private Document loadSTSInputFromDB() {
		IPreferenceStore preferencesStore = CarismaGUI.INSTANCE.getPreferenceStore();

		String user = preferencesStore.getString(KEY_USER);
		String secret = preferencesStore.getString(KEY_SECRET);
		String url = preferencesStore.getString(KEY_URL);

		MongoDBRestAPI db = new MongoDBRestAPI(user, secret, url);

		String sts_collection = preferencesStore.getString(KEY_STS_COLLECTION);
		String sts_document = preferencesStore.getString(KEY_STS_DOCUMENT);
		String sts_field = preferencesStore.getString(KEY_STS_FIELD);

		Content content = db.read(new MongoDBDynamicConfiguration(url, sts_collection, sts_document, sts_field));
		if(content!=null){
			
			if (content.getFormat().compareTo(XML_DOM.ID) == 0) {
				return ((XML_DOM) content).getDocument();
			} else if (content.getFormat().compareTo(JSON.ID) == 0) {
				try {
					return new XML_DOM((JSON) content).getDocument();
				} catch (ContentException e) {
					e.printStackTrace();
					return null;
				}
			} else if(content.getFormat().compareTo(BASE64.ID)==0){
				try {
					return new XML_DOM((BASE64) content).getDocument();
				} catch (ContentException e) {
					e.printStackTrace();
					return null;
				}
			}
		}
		return null;
	}

	private Document loadSTSInputFromFile() {
		Shell activeShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		FileDialog dialog = new FileDialog(activeShell);
		dialog.open();
		return FileIO.read(new File(new File(dialog.getFilterPath()),dialog.getFileName()));
	}
}
