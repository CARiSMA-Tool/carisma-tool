package carisma.check.createhelpdocument;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.io.File;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Deployment;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.StateMachine;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Element;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLDeploymentHelper;
import carisma.modeltype.uml2.UMLHelper;

public class CreateHelpDocument implements CarismaCheck {
	AnalysisHost host;
	
	String linkToRabacHelp = "<a href='RABAC Helpfile' target='_blank'>/carisma.check.rabac/help/html/maintopic.html</a";
	String linkToSecureLinksHelp = "<a href='SecureLinks Helpfile' target='_blank'>/carisma.check.staticcheck/help/html/securelinks.html</a";	

	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		// TODO Auto-generated method stub
		this.host = host;
		Resource model = host.getAnalyzedModel();

		if (model.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}

		if (model.getContents().get(0) instanceof Package) {
			Package content = (Package) model.getContents().get(0);

			try {
				InputFileParameter inputFile = (InputFileParameter) (parameters
						.get("carisma.check.createHelpDocument.STSinput"));
				File file = inputFile.getValue();

				DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
				DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
				Document doc = dBuilder.parse(file);
				doc.getDocumentElement().normalize();

				//host.appendToReport("Root element :" + doc.getDocumentElement().getNodeName());

				NodeList elements = doc.getElementsByTagName("commitment"); // return
																			// all
																			// commitments
																			// and
																			// store
																			// them
																			// in
																			// nList
				host.appendToReport("----------------------------");

				HashSet<String> roles = new HashSet<String>();
				HashSet<String> nodeNames = new HashSet<String>();

				for (int temp = 0; temp < elements.getLength(); temp++) {

					Node nNode = elements.item(temp);
					// host.appendToReport("\nCurrent Element :" +
					// nNode.getNodeName() ); // print current node name

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
								String debtorAttribute;
								String creditorAttribute;

								Node nodePre = ePreCon.item(i);
								if (nodePre.getNodeType() == Node.ELEMENT_NODE) {
									Element preconditionElement = (Element) nodePre;

									/***********************************************
									 * GET SOURCE ATTRIBUTES
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
									 * GET DESTINATION ATTRIBUTES
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

									String preConditionType = nodePre.getNodeName();

									// host.appendToReport("precondition = " +
									// preConditionType + " | ");

								}

							}
						}

						// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
						// System.out.println("Postcondition : " +
						// eElement.getElementsByTagName("postcondition").item(0).getTextContent());

						Node post = eElement.getElementsByTagName("postcondition").item(0);
						NodeList ePost = post.getChildNodes();
						for (int i = 0; i < ePost.getLength(); i++) {
							Node nodePost = ePost.item(i);

							String securityRequirement = nodePost.getNodeName();
							if (nodePost.getNodeType() == Node.ELEMENT_NODE) {
								host.appendToReport("\n| security requirement: " + nodePost.getNodeName() + " | ");

								// Example
								if (securityRequirement == "integrity") {
									host.appendToReport("\n| CARiSMA check = Securelinks "  + " |"
											+ "\n| UML Diagram = Deployment Diagram | \n");

									Set<Dependency> dependencies = UMLDeploymentHelper.getAllDependencies(content);

									Set<Artifact> artifacts = UMLDeploymentHelper
											.getAllArtifacts(helper.makeCollection(dependencies));
									for (Dependency d : dependencies) {

										Set<CommunicationPath> comPath = UMLDeploymentHelper.getCommunicationPaths(d);

										for (CommunicationPath p : comPath) {
											List<org.eclipse.uml2.uml.Node> nodes = UMLDeploymentHelper.getNodes(p);
											// host.appendLineToReport(nodes.toString());

											for (int g = 0; g < nodes.size(); g++) {
												nodeNames.add(nodes.get(g).getLabel());
												// host.appendLineToReport("\n
												// node: " +
												// nodes.get(g).getLabel());
											}
										}
									}

									// map
									host.appendLineToReport("| nodes in deployment diagram: " + nodeNames.toString() + "|");
									host.appendLineToReport("| roles in STS model: " + roles.toString() + "|");

									for (String r : roles) {
										for (String n : nodeNames) {
											if (r.equals(n)) {
												host.appendToReport("| Role: " + r + " is mapped to Node: " + n + ". |\n");
											}
										}
									}

								}
							}

							if (securityRequirement == "needToKnow") {

								host.appendToReport("\n| CARiSMA check = RABAC "  + " |");// and
																				// Authorized-Status

							}

							if (securityRequirement == "non-disclosure") {
								host.appendToReport("\n| CARiSMA check = RABAC "  + " |");
							}

							if (securityRequirement == "non-productione") {
								host.appendToReport("\n| CARiSMA check = RABAC "  + " |");
							}

						}
					}

				}

			}

			catch (Exception e) {
				e.printStackTrace();

				return false;

			}

			/*
			 * 
			 * end-qusai
			 * 
			 */
			return true;

		}
		return false;

	}
}
