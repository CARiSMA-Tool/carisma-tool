package carisma.core.analysis;

import java.io.StringReader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;


import carisma.core.Carisma;
import carisma.core.checks.CheckDescriptor;
import carisma.core.checks.CheckRegistry;

public class AutomatedAnalysis {

	final String rabacId1 = "carisma.check.rabac.configuration";
	final String rabacId2 = "carisma.check.rabac";
	final String securelinksId = "carisma.check.staticcheck.securelinks";
	final String securedependencyId = "carisma.check.staticcheck.securedependency";

	List<CheckDescriptor> checkDescriptors = Carisma.getInstance().getCheckRegistry().getRegisteredChecks();

	List<CheckReference> checks = new ArrayList<CheckReference>();
	Path path = null;
	String pathstring = "";
	CheckRegistry cr = new CheckRegistry();

	public AutomatedAnalysis(String helpDocument, IContainer container) {

		/*
		 * initializing the Checkregistry
		 * 
		 */

		cr.initialize();

		CheckDescriptor rabacconf = cr.getCheckDescriptor(rabacId1);
		CheckDescriptor rabac = cr.getCheckDescriptor(rabacId2);
		CheckDescriptor secureLinks = cr.getCheckDescriptor(securelinksId);
		CheckDescriptor secureDependency = cr.getCheckDescriptor(securedependencyId);

		Set<String> keywords = new HashSet<String>();
		String reportDump = "";

		Set<String> checkIds = new HashSet<String>();
		checkIds.add("RABAC");
		checkIds.add("SecureLinks");
		checkIds.add("SecureDependency");

		/*
		 * parse XML file
		 * 
		 */

		try {
			DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();

			InputSource is = new InputSource();
			is.setCharacterStream(new StringReader(helpDocument));

			Document doc = dBuilder.parse(is);

			doc.getDocumentElement().normalize();
			NodeList elements = doc.getElementsByTagName("*");

			for (int temp = 0; temp < elements.getLength(); temp++) {

				Node nNode = elements.item(temp);
				// System.out.println(nNode.toString());

				if (nNode.getNodeType() == Node.ELEMENT_NODE) {
					Element eElement = (Element) nNode;
					if (eElement.getTagName().equals("Filepath")) {
						pathstring = eElement.getTextContent().replace('\\', '/');

					}

					if (eElement.getTagName().equals("ReportDump")) {
						reportDump = eElement.getTextContent();
					}
				}

			}

			/*
			 * create tokens from reportDump and put them in hashset
			 */

			StringTokenizer tokenizer = new StringTokenizer(reportDump);
			while (tokenizer.hasMoreTokens()) {
				keywords.add(tokenizer.nextToken());
			}

			/*
			 * find keywords "RABAC" "SecureDependency" oder "SecureLinks" and
			 * add them to a list
			 */

			for (String s : keywords) {

				if (s.equalsIgnoreCase("SecureDependency")) {

					CheckReference sd = cr.createReference(secureDependency);
					sd.setEnabled(false);
					checks.add(sd);
				}

				if (s.equalsIgnoreCase("SecureLinks")) {

					CheckReference sl = cr.createReference(secureLinks);
					sl.setEnabled(false);
					checks.add(sl);

				}

				if (s.equalsIgnoreCase("RABAC")) {

					CheckReference r1 = cr.createReference(rabacconf);
					r1.setEnabled(false);
					CheckReference r2 = cr.createReference(rabac);
					r2.setEnabled(false);

					checks.add(r1);
					checks.add(r2);
				}
			}

			/*
			 * new Analysis
			 * 
			 */

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public Analysis getAnalysis() {

		IFile file1 = (IFile) ResourcesPlugin.getWorkspace().getRoot().findMember(pathstring.replace('\\', '/'));

		String name = "automated_analysis_" + carisma.core.util.Utils.getISOTimestamp();
		Analysis ana = new Analysis(name, "UML2", file1);

		for (int i = 0; i < checks.size(); i++) {
			ana.getChecks().add(checks.get(i));
		}

		return ana;

	}

	public String getPathstring() {
		return pathstring.substring(0, pathstring.lastIndexOf('/')) + "/";
	}

	public CheckDescriptor getCheckDescriptor(String name) {
		for (CheckDescriptor cd : checkDescriptors) {
			if (name.equalsIgnoreCase(cd.getName())) {
				return cd;
			}
		}
		return null;
	}

}
