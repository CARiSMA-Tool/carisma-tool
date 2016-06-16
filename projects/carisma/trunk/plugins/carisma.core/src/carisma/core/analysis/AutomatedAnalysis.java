package carisma.core.analysis;

import java.util.List;
import java.io.StringReader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
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



public class AutomatedAnalysis {

	final String rabacId1 = "carisma.check.rabac.configuration";
	final String rabacId2 = "carisma.check.rabac";
	final String securelinksId = "carisma.check.staticcheck.securelinks";
	final String securedependencyId = "carisma.check.staticcheck.securedependency";

	List<CheckReference> checks = new ArrayList<CheckReference>();
	Path path = null;
	String pathstring = "";

	public AutomatedAnalysis(String helpDocument, IContainer container) {

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
			//	System.out.println(nNode.toString());

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
				if (keywords.contains(s)) {

					if (s.equalsIgnoreCase("SecureDependency")) {

						CheckReference sd = new CheckReference(securedependencyId, false);
						checks.add(sd);
					}

					if (s.equalsIgnoreCase("SecureLinks")) {

						CheckReference sl = new CheckReference(securelinksId, false);
						checks.add(sl);

					}

					if (s.equalsIgnoreCase("RABAC")) {

						CheckReference r1 = new CheckReference(rabacId1, false);
						CheckReference r2 = new CheckReference(rabacId2, false);

						checks.add(r1);
						checks.add(r2);

					}

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

	
	public String getPathstring(){
		return pathstring.substring(0, pathstring.lastIndexOf('/')) + "/";
	}
}