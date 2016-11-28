package carisma.ui.vision.automatedanalysis;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import carisma.check.rabac.RABACCheck;
import carisma.check.staticcheck.securedependency.SecureDependenciesCheck;
import carisma.check.staticcheck.securelinks.SecureLinksCheck;
import carisma.core.Carisma;
import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;
import carisma.core.analysis.Analyzer;
import carisma.core.analysis.CheckReference;
import carisma.core.analysis.result.AnalysisResult;
import carisma.core.checks.CheckDescriptor;
import carisma.core.checks.CheckRegistry;
import carisma.ui.eclipse.EclipseUIConnector;

public class AutomatedAnalysis {

	List<CheckDescriptor> checkDescriptors = Carisma.getInstance().getCheckRegistry().getRegisteredChecks();

	List<CheckReference> checks = new ArrayList<CheckReference>();
	Path path = null;
	String pathstring = "";
	CheckRegistry cr = new CheckRegistry();

	public AutomatedAnalysis(String helpDocument) {

		/*
		 * initializing the Checkregistry
		 * 
		 */

		this.cr.initialize();

		CheckDescriptor rabacconf = this.cr.getCheckDescriptor(RABACCheck.PARAM_CONFIGURATION);
		CheckDescriptor rabac = this.cr.getCheckDescriptor(RABACCheck.CHECK_ID);
		CheckDescriptor secureLinks = this.cr.getCheckDescriptor(SecureLinksCheck.CHECK_ID);
		CheckDescriptor secureDependency = this.cr.getCheckDescriptor(SecureDependenciesCheck.CHECK_ID);

		Set<String> keywords = new HashSet<String>();
		String reportDump = "";

		Set<String> checkIds = new HashSet<String>();
		checkIds.add("CHECK_ID");
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
						this.pathstring = eElement.getTextContent().replace('\\', '/');

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
			 * find keywords "CHECK_ID" "SecureDependency" oder "SecureLinks" and
			 * add them to a list
			 */

			for (String s : keywords) {

				if (s.equalsIgnoreCase("SecureDependency")) {

					CheckReference sd = CheckRegistry.createReference(secureDependency);
					sd.setEnabled(false);
					this.checks.add(sd);
				}

				if (s.equalsIgnoreCase("SecureLinks")) {

					CheckReference sl = CheckRegistry.createReference(secureLinks);
					sl.setEnabled(false);
					this.checks.add(sl);

				}

				if (s.equalsIgnoreCase("CHECK_ID")) {

					CheckReference r1 = CheckRegistry.createReference(rabacconf);
					r1.setEnabled(false);
					CheckReference r2 = CheckRegistry.createReference(rabac);
					r2.setEnabled(false);

					this.checks.add(r1);
					this.checks.add(r2);
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

		IFile file1 = (IFile) ResourcesPlugin.getWorkspace().getRoot().findMember(this.pathstring.replace('\\', '/'));

		String name = "automated_analysis_" + carisma.core.util.Utils.getISOTimestamp();
		Analysis ana = new Analysis(name, "UML2", file1);

		for (int i = 0; i < this.checks.size(); i++) {
			ana.getChecks().add(this.checks.get(i));
		}

		return ana;

	}

	public String getPathstring() {
		return this.pathstring.substring(0, this.pathstring.lastIndexOf('/')) + "/";
	}

	public CheckDescriptor getCheckDescriptor(String name) {
		for (CheckDescriptor cd : this.checkDescriptors) {
			if (name.equalsIgnoreCase(cd.getName())) {
				return cd;
			}
		}
		return null;
	}
	
	public final static void startAutomatedAnalysis(final AnalysisResult analysisResult) {

		Analysis analysis = analysisResult.getAnalysis();
		IFile iFile = analysis.getIFile();
		IContainer container = iFile.getParent();
		String report = "";

		try (ByteArrayOutputStream out = new ByteArrayOutputStream()){

			JAXBContext context = JAXBContext.newInstance(carisma.core.analysis.result.AnalysisResult.class);
			Marshaller m = context.createMarshaller();
			m.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
			m.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
			m.marshal(analysisResult, out);

			report = new String(out.toByteArray(), StandardCharsets.UTF_8);

			System.out.println("STRING Report:");
			System.out.println(report);

			out.close();
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		AutomatedAnalysis ana = new AutomatedAnalysis(report);
		Analyzer a = new Analyzer();

		a.runAnalysis(ana.getAnalysis(), new EclipseUIConnector());

		String name = ana.getAnalysis().getName() + ".adf";
		try {
			IFile file = null;
			if (container instanceof IFolder) {
				IFolder folder = (IFolder) container;
				file = folder.getFile(name);
				if (!file.exists()) {
					try {
						file.create(new ByteArrayInputStream(new byte[0]), IResource.NONE, null);
					} catch (CoreException e) {
						e.printStackTrace();
					}
				}

			} else if (container instanceof IProject) {
				IProject project = (IProject) container;
				file = project.getFile(name);

				if (!file.exists()) {
					try {
						file.create(new ByteArrayInputStream(new byte[0]), IResource.NONE, null);
					} catch (CoreException e) {
						e.printStackTrace();
					}

				}
				AnalysisUtil.storeAnalysis(ana.getAnalysis(), file.getRawLocation().toString());

				file.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
			}
		} catch (CoreException e1) {
			e1.printStackTrace();
		}

	}

}
