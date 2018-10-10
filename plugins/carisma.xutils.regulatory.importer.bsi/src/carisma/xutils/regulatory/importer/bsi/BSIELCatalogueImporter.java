package carisma.xutils.regulatory.importer.bsi;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Scanner;
import java.util.Stack;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

import carisma.xutils.regulatory.importer.bsi.datamodel.BSICatalogue;
import carisma.xutils.regulatory.importer.bsi.datamodel.BSICategory;
import carisma.xutils.regulatory.importer.bsi.datamodel.BSIEntry;
import carisma.xutils.regulatory.importer.bsi.datamodel.BSIEntry.EntryStatus;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntry;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;


public class BSIELCatalogueImporter implements ContentHandler {
	private Locator locator;
	private Stack<Attributes> attStack;
	
	/**
	 * BSICatalogue.
	 */
	private BSICatalogue currentBSICatalogue;
	private int level;
	private Object parent;
	private String elementText;
	private BSIEntry lastEntry;
	private boolean isMainText;
	private LogViewEntrySet logInput;
	public static final String IMPORTER_NAME = "BSICatalogueImporter";
	private boolean isCategory = false;
	private boolean isEntry = false;
	private boolean isLink = false;
	private boolean isTable = false;
	private String href = "";

	public BSIELCatalogueImporter() {
		super();
		this.attStack = new Stack<Attributes>();
		this.currentBSICatalogue = new BSICatalogue();
		this.lastEntry = null;
	}
	
	public BSIELCatalogueImporter(LogViewEntrySet logInput) {
		this();
		this.logInput = logInput;
	}

	/**
	 * Returns the current BSICatalogue.
	 * @return the current BSICatlaogue
	 */
	public final BSICatalogue getCurrentCatalogue() {
		return currentBSICatalogue;
	}

	public final void setOutput(final BSICatalogue output) {
		this.currentBSICatalogue = output;
	}

	@Override
	public final void setDocumentLocator(final Locator locator) {
		// System.out.println("setDocumentLocator");
		this.locator = locator;
	}

	@Override
	public final void startDocument() throws SAXException {
		this.attStack.clear();
		this.isMainText = false;
	}

	@Override
	public void endDocument() throws SAXException {
		// TODO Auto-generated method stub

	}

	@Override
	public void startPrefixMapping(final String prefix, final String uri)
			throws SAXException {
		// TODO Auto-generated method stub

	}

	@Override
	public void endPrefixMapping(final String prefix) throws SAXException {
		// TODO Auto-generated method stub

	}

	@Override
	public final void startElement(final String uri, final String localName, final String qName,
			final Attributes atts) throws SAXException {
		this.elementText = "";
		String hrefVal = atts.getValue("href");
		String classVal = atts.getValue("class");

		switch (this.level) {
		case 1:
			if (localName.equals("A") && hrefVal != null && hrefVal.matches(".*[bgm]\\d+.*")) {
				isCategory = true;
				href = hrefVal;
			}
			break;
		case 2:
			if (localName.equals("A") && hrefVal != null && hrefVal.matches("[bgm]\\d+.*")) {
				isEntry = true;
				href = hrefVal;
			}
			break;
		case 3:
			if (localName.equals("H1")) {
				this.isMainText = true;
				isLink = false;
			}
			
			// Querverweise auf andere Eintraege:
			if (localName.equals("A") && hrefVal != null && hrefVal.matches(".*[bgm]\\d+.*")) { // interner Link auf anderen entry
				href = hrefVal;
				isLink = true;
			}
			// filter the tables of the main groups in the catalogue (e.g. B 1)
			if (localName.equals("UL") && classVal != null 
					&& (classVal.equals("liersteebene") || (classVal.equals("lizweiteebene")))) {
				isTable = true;
			}
			break;
		}

		this.attStack.push(atts);
	}

	@Override
	public final void endElement(final String uri, final String localName, final String qName)
			throws SAXException {
		// if(this.level==2 && this.lastEntry!=null){
		// this.lastEntry.setText(this.elementText);
		// this.lastEntry=null;
		// }
		this.attStack.pop();
		if (localName.equals("LI")) {
			isTable = false;
		}
	}

	@Override
	public final void characters(final char[] ch, final int start, final int length)
			throws SAXException {
		String realText = (new String(ch)).substring(start, start + length);
		this.elementText += realText;
		if (this.level == 1 && isCategory && realText.matches("[BGM]\\d+.*")) {
			String catID = realText.substring(0, 2);
			catID = catID.substring(0, 1) + " " + catID.substring(1);
			BSICategory newCategory = new BSICategory(realText.substring(3),
					catID, href);
			this.currentBSICatalogue.addCategory(newCategory);
			isCategory = false;
			href = "";
		} else if (this.level == 2 && isEntry && realText.matches("[BGM] \\d+.\\d+.*")) {
			int tempPos = 2 + realText.split(" ")[1].length();
			String entryID = realText.substring(0, tempPos);
			String entryTitle = realText.substring(tempPos + 1);
			BSIEntry newEntry = new BSIEntry(entryTitle, entryID,
					EntryStatus.unknown, "", href);
			((BSICategory) this.parent).addEntry(newEntry);
			// this.lastEntry=newEntry;
			isEntry = false;
			href = "";
		} 
		/* 
		 * TODO: check if all references are correct
		 */
		else if (this.level == 3 && isLink && realText.matches("[BGM][^SI].*\\d+.\\d+.*") && !isTable) {
			outputMessage(IMPORTER_NAME, "Link: " + href, 7);
			if (realText.contains("&#160;")) {
				realText = realText.replace("&#160;", " ");
			}
			// internen Namen bauen, z.B. "B_5.3"
			String [] titleParts = realText.split(" ");
			//TODO: den folgenden Check durch eine richtige Regex ersetzen
			if (titleParts.length >= 2) {
				String intRef = titleParts[0] + "_" + titleParts[1];
				outputMessage(IMPORTER_NAME, "interner Link: " + intRef, 7);
				((BSIEntry) this.parent).getRefs().add(intRef);
			}
			isLink = false;
			href = "";
		}
		if (this.level == 3 && realText.startsWith("© Bundesamt für Sicherheit ")) {
			this.isMainText = false;
		}

		if (this.level == 3
				&& this.isMainText
				&& !(((BSIEntry) this.parent).getText().endsWith("\n") && realText
						.equals("\n"))) {
				// change the html codes in the text
			realText = realText.replace("&auml;", "ä");
			realText = realText.replace("&uuml;", "ü");
			realText = realText.replace("&ouml;", "ö");
			realText = realText.replace("&Auml;", "Ä");
			realText = realText.replace("&Uuml;", "Ü");
			realText = realText.replace("&Ouml;", "Ö");
			realText = realText.replace("&nbsp;", " ");
			realText = realText.replace("&szlig;", "ß");
			realText = realText.replace("&sect;", "§");
			((BSIEntry) this.parent).appendText(realText);
		}
		
		// System.out.println("characters: " + realText);

		// System.out.println(this.locator.getPublicId());
		// System.out.println(this.locator.getSystemId());
		// if(realText.contains("Infrastruktur")){
		// System.out.println("ldksjfls");
		// Attributes lastAtts = this.attStack.peek();
		// // System.out.println("Relevante Attribute: " +
		// lastAtts.getLength());
		// String titleVal=lastAtts.getValue("title");
		// String hrefVal=lastAtts.getValue("href");
		// if (hrefVal != null) {
		// System.out.println(lastAtts.getValue("href"));
		// }
		// for (int curAttIdx = 0; curAttIdx < lastAtts.getLength();
		// curAttIdx++) {
		// if (lastAtts.getQName(curAttIdx).equals("href")) {
		// // System.out.println(lastAtts.getLocalName(curAttIdx)+" "+
		// // lastAtts.getQName(curAttIdx)+" "+lastAtts.getURI(curAttIdx));
		// System.out.println(lastAtts.getValue(curAttIdx));
		// }
		// }
		// }
	}

	@Override
	public void ignorableWhitespace(final char[] ch, final int start, final int length)
			throws SAXException {
		// TODO Auto-generated method stub

	}

	@Override
	public void processingInstruction(final String target, final String data)
			throws SAXException {
		// TODO Auto-generated method stub

	}

	@Override
	public void skippedEntity(final String name) throws SAXException {
		// TODO Auto-generated method stub

	}

	public final void startParse(final String baseDir, final String filename)
			throws IOException, ParserConfigurationException, SAXException {
		String finalPath = baseDir;
		if ((!baseDir.endsWith(File.separator) && !filename.startsWith(File.separator))) {
			finalPath += File.separator;
		}
		finalPath += filename;
//		String fileContent = new Scanner(new File(finalPath),"UTF-8").useDelimiter("\\Z").next();
		InputSource inSource = new InputSource();
		inSource.setEncoding("UTF-8");
		inSource.setByteStream(new FileInputStream(finalPath));
		org.xml.sax.XMLReader reader = org.xml.sax.helpers.XMLReaderFactory.createXMLReader ("org.htmlparser.sax.XMLReader");
		reader.setContentHandler(this);
// Different tries to turn off the dtd validation in the reader
// By TH, but he doesn't know what these do except the validation feature
// FIXME: Koennen wir die entfernen?
//		reader.setFeature("http://xml.org/sax/features/validation",false);
//		reader.setFeature("http://xml.org/sax/features/external-general-entities",false);
//		reader.setFeature("http://apache.org/xml/features/validation/schema",false);
//		reader.setFeature("http://xml.org/sax/features/external-parameter-entities",false);
//		reader.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar",false);
//		reader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
		reader.parse(inSource);

	}

	public final void doImport(final File sourceFolder) throws IOException, ParserConfigurationException,
			SAXException {
		String baseDir = sourceFolder.getPath() + File.separator;
		setLevel(1);
		startParse(baseDir, "kataloge.html");
		outputMessage(IMPORTER_NAME, "Parsing " + sourceFolder.getAbsolutePath(), 3);
		
		setLevel(2);
		for (BSICategory curCategory : currentBSICatalogue.getCategories()) {
			parent = curCategory;
			startParse(baseDir, curCategory.getURL());
			outputMessage(IMPORTER_NAME, "" + curCategory.getEntries().size() + " Elemente in Kategorie " + curCategory.getName(), 6);
//			break; //debug
		}

		setLevel(3);
		for (BSICategory curCategory : this.currentBSICatalogue.getCategories()) {
			//hack, weil sonst das Verzeichnis fehlt:
			String tempDir = curCategory.getURL().split("/")[0] + "/"
					+ curCategory.getURL().split("/")[1] + "/";

			for (BSIEntry curEntry : curCategory.getEntries()) {
				outputMessage(IMPORTER_NAME, curEntry.getId() + " "
						+ curEntry.getTitle(), 6);				
				this.parent = curEntry;
				startParse(baseDir, tempDir + curEntry.getURL());
			}
//			break; //debug
		}
	}
	
	private void outputMessage(final String importerName, final String message, final int level) {
		if (logInput != null) {
			logInput.add(new LogViewEntry(importerName, message, level));
			if (logInput.showDebug()) {
				System.out.println("[" + importerName + ".debug] " + message);
			}
		} else {
			if (importerName.equals("ERROR")) {
				System.err.println(message);
			} else {
				System.out.println(message);
			}
		}
	}
	
	private void outputMessage(final String importerName, final String message) {
		this.outputMessage(importerName, message, 0);
	}
	
	/**
	 * This method sets the level to the given level.
	 * @param newLevel the new level
	 */
	public final void setLevel(final int newLevel) {
		this.level = newLevel;
	}
}
