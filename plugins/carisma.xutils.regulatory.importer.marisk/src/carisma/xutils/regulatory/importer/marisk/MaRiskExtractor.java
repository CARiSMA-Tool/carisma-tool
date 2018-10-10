package carisma.xutils.regulatory.importer.marisk;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.net.ssl.HttpsURLConnection;

import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntry;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;


/**
 * @author dbuerger
 *
 */

public class MaRiskExtractor {
//	public static final String DEFAULT_MARISK_RESOURCE_SUBFOLDER = "resources" + File.separator;
//	/** Default MaRisk ontology save path */
//	public static final String DEFAULT_MARISK_ONTOLOGY_SAVEPATH = ExtractorConstants.DEFAULT_ONTOLOGY_OUTPUT_FOLDER + "MaRisk.owl";	
		/** the states to check if we are in the content of a paragraph. */
	private enum STATE { undefined, TEXT, SOLUTION; }
		/** Name for the importer. */
	public static final String EXTRACTOR_NAME = "MARiskExtractor";
//		/** Default MARisk subfolder name */
//	public static final String DEFAULT_MARISK_SUBFOLDER = "MaRisk" + File.separator;
		/** reference to the logView output (GUI). */
	private LogViewEntrySet logInput = null;
		/** the reader to parser the ontologyFilepath. */
	private SAXReader reader = null;
		/** placeholder. */
	private String textAnnotation = "";
		/** Text Annotation of the actual handled MARiskEntry. */
		/** the number of the current article. */
	private String currentNumber = "";
		/** the number of the subarticle. */
	private String currentSubNumber = "";
		/** the name of the current article. */
	private String currentName;
		/** the content of the article. */
	private String currentContent = "";
		/** the solution to the content. */
	private String currentAnnotation = "";
		/** the number of the MARiskBinding and the corresponding MARiskComment. */
	private int bindingCommentNumber = 0;
	/** the current state. */
	private STATE currentState = STATE.undefined;
		

		// the individuals for the ontology
	/**
	 * the current Article.
	 */
	private OWLNamedIndividual currentArticle = null;
	/**
	 * the current Subarticle.
	 */
	private OWLNamedIndividual currentSubArticle = null;
	/**
	 * the current Binding.
	 */
	private OWLNamedIndividual currentBinding = null;
	/**
	 * the current Comment.
	 */
	private OWLNamedIndividual currentComment = null;
	
			
	/**
	 * Default constructor initializes extractor with console output.
	 */
	public MaRiskExtractor() {
		this(null);
	}
	
	/**
	 * Constructor.
	 */
	public MaRiskExtractor(final LogViewEntrySet newLogInput) {
		logInput = newLogInput;
		reader = new SAXReader();
		reader.setIncludeExternalDTDDeclarations(false);
		reader.setIncludeInternalDTDDeclarations(true);
			// if we get a NULLPOINTEREXCEPTION, we have to check in the resolver if we have the right dtd stored
			// this only can happen, when we add a new .html ontologyFilepath
		reader.setEncoding("UTF-8");
		HttpsURLConnection.setFollowRedirects(false);
	}
	
	/** Resets the parser. 
	 */
	private void resetParser() {
		textAnnotation = "";
		currentNumber = "";
		currentSubNumber = "";
		currentName = "";
		currentContent = "";
		currentAnnotation = "";
		bindingCommentNumber = 0;
		currentState = STATE.undefined;
	}
	
	/** Creates a new ontology which is wrapped in the returned {@link RegulatoryOntologyHelper}. 
	 * It will contain all MARisk content found in .html files within the sourceFolder. 
	 * @param sourceFolder folder to look for .html files and dtds
	 * @return a {@link RegulatoryOntologyHelper} which wraps the new created and filled ontology.
	 */
	public final RegulatoryOntologyHelper extract(final File sourceFolder) {
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper();
		roh.createNewRegulatoryOntology();
		extract(sourceFolder, roh);
		return roh;
	}
	
	/** Adds all MARisk content found in .html files within the sourceFolder to the ontology wrapped in the ontologyContainer.
	 * 
	 * @param sourceFolder folder to look for .html files and dtds.
	 * @param ontologyContainer Wrapper for the modified ontology.
	 */
	public final void extract(final File sourceFolder, final RegulatoryOntologyHelper ontologyContainer) {
		File maRiskFolder = null;
		if (sourceFolder != null) {
			maRiskFolder = sourceFolder;
		} else {
			outputMessage("ERROR", "No source folder for MARisk Extractor given!", 1);
			return;
		}
		outputMessage(EXTRACTOR_NAME, "MARisk source path: " + maRiskFolder.getAbsolutePath() + File.separator, 1);		
		if (ontologyContainer == null) {
			outputMessage(EXTRACTOR_NAME, "No RegulatoryHelper present!", 1);
			return;
		}
		if (!maRiskFolder.exists() || !maRiskFolder.isDirectory()) {
			outputMessage(EXTRACTOR_NAME, "Source folder non-existent or not a directory!", 1);
			return;			
		}
		List<Node> nodes;
		try {
			nodes = parse(maRiskFolder);
		} catch (DocumentException e) {
			outputMessage("ERROR", "Error while reading the source!", 1);
			outputMessage("ERROR", e.getMessage(), 1);
			return;
		}

		handleNodes(nodes, ontologyContainer);
	}

	/**
	 * parses the given document and extracts articles and the according annotations.
	 * Searches in the given folder for any File with the ending ".html".
	 * @param maRiskFolder folder to look for .html files and dtds.
	 * @throws DocumentException 
	 */
	@SuppressWarnings("unchecked")
	private List<Node> parse(final File maRiskFolder) throws DocumentException {
		List<Node> nodes = new ArrayList<Node>();
		for (File maRiskFile : maRiskFolder.listFiles()) {
			if (!maRiskFile.getName().endsWith(".html")) {
				continue;
			}
			resetParser();
			reader.setStripWhitespaceText(true); 	// ignore the whitespaces between start- and end-Tags
			reader.setMergeAdjacentText(true);		// merge adjacent text nodes
			
				Document maRiskDocument = null;			// the document to store the xml-document
				outputMessage(EXTRACTOR_NAME, "Start reading the source ...", 3);
				//FIXME SW: hier ist ein Ordner hart verdrahtet
				// DW: ohne diesen Ordner wird im Internet gesucht,
				// und im Archiv sind die gesuchten Dateien immer an dieser Stelle
//				reader.setEntityResolver(new MARiskEntityResolver(""));
				File dtdFolder = new File(maRiskFolder.getAbsolutePath() + File.separator + "resources" + File.separator + "dtds" + File.separator);
				reader.setEntityResolver(new MARiskEntityResolver(dtdFolder.getPath()));
//				reader.setEntityResolver(new MARiskEntityResolver("resources" + File.separator + "dtds" + File.separator));  //tmp
				maRiskDocument = reader.read(maRiskFile);
				outputMessage(EXTRACTOR_NAME, "Finished reading the source ...", 3);
				nodes.addAll(maRiskDocument.selectNodes("//*"));
		}
		return nodes;
	}
	
	/**
	 * parses the content of the nodes. for each related entry, there will be created a new entry in the ontology.
	 * @param nodes the set of nodes
	 * @param ontologyContainer Wrapper for the modified ontology.
	 */
	private void handleNodes(final List<Node> nodes, final RegulatoryOntologyHelper ontologyContainer) {
		for (Node node : nodes) {
			// switch the state for each table cell
			if (node.getName().equals("td") && currentState == STATE.TEXT) { 
				currentState = STATE.SOLUTION;
			} else if (node.getName().equals("td") && currentState == STATE.SOLUTION) { 
				currentState = STATE.TEXT; 
			}
			
			// if we find the first headline, we have to set the state to text
			// and extract the number and the text of the headline
			if (node.getName().equals("strong") && node.getParent().getName().equals("h2")) {
				// for the first time we read a headline, we have to switch the state and set the text
				if (currentState == STATE.undefined && node.getText().matches("[0-9].*")) {
					currentState = STATE.TEXT;
				} 	// to check if the state is undefined
				if (currentState != STATE.undefined) {
					textAnnotation = node.getText();
					int index = getIndexOfFirstLetter(textAnnotation);

					currentNumber = textAnnotation.substring(0, index - 1);
					currentName = textAnnotation.substring(textAnnotation.indexOf(" ") + 1);
				
					if (currentNumber.matches("[0-9]{1,3}.")) {											// the other numbers like 7. 
						currentNumber = currentNumber.substring(0, currentNumber.indexOf("."));

						// if we are in a new clause, the subClause has to be deleted
						if (currentSubNumber.contains(".") && !currentNumber.equals(currentSubNumber.substring(0, currentSubNumber.lastIndexOf(".")))) {
							currentSubNumber = "";
						}

						currentArticle = ontologyContainer.createMARiskClause("MARiskVA_" + currentNumber, currentNumber, currentName);
					}
					// if we have found a subClause, we have to make a relation to the corresponding clause
					if (!currentSubNumber.equals("")) {
						ontologyContainer.createMARiskSubClause(currentArticle, currentSubArticle);
					}
					bindingCommentNumber = 0;
				}
				// to find the number of the article, we have to look for the node named "strong" 
				// and check if the parent is a node of type "h3"
			} else if (node.getName().equals("strong") && node.getParent().getName().equals("h3") && currentState != STATE.undefined) {
				textAnnotation = node.getText();
				int index = getIndexOfFirstLetter(textAnnotation);

				currentNumber = textAnnotation.substring(0, index - 1);
				currentName = textAnnotation.substring(index);
				if (!currentNumber.matches("[0-9]{1,3}. ")) { // TODO This regex doesn't match the comment underneath 
					// find numbers with no dot at the end, like 7.2.2.2
					currentSubNumber = currentNumber; 
					currentSubArticle = ontologyContainer.createMARiskClause("MARiskVA_" + currentSubNumber, currentSubNumber, currentName);
				}
				// if e have found a subClause, we have to create the relation to the corresponding clause
				if (!currentSubNumber.equals("")) {
					ontologyContainer.createMARiskSubClause(currentArticle, currentSubArticle);
				}
				bindingCommentNumber = 0;
				
			// find the content of an article and check if it does not represent the headline of an aritcle
			// if it does not represent the headline of an article, store it in an ontology
			} else if (node.getName().equals("td") && currentState == STATE.TEXT) {
				currentContent = node.getStringValue();	
				createMariskBinding(ontologyContainer);
				
			// find the content of an annotation and store it in an ontology
			} else if (node.getName().equals("td") && currentState == STATE.SOLUTION) {
				currentAnnotation = node.getStringValue();
				createMARiskComment(ontologyContainer);

			// if we have found the navToTop node, we can end with parsing
			} else if (node.getName().equals("p") && node.valueOf("@class").equals("navToTop")) {
				currentState = STATE.undefined;
				outputMessage(EXTRACTOR_NAME, "Done...", 3);
			}
		}
	}
	
	
	/** Handle new Bindings, and the relation to a MARiskClause.
	 * 
	 * @param ontologyContainer Wrapper for the modified ontology.
	 */
	private void createMariskBinding(final RegulatoryOntologyHelper ontologyContainer) {
		bindingCommentNumber++;
		int index = getIndexOfFirstLetter(currentContent);
		
		currentContent = currentContent.substring(index, currentContent.length());
		if (currentContent.length() > 3 && !currentContent.matches("[0-9]{1,3}\\..*")) { // no empty strings an no headline
			currentContent = currentContent.replace("&nbsp;", " ");
			// create a new binding either with the relation to the subArticle or the article
			if (!currentSubNumber.equals("")) {
				currentBinding = ontologyContainer.createMARiskBindingEntry("MARiskVA_" + currentSubNumber + "_B" + bindingCommentNumber, 
					String.valueOf(bindingCommentNumber), currentContent);
				ontologyContainer.createMARiskEntry(currentSubArticle, currentBinding);
			} else {
				currentBinding = ontologyContainer.createMARiskBindingEntry("MARiskVA_" + currentNumber + "_B" + bindingCommentNumber, 
					String.valueOf(bindingCommentNumber), currentContent);
				ontologyContainer.createMARiskEntry(currentArticle, currentBinding);
			}						
		}
	}
	
	/** Handle new MARiskComments, and the relation to a MARiskClause and the Binding.
	 * Iff the text-content length is longer than 3.
	 * 
	 * @param ontologyContainer Wrapper for the modified ontology.
	 */
	private void createMARiskComment(final RegulatoryOntologyHelper ontologyContainer) {
		if (currentAnnotation.length() > 3) {	// check for an empty string
			currentAnnotation = currentAnnotation.replace("&nbsp;", " ");
			// create a new comment either with the relation to the subArticle or the article
			if (!"".equals(currentSubNumber)) {
				currentComment = ontologyContainer.createMARiskCommentEntry("MARiskVA_" + currentSubNumber + "_C" + bindingCommentNumber, 
					String.valueOf(bindingCommentNumber), currentAnnotation);
				ontologyContainer.createMARiskEntry(currentSubArticle, currentComment);
			
			} else {
				currentComment = ontologyContainer.createMARiskCommentEntry("MARiskVA_" + currentNumber + "_C" + bindingCommentNumber, 
					String.valueOf(bindingCommentNumber), currentAnnotation);
				ontologyContainer.createMARiskEntry(currentArticle, currentComment);
			}
			// create a new relation to the corresponding binding
			ontologyContainer.createMARiskBinding(currentBinding, currentComment);
		}
	}
	
	/** Searches for the first index of a Character which matches [A-Za-z].
	 * 				<br><br>
	 *  OLDComment [to find the index where the name of the article starts
	 *  only to search for " " is not enough, sometimes the method 
	 *  finds the wrong index ]
	 * 
	 * @param text analyzed string
	 * @return The index of the first Character or 0 if the String has not been found.
	 */
	private int getIndexOfFirstLetter(final String text) {
		int index = 0;
		Pattern pattern = Pattern.compile("[A-Za-z]");
		Matcher matcher = pattern.matcher(text);
		while (matcher.find()) {
			if (index < 1) {
				index = matcher.start();
				break;
			}
		}
		return index;
	}
	
	/**
	 * Send a message either to the command line output or to the GUI, if present.
	 * @param importerName - A prefix for the GUI output
	 * @param message - the message to print
	 */
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
	
	/**
	 * main.
	 * @param args the start parameter
	 */
	public static void main(final String[] args) {
		String ontologySavepath = "";
		String sourceFolder = "";
		if (args.length != 2) {
			System.err.println("No path for ontology and/or no source folder given!");
			return;
		} else {
			sourceFolder = args[0];
			ontologySavepath = args[1];
		}

		MaRiskExtractor extractor = new MaRiskExtractor();
		
		RegulatoryOntologyHelper roh = extractor.extract(new File(sourceFolder));
		
		System.out.println("Storing the MaRisk ontology to " + ontologySavepath + "!");
		roh.saveOntologyToFile(new File(ontologySavepath));
			
	}
}