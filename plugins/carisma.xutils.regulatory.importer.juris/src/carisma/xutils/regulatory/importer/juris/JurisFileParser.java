package carisma.xutils.regulatory.importer.juris;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntry;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;


/**
 * this is the parser for the html version of the desired law-entry.
 * 
 *  @author dbuerger
 */
public class JurisFileParser {
	
//		/** the states to check if we are in the content of a paragraph. */
//	private enum STATE{
//		jurAbsatz,
//		table,
//		undefined;
//	}
//	
		/** store the articles to find references. */
	private class Article{
		protected OWLNamedIndividual paragraph = null;
		protected String paragraphNumber = "";
		protected String sectionNumber = "";
		protected String content = "";	
		protected OWLNamedIndividual section = null;
		
		public Article (OWLNamedIndividual paragraph, String paragraphNumber, OWLNamedIndividual section, String sectionNumber, String content) {
			this.paragraph = paragraph;
			this.paragraphNumber = paragraphNumber;
			this.content = content;
			this.sectionNumber = sectionNumber;
			this.section = section;
		}
	}
		/** Name for the importer. */
	public static final String IMPORTER_NAME = "JurisFileParser";
	/** the List to store the paragraphs found. */
	private static List<String> extractedParagraphs;
		/** the reader to parser the file. */
	private SAXReader reader = null;
		/** the id of the current law entry. */
	private String currentID = "";
		/** the number of the current paragraph. */
	private String currentParagraphNumber = "";
		/** list to store the paragraphs for the references. */
	private ArrayList<Article> paragraphList = new ArrayList<Article>();
		/** the ontology helper. */
//	private RegulatoryOntologyHelper roh = null;
		/** the number of the current Section. */
	private String currentSectionNumber = "";
		/** reference to the logView output (GUI). */
	private LogViewEntrySet logInput = null;
	
	
	private OWLNamedIndividual currentParagraph = null; //e.g. 1 + Name
	private OWLNamedIndividual currentSection = null;   //e.g. (1) <Text of the section within the paragraph>
		
	/**
	 * constructor of the Law Parser.
	 * @param lawFolderPath the file to store the ontology 
	 * @param logInput the input
	 */
	public JurisFileParser(final File dtdFolder, LogViewEntrySet logInput) {
		this.logInput = logInput;
		reader = new SAXReader();
			// to validate offline against a DTD
		reader.setIncludeExternalDTDDeclarations(false);
		reader.setIncludeInternalDTDDeclarations(true);
		reader.setValidation(false);
		reader.setEntityResolver(new JurisEntityResolver(dtdFolder.getPath()));
			// if we get a NULLPOINTEREXCEPTION, we have to check in the resolver if we have the right dtd stored
			// this only can happen, when we add a new .html file
		reader.setStripWhitespaceText(true); 	// ignore the whitespaces between start- and end-Tags
		reader.setMergeAdjacentText(true);		// merge adjacent text nodes
		
			// not validate against a DTD
//		try {
//			reader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd",false);
//		} catch (SAXException e) {
//			System.err.println("Failed to set the Feature!");
//			e.printStackTrace();
//		}
			// to avoid server timeouts
//		HttpsURLConnection.setFollowRedirects(false);
	}
	
	/**
	 * resets the parser.
	 */
	private void resetParser() {
		currentID = "";
		currentParagraphNumber = "";
		currentSectionNumber = "";
		paragraphList.clear();
		currentParagraph = null;
		currentSection = null;
	}
	
	/**
	 * checks if the node is a law name.
	 * @param node the node
	 * @return true if isLawName
	 */
	private boolean isLawName(final Node node) {
		if (node.getName().equals("span") 
				&& node.getParent().getName().equals("h1") 
				&& node.valueOf("@class").equals("jnlangue")) {
			return true;
		}
		return false;
	}
	
	/**
	 * checks if the node is a law name abbreviation.
	 * @param node the node
	 * @return true if isLawNameAbbreviation
	 */
	private boolean isLawNameAbbreviation(final Node node) {
		if (node.getName().equals("span") 
				&& node.getParent().getName().equals("h1") 
				&& node.valueOf("@class").equals("jnamtabk")) {
			return true;
		}
		return false;
	}
	
	/**
	 * checks if the node is a section. 
	 * @param node the node 
	 * @return true if isSetion
	 */
	private boolean isSection(final Node node) {
		if (node.getName().equals("span") 
				&& node.valueOf("@class").equals("jnenbez") 
				&& node.getParent().getParent().getParent().valueOf("@title").equals("Einzelnorm")) {
			return true;
		}
		return false;
	}
	
	/**
	 * checks if the node is a subsection.
	 * @param node the node
	 * @return true if isSubSection
	 */
	private boolean isSubsection(final Node node) {
		if (node.getName().equals("div") && node.valueOf("@class").equals("jurAbsatz")
				&& node.getParent().getParent().getParent().valueOf("@id").equals(currentID) && currentID != "") {
			return true;
		}
		return false;
	}
	
	/**
	 * checks if the node belongs to a paragraph.
	 * @param node the node
	 * @return true if isParagraph
	 */
	private boolean isParagraph(final Node node) {
		if (node.getName().equals("span") && node.valueOf("@class").equals("jnentitel") 
				&& node.getParent().getParent().getParent().valueOf("@id").equals(currentID) && currentID != "") {
			return true;
		}
		return false;
	}
	
	/**
	 * checks if the paragraph is a footer.
	 * @param node the node
	 * @param content the content
	 * @return true if isFooter
	 */
	private boolean isFooter(final Node node, final String content) {
		if (node.getName().equals("div") && node.valueOf("@id").equals("fusszeile") && !content.equals("")) {
			return true;
		}
		return false;
	}
	
	/**
	 * this method parses the given input source and creates a new 
	 * ontology file. Therefore it is necessary to read out all nodes and
	 * check all these nodes for relevance. If a relevant node was found, 
	 * all informations and references (actually only from the same code of law)
	 * were retained in the ontology file which has been given to the constructor.
	 * @param jurisFile the file to be parsed
	 * @param ontologyContainer  the ontology helper
	 * @return true if the parsing was succesful
	 */
	public final boolean parse(final File jurisFile, final RegulatoryOntologyHelper ontologyContainer) {
		Document jurisDocument = null;				// the document to store the xml-document
		outputMessage(IMPORTER_NAME, "Start parsing file " + jurisFile.getName(), 3);
		try {
			jurisDocument = reader.read(jurisFile);
		} catch (DocumentException e) {
			outputMessage("ERROR", "Failed to read the document, caused by " + e.getLocalizedMessage(), 1);
			e.printStackTrace();
			return false;
		}
		resetParser();
		outputMessage(IMPORTER_NAME, "Finished reading " + jurisFile.getName(), 3);
		
		@SuppressWarnings("unchecked")
		List<Node> nodes = jurisDocument.selectNodes("//*");
		
		String lawName = "";
		String lawNameAbbreviation = "";
		OWLNamedIndividual currentLaw = null;
		String currentParagraphContent = "";
		
		for (Node node : nodes) {
				// if we have found the title, we have to store it  ...
			
			if (isLawName(node)) {
				lawName = node.getText();
			} else if (isLawNameAbbreviation(node)) {
				lawNameAbbreviation = node.getText();
					// get rid of the brackets
				if (lawNameAbbreviation.matches(".*\\(.*\\).*")) {
					lawNameAbbreviation = lawNameAbbreviation.substring(lawNameAbbreviation.indexOf("(") + 1);
					lawNameAbbreviation = lawNameAbbreviation.substring(0, lawNameAbbreviation.indexOf(")"));
				}
//				outputMessage(IMPORTER_NAME,"Creating new law: " + lawName + " (short name: " + lawNameAbbreviation + ")");
				currentLaw = ontologyContainer.createLaw(lawNameAbbreviation, lawName);
			}
			try {	// check for the different cases which can occur
				
					// if we start with a paragraph, we have to store the id
					// and the number of the paragraph
				if (isSection(node)) {
					// before we start to parse the next paragraph, we have to store the previous one
					if (!currentParagraphContent.equals("") && !currentParagraphNumber.equals("")) {
						currentSection = ontologyContainer.createSection(currentParagraph, currentSectionNumber, currentParagraphContent);
						currentParagraphContent = "";
					}
					String nodeText = node.getText();
					if (!nodeText.contains("Anlage")) {	// if we found an attachment, it will be ignored
						currentParagraphNumber = nodeText.substring(nodeText.indexOf(" ")).trim();
						currentID = node.getParent().getParent().getParent().valueOf("@id");
						currentSectionNumber = "";
					}
					
					// if  we have found the name of a paragraph 
					// then we have to check the id and store the name
				} else if (isParagraph(node)) {
					String paragraphTitle = node.getText();
//					outputMessage(IMPORTER_NAME,"Creating new paragraph: " + currentParagraphNumber + " (title: " + paragraphTitle + ")");
					if (!currentParagraphNumber.matches(".*(( bis )|(, )|( und ))+.*")) {
						currentParagraph = ontologyContainer.createParagraph(currentLaw, currentParagraphNumber, paragraphTitle);
					} else {
//						outputMessage(IMPORTER_NAME,"Found enumeration " + currentParagraphNumber);
						for (String paragraphNumber : getParagraphNumbers(currentParagraphNumber)) {
								
								// if there are enumerations, we have to ensure that each paragraph is mentioned alone
							currentParagraph = ontologyContainer.createParagraph(currentLaw, paragraphNumber, paragraphTitle);
//								outputMessage(IMPORTER_NAME,"Creating new section: " + str + " (text: " + currentContent + ")");
						}
						currentParagraphContent = "";
						currentParagraphNumber = "";
					}
											
					// if we have a section, we have to check the id and to concatenate the section content
				} else if (isSubsection(node)) {
						
						// get all text from the underlying nodes (such as tables for example)
					String text = node.getStringValue();
						
						// check if the paragraph starts with a number
					if (text.matches("\\(\\d{1,4}[a-z]?\\).*")) {
						String substring = text.substring(0, text.indexOf(" "));
						currentParagraphContent = text.substring(text.indexOf(substring) + substring.length() + 1);
						currentSectionNumber = substring.substring(substring.indexOf("(") + 1, substring.indexOf(")"));
					} else {	
							// else set the number to '1'
						currentSectionNumber = "1";
						currentParagraphContent = text;
					}
					Pattern pattern = Pattern.compile("\\.\\([0-9]{1,4}[a-z]?\\)");
					Matcher matcher = pattern.matcher(currentParagraphContent);
					int index = 0;
					if (currentParagraphNumber.equals("")) { /* do nothing */	}
					else if (matcher.find()) {
						index = matcher.start();
						String first = currentParagraphContent.substring(0, index + 1);
						String second = currentParagraphContent.substring(currentParagraphContent.indexOf(first) + first.length());
							
							// save the first section
//						outputMessage(IMPORTER_NAME,"Creating new Section: " + currentParagraphNumber + " - " + currentSectionNumber);
						currentSection = ontologyContainer.createSection(currentParagraph, currentSectionNumber, first);
						paragraphList.add(new Article(currentParagraph, currentParagraphNumber, currentSection, currentSectionNumber, first));
							
							// split the second section in the section number and the content
						String secondSubstring = second.substring(0, second.indexOf(" "));
						currentParagraphContent = second.substring(second.indexOf(secondSubstring) + secondSubstring.length() + 1);
						currentSectionNumber = secondSubstring.substring(secondSubstring.indexOf("(") + 1, secondSubstring.indexOf(")"));
							
							// save the second section
//						outputMessage(IMPORTER_NAME,"Creating new Section: " + currentParagraphNumber + " - " + currentSectionNumber);
						currentSection = ontologyContainer.createSection(currentParagraph, currentSectionNumber, currentParagraphContent);
						paragraphList.add(new Article(currentParagraph, currentParagraphNumber, currentSection, currentSectionNumber, currentParagraphContent));
					} else {
							// store the current section
//						outputMessage(IMPORTER_NAME,"Creating new Section: " + currentParagraphNumber + " - " + currentSectionNumber);
						currentSection = ontologyContainer.createSection(currentParagraph, currentSectionNumber, currentParagraphContent);
						paragraphList.add(new Article(currentParagraph, currentParagraphNumber, currentSection, currentSectionNumber, currentParagraphContent));
					}
							// if we read the footnote, we have to store the last section and 
							// set the state to undefiened, now the parsing is done ...
				} else if (isFooter(node, currentParagraphContent)) {	
					currentSection = ontologyContainer.createSection(currentParagraph, String.valueOf(currentSectionNumber), currentParagraphContent);
					paragraphList.add(new Article(currentParagraph, currentParagraphNumber, currentSection, currentSectionNumber, currentParagraphContent));
					currentParagraphContent = "";
				}
					
			} catch (IndexOutOfBoundsException e) { /* do nothing, here is no paragraph .. */ }
		}
		
			
			// search for references in the given law
		findReferences(ontologyContainer);
		
		return true;
	}
	
	/**
	 * searches for the references between the articles.
	 * @param ontologyContainer the ontology helper
	 */
	private void findReferences(final RegulatoryOntologyHelper ontologyContainer) {
		ArticleFinder finder = new ArticleFinder();
		ArrayList<LawCrossConnection> list = null;
			// iterate over the whole list of paragraphs
		for (Article article : paragraphList) {
				// and search for connections to other paragraphs
			list = finder.getLawCrossConnections(article.content);
				// for each connection we have to test, if it references another code of law
			for (LawCrossConnection connection : list) {
					// if it references another code of law, it will be ignored
				if (connection.getCodeOfLaw().equals("")) {
						// if it references the same code of law, we have to find the paragraph it belongs to
					for (Article article2 : paragraphList) {
						if (connection.getParagraph().equals("")) {
							if (article2.paragraphNumber.equals(connection.getArticle())) {
								// if we found the paragraph, we have to create a new reference in the ontology
								ontologyContainer.createRuleReference(article.section, article2.paragraph);
//							outputMessage(IMPORTER_NAME,"Found reference from " + article.section + " to " + article2.paragraph);
							break;
							}
						} else {
							if (article2.paragraphNumber.equals(connection.getArticle()) 
							    && article2.sectionNumber.equals(connection.getParagraph())) {
								ontologyContainer.createRuleReference(article.section, article2.section);
//								outputMessage(IMPORTER_NAME,"Found reference from " + article.section + " to " + 
//													article2.section);
								break;
							}
						}
						
//						
					}
				}
			}
			list.clear();
		}
		paragraphList.clear();
	}
	
	/**
	 * extracts the Integer values out of an array. 
	 * It can be chosen if a suffix (e.g. 16a) will be accepted.
	 * @param paragraphNumber the string to be searched in
	 * @param allowParagraphSuffix if the suffic will be accepted (e.g. "36c")
	 * @return the Integer value of the array
	 */
	public static String extractParagraph(final String paragraphNumber, final boolean allowParagraphSuffix) {
		String pattern = "";
		if (allowParagraphSuffix) {
			pattern = "\\d{1,}([a-z]|)";
		} else {
			pattern = "\\d{1,}";
		}
		Matcher m = Pattern.compile(pattern).matcher(paragraphNumber);
		if (m.find()) {
			return m.group();
		}
		return "";
	}

	/**
	 * extracts the number out of a string. Enumerations are separated in a summary of all relevant numbers 
	 * (eg. 14 bis 17 will return: 14, 15, 16, 17 and 16a bis 17c will return: 16a, 17, 17a, 17b, 17c).
	 * @param someNumbers the string of numbers
	 * @return a list of all included numbers
	 */
	public static List<String> getParagraphNumbers(final String someNumbers) {
		//System.out.println("[JurisFileParser] Input: " + someNumbers);
		extractedParagraphs = new ArrayList<String>();
		String[] numbers = {""};
		// the alphabet to find the bounds
		char[] alphabet = 	{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
							 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};
		numbers = someNumbers.split("(, )");
		if (numbers.length > 0) {
			for (String number : numbers) {
					/* 
					 * if the String contains "bis", the whole number of paragraphs
					 * between the upper and lower bound should be listed in the array
					 */
				if (number.matches(".*bis.*")) {
					String[] rangeLimits = number.split(" bis ");
					int upperInt = 0;
					int lowerInt = 0;
					try {
// FIXME: true leads to "26c" being returned; this cannot (?) be parsed by parseInt
// therefore it exists a NumberFormatException which handles this case
						lowerInt = Integer.parseInt(extractParagraph(rangeLimits[0], true));
						upperInt = Integer.parseInt(extractParagraph(rangeLimits[1], true));
							/* if the upper and lower bound only contain integers */
						for (int c = lowerInt; c <= upperInt; c++) {
							extractedParagraphs.add(String.valueOf(c));
						}
					} catch (NumberFormatException n) {
// if parseInt threw Exception (i.e. "26c")
						try {							
							if (Integer.parseInt(extractParagraph(rangeLimits[0], false)) 
								< Integer.parseInt(extractParagraph(rangeLimits[1], false))) {
								getIntegersWithHigherRange(rangeLimits, alphabet);
							} else {
								getIntegersWithSameRange(rangeLimits, alphabet, lowerInt);
							}
						} catch (IndexOutOfBoundsException i) {
							if (lowerInt != 0) {
								extractedParagraphs.add(String.valueOf(lowerInt));
							}
						} catch (NumberFormatException n1) {
							System.err.println("[JurisFileParser] " + rangeLimits[0] + " " + rangeLimits[1]);
						}
					} catch (IndexOutOfBoundsException i) { 
						if (lowerInt != 0) {
							extractedParagraphs.add(String.valueOf(lowerInt));
						}
					}
				} else if (number.contains(" und ")) { 
					extractedParagraphs.add(number.substring(0, number.indexOf(" und")));
					extractedParagraphs.add(number.substring(number.indexOf("und ") + 4));
				} else {
					if (number.matches(".*\\d{1,3}.*")) {
						extractedParagraphs.add(number);
					}
				}
			}
		} else { extractedParagraphs.add(someNumbers); }
		/*for (String number : extractedParagraphs) { 
			System.out.println("[JurisFileParser] Output: " + number); 
		}*/
	return extractedParagraphs;
	}
	
	/**
	 * extracts the integer values from a given string.
	 * @param rangeLimits the upper and lower bounds
	 * @param alphabet the alphabet
	 * @return the integer
	 */
	public static final int getIntegersWithHigherRange(final String[] rangeLimits, final char[] alphabet) {
		/* 
		 * if the upper or lower bound don't contain only integers
		 * and have got a higher range
		 */
			// only for tests
		int sizeBeforeInput = extractedParagraphs.size();
		String upperString = rangeLimits[1];
		char upperAlphabet = upperString.toCharArray()[upperString.length() - 1];
		System.out.println(Arrays.toString(rangeLimits) + " " + upperString);
		int lowerInt = 0; int upperInt = 0; int upperLetterBound = 0;
		try {
			lowerInt = Integer.parseInt(extractParagraph(rangeLimits[0], true));
			extractedParagraphs.add(String.valueOf(lowerInt));
		} catch (NumberFormatException n) { 
 			char lowerAlphabet = rangeLimits[0].toCharArray()[rangeLimits[0].length() - 1];
			lowerInt = Integer.parseInt(extractParagraph(rangeLimits[0], false));
			extractedParagraphs.add(String.valueOf(lowerInt) + String.valueOf(lowerAlphabet));
		}
		upperInt = Integer.parseInt(extractParagraph(rangeLimits[1], false));
		for (int c = lowerInt + 1; c <= upperInt; c++) {
			extractedParagraphs.add(String.valueOf(c));
		}
		for (int c = 0; c < alphabet.length; c++) {
				// search for the matching upper bound
			if (alphabet[c] == upperAlphabet) { upperLetterBound = c; }
		}
		for (int i = 0; i <= upperLetterBound; i++) {
			extractedParagraphs.add(String.valueOf(upperInt) + alphabet[i]);
		}
		return extractedParagraphs.size() - sizeBeforeInput; 
	}
	
	
	/**
	 * returns a list of paragraphs, where the input only contains integer values with the same range.
	 * @param alphabet the alphabet
	 * @param lowerInt to test if the lower range contains letters
	 * @param rangeLimits the boundaries of the range
	 * @return the integer
	 */
	public static final int getIntegersWithSameRange(final String[] rangeLimits, final char[] alphabet, final int lowerInt) {
			// only for testing
		int sizeBeforeInput = extractedParagraphs.size();
		int lowerBound = 0; int upperLetterBound = 0;
		if (lowerInt != 0) {
			extractedParagraphs.add(String.valueOf(lowerInt));
			char upperAlphabet = rangeLimits[1].toCharArray()[rangeLimits[1].length() - 1];
			for (int c = 0; c < alphabet.length; c++) {
					// search for the matching upper bound
				if (alphabet[c] == upperAlphabet) { upperLetterBound = c; }
			}
				/* if both bounds don't contain only integers */
		} else {
			char upperAlphabet =  rangeLimits[1].toCharArray()[rangeLimits[1].length() - 1];
			char lowerAlphabet =  rangeLimits[0].toCharArray()[rangeLimits[0].length() - 1];
			for (int c = 0; c < alphabet.length; c++) {
					// search for both matching bounds
				if (alphabet[c] == upperAlphabet) { upperLetterBound = c; }
				if (alphabet[c] == lowerAlphabet) { lowerBound = c; }
			}
		}	// add all resulting articles to the paragraph list
		for (int c = lowerBound; c <= upperLetterBound; c++) { 
			extractedParagraphs.add(extractParagraph(rangeLimits[0], false) + alphabet[c]); 
		}
		return extractedParagraphs.size() - sizeBeforeInput;
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
	
	private void outputMessage(final String importerName, final String message) {
		this.outputMessage(importerName, message, 0);
	}
}

	