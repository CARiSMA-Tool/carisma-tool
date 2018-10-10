package carisma.xutils.regulatory.importer.juris;
//package carisma.xutils.regulatory.importer.juris;
//
//
//import java.io.File;
//
//import org.semanticweb.owlapi.model.OWLNamedIndividual;
//import org.xml.sax.Attributes;
//import org.xml.sax.ContentHandler;
//import org.xml.sax.Locator;
//import org.xml.sax.SAXException;
//
//import carisma.regulatory.ontology.utils.GenericOntologyHelper;
//import carisma.regulatory.ontology.utils.OntologyHelperException;
//import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
//
///**
// *
// * @author wessel
// *
// */
//
//public class LawContentHandler implements ContentHandler {
//	
//		/** string to concat the complete description of a section */
//	private String sectionTxt = "";
//		/** string to concat the complete name of a paragraph */
//	private String titleTxt = "";
//		/** detect if we are in an section */
//	private boolean isSection = false;
//		/** detect if there is a footnote */
//	private boolean isFootNote = false;
//		/** detect if we are in the title of a paragraph */
//	private boolean isTitle = false;
//		/** detect if there is an attachment for a paragraph */
//	private boolean isAttachment = false;
//		/** flie to store the ontology in */
//	private String file = "";
//		/** counts the inner tables */
//	private int innerTable = 0;
//
//	private enum State {
//		LawName,
//		Content,
//		Number,
//		Table,
//		Title,
//		Text,
//		undefined
//	};
//	
//	private State currentState = State.undefined;		
//	private GenericOntologyHelper goh = new GenericOntologyHelper();
//	private RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh); 
//	
//	private OWLNamedIndividual currentLaw = null;       //e.g. BDSG
//	private OWLNamedIndividual currentParagraph = null; //e.g. 1 + Name
//	private OWLNamedIndividual currentSection = null;   //e.g. (1) <Text of the section within the paragraph>
//	
//	private String currentNumber;
//	
//	/**
//	 * Creates a new instance of the content handler
//	 * and initializes a new ontology to write to. 
//	 */
//	public LawContentHandler(String file) {
//		this.file = file;
//		goh.createNewOntology(RegulatoryOntologyHelper.ONTOLOGY_IRI_BASE);
//		RegulatoryOntologyHelper.initializeOntology(goh);
//	}
//
//	@Override
//	public void startElement(final String uri, final String localName, final String qName, final Attributes atts) throws SAXException {
//		
//		//Determine current position in file and assign the correct state
//		String classVal = atts.getValue("class");
//		
//		if (qName.equals("title")) {
//			currentState = State.LawName;
//			System.out.println("Set current state to LawName");
//		} else if (qName.equals("span")) {
//			System.out.println("Found span tag, retrieving type ...");
//			//spans may contain either the number or the title of the
//			//paragraph, so we must retrieve the type of this span.
//			if ("jnentitel".equals(classVal)) {
//				currentState = State.Title;
//				isTitle = true;
//				System.out.println("Set current state to Title");
//			} else if ("jnenbez".equals(classVal)) {
//				currentState = State.Number;
//				System.out.println("Set current state to Number");
//			}
//		} else if (qName.equals("div")) {
//			System.out.println("Found div tag, retrieving type ...");
//			//If it is a div tag, we must ensure it contains a section
//			//of a paragraph, i.e. check if we are within a paragraph
//			if ("jurAbsatz".equals(classVal)) {
//				currentState = State.Text;
//				isSection = true;
//				System.out.println("Set current state to Text");
//			} else if("jnheader".equals(classVal)){
//				currentState = State.Content;
//			} else if("jnfussnote".equals(classVal)){
//					// if there is a footnote, this content will be ignored
//				currentState = State.Content;
//				isFootNote = true;
//			} else if("jnnorm".equals(classVal)){
//					// to determine when a footnote ends
//				isFootNote = false;
//				sectionTxt = "";
//			}
//		} else if (qName.equals("dl")){	
//			System.out.println("Found table ...");
//			if(currentState == State.Table){
//				innerTable++;
//			}
//			currentState = State.Table;
//		} else if (currentState == State.Table && qName.equals("dt")){
//				// if we find a new table row, we create a newline for every row
//			sectionTxt += "\n";
//		}
//	}
//
//	@Override
//	public void characters(final char[] ch, final int start, final int length) throws SAXException {
//		//According to the current state we are in
//		//different parts of a lawEntry must be filled.
//		
//		String chars = new String(ch).substring(start, start + length).trim();
//		
//		if (chars.length() > 0) {
//			switch(currentState)
//			{
//			case LawName:
//					String[] tokenized = chars.split("\\-");
//					String shortName = "";
//					String longName = "";
//
//					if (tokenized.length == 1)
//						shortName = longName = tokenized[0].trim();
//					if( tokenized.length > 1 ) {
//						shortName = tokenized[0].trim();
//						longName = tokenized[1].trim();
//					}
//					System.out.println("Creating new law: " + longName + " (short name: " + shortName + ")");
//					currentLaw = roh.createLaw(shortName, longName);
//				break;
//			case Number:
//				if (chars.matches("[0-9]+[a-z]{0,1}")) {
//					currentNumber = chars;
//					System.out.println("Added " + chars + " to currentNumber");
//				} else {
//					if(chars.equals("Anlage")){
//							// if there is an attachment, it will be ignored
//						isAttachment = true;
//						System.out.println("Found attachment.... will be ignored!");
//					} else {
//						System.out.println("Not added: " + chars);
//					}
//				}
//				break;
//			case Title:
//					// find the umlauts in the given char string
//				titleTxt = concatStrings(titleTxt, chars);
////				System.out.println(titleTxt);
//				break;
//			case Text:
//					// find the umlauts in the given char string
//				sectionTxt = concatStrings(sectionTxt, chars);
////				System.out.println(sectionTxt);
//				break;
//			case Content:
//				break;
//			case Table:
//				sectionTxt = concatStrings(sectionTxt, chars);
//				break;
//			default:
//				//Ignore all other states (i.e. undefined)
//				break;
//			}
//		} 
//	}
//	
//	@Override
//	public void endElement(final String uri, final String localName, final String qName) throws SAXException {
//		if(qName.equals("head")){
//				// to seperate the head from the content of the law
//			currentState = State.Content;
//			System.out.println("Set current State to content");
//		} else if(!isFootNote && isSection && currentState == State.Text && currentNumber != null && !isAttachment){
//				// a new section should only be created, when it does not belong to a footnote and 
//				// belongs to a section (e.g. a link is ignored)
//			System.out.println("Creating new section: " + currentNumber + " (text: " + sectionTxt + ")");
//			currentSection = roh.createSection(currentParagraph, currentNumber, sectionTxt);
//			sectionTxt = "";
//			isSection = false;
//		} else if(isTitle && currentState == State.Title && currentNumber != null && !isAttachment){
//				// a new title should only be created, when the whole name was read
//			System.out.println("Creating new paragraph: " + currentNumber + " (title: " + titleTxt + ")");
//			currentParagraph = roh.createParagraph(currentLaw, currentNumber, titleTxt);
//			isTitle = false;
//			titleTxt = "";
//		} else if(qName.equals("dl")){
//				// if we leave the table, we have to set the state to text
//			if(innerTable > 0){
//				innerTable--;
//			} else {
//				currentState = State.Text;
//				sectionTxt += "\n";
//			}
//			
//		}
//	}
//
//	@Override
//	public void endDocument() throws SAXException {
//		try{
//			goh.saveOntologyToFile(new File(file));
//			System.out.println("Ontology sucessfully stored to " + file);
//		} catch (OntologyHelperException e){
//			System.err.println("Failed to store the ontology ...");
//		} catch (NullPointerException e) {
//			System.err.println("Error occured while saving to file (NullPointerException)");
//		}
//	}
//
//	@Override
//	public void endPrefixMapping(final String prefix) throws SAXException {
//	}
//
//	@Override
//	public void ignorableWhitespace(final char[] ch, final int start, final int length) throws SAXException {
//	}
//
//	@Override
//	public void processingInstruction(final String target, final String data) throws SAXException {
//	}
//
//	@Override
//	public void setDocumentLocator(final Locator locator) {
//	}
//
//	@Override
//	public void skippedEntity(final String name) throws SAXException {
//	}
//
//	@Override
//	public void startDocument() throws SAXException {
//	}
//
//	@Override
//	public void startPrefixMapping(final String prefix, final String uri) throws SAXException {
//	}
//	
// 
//	/**
//	 * concat the string which is split around a umlaut
//	 * 
//	 * @param str the string to concatenated
//	 * @param chars the chars to add
//	 */
//	private String concatStrings(String str, String chars){
//		if(chars.matches(".*(�|�|�|�|�|�|�)+.*")){
//			if(str.length() > 2){
//					// if the last letter before an umlaut is a number, then there must be a space
//				if(str.substring(str.length()-1).matches("[0-9]") || chars.matches("(�|�|�)")){
//					str += " " + chars;
//				} else {
//					str += chars;
//				}
//			} else{
//				str += chars;
//			}
//		} else {
//				// if the last letter of the String is an umlaut, then must not be a space
//			if(str.length() > 2){
//				if(str.substring(str.length()-2).matches(".*(�|�|�|�|�|�|�)+.*")){
//					str += chars; 
//				} else if(str.substring(str.length()-1).equals("�")){
//					str += chars;
//				} else {
//					str += " " + chars; 
//				}
//			} else {
//				str += chars;
//			}
//		}
//		return str;
//	}
//}
