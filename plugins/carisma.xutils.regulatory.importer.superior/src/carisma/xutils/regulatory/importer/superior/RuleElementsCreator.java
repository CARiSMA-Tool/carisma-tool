package carisma.xutils.regulatory.importer.superior;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.TreeSet;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;

import carisma.check.processanalysis.texttools.wortschatz.WSNormalizer;
import carisma.check.processanalysis.texttools.wortschatz.WrongElementException;
//import carisma.processanalysis.textmodel.ScoredString;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.textmodel.WordKind;
import carisma.processanalysis.texttools.KeywordCollection;
import carisma.processanalysis.texttools.KeywordSet;
import carisma.regulatory.ontology.utils.IndividualIDGenerator;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.importer.superior.XMLClasses.Entry;
import carisma.xutils.regulatory.importer.superior.XMLClasses.iEntry;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntry;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.XStreamException;
import com.thoughtworks.xstream.io.xml.DomDriver;
import com.thoughtworks.xstream.mapper.CannotResolveClassException;

import com.csvreader.CsvReader;
import com.csvreader.CsvWriter;;

public class RuleElementsCreator {
		/** file to load the xml-file */
	private String fileName;
		/** the normalizer */
	private WSNormalizer normalizer = null;
		/** Name for the importer */
	public static final String IMPORTER_NAME = "RuleElementsCreator";
		/** Default path to source folder */
	public static final String DEFAULT_SOURCE_FOLDER = "resources" + File.separator;
		/** reference to the logView output (GUI) */
	private LogViewEntrySet logInput = null;
	
	private String sourcePath;
	
		// to test the expander	// TODO remove
//	private ArrayList<String> testList = new ArrayList<String>();
	
	private RegulatoryOntologyHelper roh;
	private LinkedList<ElementEntry> elements;

	/**
	 * creates a new RuleElementCreator
	 * therefore it loads the xml-file with the elementEntries and stores them into a list
	 * 
	 * @param roh the Regulatory
	 * OntoloyHelper to store into an ontology
	 */
	public RuleElementsCreator(RegulatoryOntologyHelper roh, String sourcePath, LogViewEntrySet logInput) {
		this.logInput = logInput;
		
		if (sourcePath != null && sourcePath != "") {
			this.sourcePath = sourcePath;
		} else {
			this.sourcePath = DEFAULT_SOURCE_FOLDER;
		}
		this.fileName = this.sourcePath + "ElementEntry.xml";
		outputMessage(IMPORTER_NAME, "Filename: " + this.fileName, 3);
		
		try {
			this.normalizer = new WSNormalizer(WSNormalizer.NormalizerMode.USE_CACHE_AND_NET);
		} catch (WrongElementException e1) {
			e1.printStackTrace();
			outputMessage("ERROR", e1.getMessage(), 1);
		}
		
		this.roh = roh;
		this.elements = new LinkedList<RuleElementsCreator.ElementEntry>();
		
		XStream xs = new XStream(new DomDriver());	
			// to find the class 
		xs.setClassLoader(this.getClass().getClassLoader());
		xs.processAnnotations(XMLClasses.class);
		FileInputStream inputStream;
		try {
				// get the file
			File file = new File(fileName);
			if (!file.exists()) {
//				TODO KR: Elegantere Loesung? Eventuell Excepetion?
				outputMessage("ERROR", "ElementEntry.xml in /resources not present!", 1);
				return;
			}
			inputStream = new FileInputStream(file);
			Entry entries = (Entry) xs.fromXML(inputStream);
			for(iEntry entry : entries.getEntrys()){ 	// iterate over the entrys in the document
				for(String[] list : entry.getList()){	// iterate over the list from the entry
						// add a new ElementEntry 
					this.elements.add(new ElementEntry(entry.getName(), list[0], list[1]));
					outputMessage(IMPORTER_NAME, "New Entry: " + entry.getName() + " Entry_Values " + list[0] + " " + list[1], 6);
				}
			}
		} catch (FileNotFoundException e) {
			outputMessage("ERROR", "FileNotFoundException for " + this.fileName, 1);
			e.printStackTrace();
		} catch (CannotResolveClassException c) {
			outputMessage("ERROR", "The class for the xml-element could not be found!", 1);
			outputMessage("ERROR", c.getLocalizedMessage(), 1);
			c.printStackTrace();
		} catch (XStreamException x){
			outputMessage("ERROR", "If the Error Message relies on an ObjectAccessException, then check the java version. Required Version Java 6!!\n", 1);
			outputMessage("ERROR", x.getMessage(), 1);
			x.printStackTrace();
		}
	}
// (DW) Never used locally, so commented out to reflect this	
//	/**
//	 * for a given individual, there will be added a new reference to a matching element entry
//	 * 
//	 * @param source the individual
//	 * @param className the class name of the element entry
//	 * @param elName the name of the element
//	 * @param elType the type of the element
//	 * @return a new individual
//	 */
//	private OWLNamedIndividual createAppropriateElement(OWLNamedIndividual source, String className, String elName, String elType){
//		OWLNamedIndividual newElement;
//		System.out.println("Individual " + source.toString() + " Klassenname " + className + " elName " + elName + " elType " +elType);
//		if(className.equals(RegulatoryOntologyHelper.CLS_ACTIVITY)){
////			newElement = roh.createREActivity(elName, elType, elName, "irgendein informativer Text, warum diese Property Sinn macht", "", "");
//			newElement = roh.createREActivity(elName);
//			roh.createREReference(source, newElement, RegulatoryOntologyHelper.REL_RULE_CONTAINEDACTIVITIES);
//		}
//		else if(className.equals(RegulatoryOntologyHelper.CLS_ARTIFACT)){
////			newElement = roh.createREArtifact(elName, elType, elName, "irgendein informativer Text, warum diese Property Sinn macht", "", "");
//			newElement = roh.createREArtifact(elName);
//			roh.createREReference(source, newElement, RegulatoryOntologyHelper.REL_RULE_CONTAINEDARTIFACTS);
//		}
//		else if(className.equals(RegulatoryOntologyHelper.CLS_PROCESS)){
////			newElement = roh.createREProcess(elName, elType, elName, "irgendein informativer Text, warum diese Property Sinn macht", "", "");
//			newElement = roh.createREProcess(elName);
//			roh.createREReference(source, newElement, RegulatoryOntologyHelper.REL_RULE_CONTAINEDPROCESSES);
//		}
//		else if(className.equals(RegulatoryOntologyHelper.CLS_PROPERTY)){
////			newElement = roh.createREProperty(elName, elType, elName, "irgendein informativer Text, warum diese Property Sinn macht", "", "");
//			newElement = roh.createREProperty(elName);
//			roh.createREReference(source, newElement, RegulatoryOntologyHelper.REL_RULE_CONTAINEDPROPERTIES);
//		}
//		else if(className.equals(RegulatoryOntologyHelper.CLS_ROLE)){
////			newElement = roh.createRERole(elName, elType, elName, "irgendein informativer Text, warum diese Property Sinn macht", "", "");
//			newElement = roh.createRERole(elName);
//			roh.createREReference(source, newElement, RegulatoryOntologyHelper.REL_RULE_CONTAINEDROLES);
//		}
//		else{
//			outputMessage("ERROR", "Error: Unsupported Element Type", 1);
//			newElement = null;
//		}
//		return newElement;
//	}
	
	/**
	 * tries to get the baseform of a given word. It uses a cache with stored words and the Internet
	 * for not yet known words to find a suitable baseform for the given word. If no word has matched,
	 * an empty string will be returned
	 * 
	 * @param word the word to search a baseform for
	 * @return the baseform if there exists one
	 */
	public String getBaseForm(String word) {
		Word newWord = new Word(word);
		
//		if(normalizer.isMisspelled(word)){
//			ArrayList<String> correctSpelled = normalizer.getCorrectSpelledCandidates(word);
//			if(!correctSpelled.isEmpty()){
//				word = new Word(correctSpelled.get(0));		// TODO check the other words 
//			} else{
//				word = null;
//			}
//		}
		if (newWord != null && normalizer != null) {
			try {
				outputMessage(IMPORTER_NAME, newWord.toString(), 9);
				return normalizer.getBaseform(newWord).toString();
			} catch(NullPointerException n){
				outputMessage("ERROR", "NullPointer ... using a cache with USE_ONLY_CACHE??", 1);
				return "";
			}catch (Exception e){		// if the connection to 'wortschatz' is closed
				outputMessage("ERROR", "Communication Link Failure! Word " + newWord.toString(), 1);
				e.printStackTrace();
				return "";
			}
		} else { return ""; /* return nothing, word is misspelled and no alternative is found */}
		
	}
	
	/**
	 * checks for each individual - which has just been stored within the RegulatoryOntologyHelper - 
	 * if it matches a given property. For every word in the annotation of the individual, it will 
	 * be checked if it matches a set of words, and if so, a new appropriate element will be created
	 * 
	 * @throws NoSuchPropertyException
	 */
	public void run() throws NoSuchPropertyException{
		// ein wenig deplaziert:
		KeywordCollection keywords = this.readKeywordsCSV();
		
		HashMap<ElementEntry, OWLNamedIndividual> representatives = this.createElementIndividuals(); 
		Set<OWLNamedIndividual> individuals = roh.getGoh().getIndividuals(RegulatoryOntologyHelper.CLS_RULE, false);
		for (OWLNamedIndividual curInd : individuals) { // Every iteration creates those properties originating from curInd			
//			System.out.println(curInd); // TODO remove
			
//			System.out.println(roh.getOWLClass(curInd.get
			
			String text;
			String title;/* = "";
			if(!curInd.toString().contains("MARiskVA")){
				title = roh.getGoh().getStringAnnotation(curInd, RegulatoryOntologyHelper.PROP_RULE_TITLE);
			} else{
				// TODO aktuell werden keine Marisk Eintraege einbezogen. evtl Annotations bei marisk angleichen?
				
//				title = roh.getGoh().getStringAnnotation(curInd, RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER);
			}
			*/
			
			/* title nach diesem Block:
			 * BSIRule.*: Überschrift des Katalog-Eintrags
			 * LawRule.Law: ?
			 * LawRule.Paragraph: leerer String oder Titel des Abschnitts
			 * LawRule.Section: Übersprungen
			 * MARiskRule.MARiskClause: Titel der (Unter?)Abschnitte?
			 * MARiskRule.ContentEntry: Übersprungen 
			 */
			try{
			title = roh.getGoh().getStringAnnotation(curInd, RegulatoryOntologyHelper.PROP_RULE_TITLE);			
			text = roh.getGoh().getStringAnnotation(curInd, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT);
			roh.setKeywords(curInd, keywords.getWithDefaultEmpty(curInd.getIRI().toString()).asCSVLineNoID());
			}
			catch(NoSuchPropertyException e){
				// nicht schlimm, Individuum wird ignoriert.
				continue;
			}
			
			if(!title.equals("")){
				String[] titleWords = title.split("\\s");
				
				// titleStringSetTemp enthält die vorkommenden Strings, ohne Positionsangaben
				TreeSet<String> titleStringSetTemp = new TreeSet<String>(Arrays.asList(titleWords));

				// wird in der folgenden Schleife gefüllt: titleWordsSetNew enthält die vorkommenden Worte mit Positionsangaben
				LinkedList<PlacedTitleWord> titleWordsSetNew = new LinkedList<PlacedTitleWord>();
				
				for(String string : titleStringSetTemp){
					outputMessage(IMPORTER_NAME, "String: " + string, 8);
						// to check if the word starts with numbers
						// Wortschatz does not deal with those words
					if(!string.matches("[0-9]{1,3}.*")){ //TODO wenn abfangen, dann nicht hier
						//TODO: wie kommt WS inzwischen mit Zahlen klar? -> nicht gut
						
								// search for every word in the titleset the baseform
						String curStringBaseform = getBaseForm(string);
						if(curStringBaseform.length() == 0)
							curStringBaseform = string;
						
						int startIdx = text.indexOf(string);
						int endIdx = startIdx + string.length() - 1;
						
						if(startIdx == -1) // das wort kommt im Titel vor, aber nicht im Text
							continue;
						
						//workaround: startIndex muss um 1 niedriger sein als man erwarten würde:
						startIdx--;
						
						PlacedTitleWord newTitleWord = new PlacedTitleWord(string, curStringBaseform, startIdx, endIdx);
						titleWordsSetNew.add(newTitleWord);						
						
								// to fill the words for the expanderTest
//							testList.add(string); // TODO remove
					}
				}

				/* braucht man nicht
				// for each found word, test if it is in the titleset 
					// and if not, fill it in the list
				for(String string : titleWordsSetNew){
					if(!titleWordsSet.contains(string)){
						if(!string.equals("")){
							titleWordsSet.add(string);
							// to fill the words for the expanderTest
//							testList.add(string); // TODO remove
						}
					}
				} */
				//stattdessen:
//				titleWordsSetNew.remove(new String(""));
				titleStringSetTemp = null; //zur Klarstellung
				
				// test for all elements if the caption is included in the titlewordset
				for (ElementEntry curElEntry : this.elements) {
					for (PlacedTitleWord curTitleWord : titleWordsSetNew) {
						if (curTitleWord.getBaseformedWord().equals(curElEntry.caption_de)) {
							outputMessage(IMPORTER_NAME, "Current Entry Name: " + curElEntry.className, 8);
							outputMessage(IMPORTER_NAME,"Current Entry Group: " + curElEntry.className, 8);
							outputMessage(IMPORTER_NAME, curElEntry.caption_de + " in " + curInd, 7);

							// TODO TH: Ändern: kein neues Individuum mehr,
							// stattdessen Property auf Repräsentant
							// Welche Annotations an Property?
							// String indName=
							// curElEntry.className.toLowerCase()+"_"+curInd.getIRI().getFragment();
							// this.createAppropriateElement(curInd,
							// curElEntry.classGroup, indName,
							// curElEntry.className);

							// neu:
//							roh.createREReference(curInd, representatives.get(curElEntry), getRelationForREType(curElEntry.classGroup), "?1", "?2", "?3");
							
							// noch neuer:
							roh.createContainedRuleElementsRelation(curInd, representatives.get(curElEntry),  curTitleWord.getOriginalWord(), curTitleWord.getStartIndex(), curTitleWord.getEndIndex());
							// am neuesten und noch nicht freigegeben
//							roh.createRuleElementRepresentation(curInd, representatives.get(curElEntry), curTitleWord.getOriginalWord(), curTitleWord.getStartIndex(), curTitleWord.getEndIndex());
							
							// roh.setRuleElementProperties(newProperty, "?" ,
							// "?", "?", "?", "?");
							// roh.getGoh().createAnnotation(newProperty,
							// RegulatoryOntologyHelper.PROP_RULEELEMENT_TYPE,
							// "?");
						}

					}
				}

				//			break; //debug
			}
		}
//		testExpander();
		normalizer.storeCache();
		this.exportKeywordCSV(); // debug
		outputMessage(IMPORTER_NAME, "Done ...", 3);
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
		
	public HashMap<ElementEntry, OWLNamedIndividual> createElementIndividuals()  throws NoSuchPropertyException {
		HashMap<ElementEntry, OWLNamedIndividual> ret = new HashMap<RuleElementsCreator.ElementEntry, OWLNamedIndividual>();
		for (ElementEntry curEntry : this.elements) {
			outputMessage(IMPORTER_NAME, curEntry.classGroup + "/" + curEntry.className
					+ "/" + curEntry.caption_de, 7);
			//String owlClass = RegulatoryOntologyHelper.CLS_RULEELEMENT + "/" + curEntry.classGroup+"/"+curEntry.className;
			String owlClass = curEntry.classGroup;
			outputMessage(IMPORTER_NAME, "Klasse: " + owlClass, 7);
					
			OWLNamedIndividual newRuleElement = 
			this.roh.createRE(IndividualIDGenerator.generateRuleElementID(curEntry.className,owlClass), owlClass, curEntry.caption_de);
			ret.put(curEntry, newRuleElement);
		}
		return ret;
	}
//TODO: Find another way to determine the rule-ruleElement relation or use the general relation.
	/**
	 * Temporary method for returning the correct ontology relation from 
	 * rules to the corresponding rule element. 
	 * @param ruleElementType
	 * @return
	 */
	private String getRelationForREType(final String ruleElementType) {
		if (ruleElementType.equals("Activity")) {
			return RegulatoryOntologyHelper.REL_RULE_CONTAINEDACTIVITIES;
		} else if (ruleElementType.equals("Artifact")) {
			return RegulatoryOntologyHelper.REL_RULE_CONTAINEDARTIFACTS;			
		} else if (ruleElementType.equals("Process")) {
			return RegulatoryOntologyHelper.REL_RULE_CONTAINEDPROCESSES;			
		} else if (ruleElementType.equals("Property")) {
			return RegulatoryOntologyHelper.REL_RULE_CONTAINEDPROPERTIES;			
		} else if (ruleElementType.equals("Role")) {
			return RegulatoryOntologyHelper.REL_RULE_CONTAINEDROLES;			
		} else {
			return "";
		}
	}
	/**
	 * represents an object of the type elementEntry
	 * this is used to store all entries in a list
	 */
	public class ElementEntry{
		public String className;
		public String classGroup;
		public String caption_de;

		public ElementEntry(String classGroup, String className, String caption_de) {
			super();
			this.classGroup = classGroup;
			this.className = className;
			this.caption_de = caption_de;
			
		}
	}
	
	// nur Hilfsfunktion
	private void exportKeywordCSV(){
		this.roh.getAllKeywords().writeToCSV(this.sourcePath + "keywordsOut.csv");
	}
	
	private KeywordCollection readKeywordsCSV(){
		KeywordCollection ret = new KeywordCollection();
		
		try {
//			CsvReader reader = new CsvReader(this.sourcePath + "keywordsIn.csv");
			CsvReader reader=new CsvReader(new InputStreamReader(new FileInputStream(this.sourcePath + "keywordsIn.csv"), "UTF-8"));
			while(reader.readRecord()){
				String[] values = reader.getValues();
				LinkedList<String> stringList = new LinkedList<String>(Arrays.asList(values)); // geht das so?
				String id = stringList.removeFirst();
//				KeywordSet newSet = new KeywordSet(id, stringList);
				KeywordSet newSet = new KeywordSet();
				for(String curString : stringList){
					// TODO TH Hier evtl. anderen Score? -> wahrsch. hoeher
					newSet.add(new Word(curString, WordKind.PATTERNKEYWORD)); //, 200));
					// TODO: score geht verloren!
				}
				ret.put(id, newSet);
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return ret;
	}
	
		// only to test the expander // TODO remove
//	private void testExpander(){
//		long counter = 0;
//		try {
//			WSExpander expander = new WSExpander(WSExpander.USE_CACHE_AND_NET);
//			for(String key : testList){
//				String test = "Word: " + key + " Extensions: ";
//				ArrayList<WordExtension> extensionList = expander.getWordExtensions(new Word(key), false);
//				if(extensionList != null){
//					counter += extensionList.size();
//					for(WordExtension extension : extensionList){
//						test += extension.getContent() + ", ";
//					}
//					System.out.println(test);
//					test = "";
//				}
//			}
//			expander.storeCache();
//			System.out.println("" + counter + " extensions found!");
//		} catch (WrongElementException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		
//	}

}
