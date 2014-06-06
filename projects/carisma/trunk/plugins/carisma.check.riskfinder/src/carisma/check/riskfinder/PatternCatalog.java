package carisma.check.riskfinder;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.processanalysis.textmodel.Text;
import carisma.processanalysis.textmodel.TextKind;
import carisma.processanalysis.texttools.KeywordSet;
import carisma.regulatory.ontology.utils.GenericOntologyHelper;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
//import de.fraunhofer.isst.wbmp.io.exporter.XMLDocumentExporter;
//import de.fraunhofer.isst.wbmp.model.*;

/**
 * Eine Menge von Pattern fuer den Riskfinder.
 * Evtl. ist hier eine weitere Vererbungsebene sinnvoll.
 * @author thumberg
 *
 */
public class PatternCatalog extends HashSet<Pattern> {
	
	/**
	 * Enumeration for the BSI-Elements in an ontology.
	 * @author Klaus Rudack
	 *
	 */
	public enum BSIOnto {
		/**
		 * for all BSI-Elements.
		 */
//		TODO KR: Eventuell hier im JavaDoc Elements durch was anderes ersetzten, da es BSIElemente gibt.
		ALL,
		
		/**
		 * for BSIElements.
		 */
		ELEMENT,
		
		/**
		 * for BSIMeasures.
		 */
		MEASURE,
		
		/**
		 * for BSIThreats.
		 */
		THREAT,
		
		/**
		 * for BSIElements and BSIMeasures.
		 */
		ELEMENTMEASURE,
		
		/**
		 * for BSIElements and BSIThreats.
		 */
		ELEMENTTHREAT,
		
		/**
		 * for BSIMeasures and BSIThreats.
		 */
		MEASURETHREAT,
		
		/**
		 * for no BSI-Element.
		 */
		NONE
	}

	/**
	 * Enumeration for the  MARisk-Elements in an ontology.
	 * @author Klaus Rudack
	 *
	 */
	public enum MARiskOnto {
		/**
		 * for all MARisk-Elements.
		 */
		ALL,
		
		/**
		 * for MARiskClause.
		 */
		CLAUSE,
		
		/**
		 * for MARiskEntry.
		 */
		ENTRY,
		
		/**
		 * for no MARisk-Element.
		 */
		NONE
	}
	
	/**
	 * Enumeration for the LAW-Elements in an ontology.
	 */
	public enum LAWOnto {
		/**
		 * for all Law-Elements.
		 */
		All,
		
		/**
		 * for Laws.
		 */
		LAW,
		
		/**
		 * for Paragraphs.
		 */
		PARAGRAPH,
		
		/**
		 * for Sections.
		 */
		SECTION,
		
		/**
		 * for Laws and Paragraphs.
		 */
		LAWPARAGRAPH,
		
		/**
		 * for Laws and Sections.
		 */
		LAWSECTION,
		
		/**
		 * for Paragraphs and Sections.
		 */
		PARAGRAPHSECTION,
		
		/**
		 * for no Law-Elements.
		 */
		NONE
	}
	
	/**
	 * Enumeration for the  Guideline-Elements in an ontology.
	 * @author dbuerger
	 *
	 */
	public enum GUIDELINEOnto {
		/**
		 * for all Guideline-Elements.
		 */
		ALL,
		
		/**
		 * for GuidelineEntries.
		 */
		ENTRY,
		
		/**
		 * for no Guideline-Element.
		 */
		NONE
	}
	
	/**
	 * Eine UID weil checkstyle das so will.
	 */
	private static final long serialVersionUID = -2348130769866541342L;

	/**
	 * Loads the set of pattern from an ontology file.
	 * @param filename The filename that contains the ontology.
	 * @param bsiOnto enumeration which BSI-Elements should be taken
	 * @param mariskOnto enumeration which MARisk-Elements should be taken
	 * @param lawOnto enumeration which Law-Elements should be taken
	 * @return true, iff the data was successfully loaded.
	 */
	public final boolean createFromOntology(final String filename, final BSIOnto bsiOnto, final MARiskOnto mariskOnto, final LAWOnto lawOnto, final GUIDELINEOnto guidelineOnto) {
		this.clear();
		File file = new File(filename);
		if (!file.exists()) {
			System.err.println("No file at given path: " + filename);
//			TODO KR: eventuell bessere Fehlerausgabe.
			return false;
		}
		boolean ret = true;
		ret &= createBSIFromOntology(file, bsiOnto);
		ret &= createLawFromOntology(file, lawOnto);
		ret &= createMARiskFromOntology(file, mariskOnto);
		ret &= createGuidelineFromOntology(file, guidelineOnto);
		return ret;
	}
	
	/**
	 * Loads the set of MARisk pattern from an ontology file.
	 * @param file The file that contains the ontology.
	 * @param flag a flag which elements should be loaded. 0 = all, 1 = ContentEntry, 2 = MariskClause
	 * @return true, iff the data was successfully loaded.
	 */
	public final boolean createMARiskFromOntology(final File file, final MARiskOnto flag) {		
		GenericOntologyHelper goh = new GenericOntologyHelper();
		goh.loadOWLOntologyFromFile(file);
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		Set<OWLNamedIndividual> bsiIndividuals = null;
		switch (flag) {
			case ALL:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_MARISKRULE, false);
				break;
			case CLAUSE:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_MARISKCLAUSE, false);
				break;
			case ENTRY:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_MARISKENTRY, false);
				break;
			case NONE:
				return true;
		default:
//			TODO KR: evtl Fehlermelung? Geht das hier ueberhaupt?
			return false;
		}
		
		// if-Abfrage ist mehr ein Hack, um die unterschiedlichen Annotation-Namen zu kompensieren
		if (flag == MARiskOnto.CLAUSE) {
			for (OWLNamedIndividual curInd : bsiIndividuals) {
				String name = curInd.getIRI().getFragment();
				// System.out.println("MARisk Name : " + name);

				String title = roh.getAnnotationFromIndividual_th(curInd,
						RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NAME);
				// System.out.println("MARisk Titel: " + title);

				String text = "";
				// System.out.println("MARisk Text : " + text);

				Pattern newPattern = new Pattern(new Text(name, TextKind.PATTERNNAME), new Text(title,TextKind.PATTERNTITLE), new Text(text, TextKind.PATTERNTEXT));
				this.add(newPattern);
			}
		} else {
			for (OWLNamedIndividual curInd : bsiIndividuals) {
				String name = curInd.getIRI().getFragment();
				// System.out.println("MARisk Name : " + name);

				String title = roh.getAnnotationFromIndividual_th(curInd,
						RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER);
				// System.out.println("MARisk Titel: " + title);

				String text = roh.getAnnotationFromIndividual_th(curInd,
						RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT);
				// System.out.println("MARisk Text : " + text);

				Pattern newPattern = new Pattern(
						new Text(name, TextKind.PATTERNNAME), 
						new Text(title,TextKind.PATTERNTITLE), 
						new Text(text, TextKind.PATTERNTEXT));
				this.add(newPattern);
				
				
//				TODO id sinvoller ersetzten
//				List<de.fraunhofer.isst.wbmp.model.Text> texts = new ArrayList<de.fraunhofer.isst.wbmp.model.Text>();
//				de.fraunhofer.isst.wbmp.model.Text textName = new de.fraunhofer.isst.wbmp.model.Text("name", 0);
//				Sentence sentence = new Sentence();
//				for (String s : name.split(" ")) {
//					sentence.addWord(new Word(s));
//				}
//				textName.addSentence(sentence);
//				texts.add(textName);
//				
//				de.fraunhofer.isst.wbmp.model.Text textTitle = new de.fraunhofer.isst.wbmp.model.Text("title", 0);
//				Sentence sentence1 = new Sentence();
//				for (String s : title.split(" ")) {
//					sentence.addWord(new Word(s));
//				}
//				textTitle.addSentence(sentence1);
//				texts.add(textTitle);
//				
//				de.fraunhofer.isst.wbmp.model.Text textText = new de.fraunhofer.isst.wbmp.model.Text("text", 0);
//				Sentence sentence2 = new Sentence();
//				for (String s : text.split(" ")) {
//					sentence.addWord(new Word(s));
//				}
//				textText.addSentence(sentence2);
//				texts.add(textText);
//				
//				
//				Entity entity = new Entity(String.valueOf(Math.random()), name, texts);
//				Activator.patternDocument.addEntity(entity);
			}
		}
		return true;
	}
	
	
	/**
	 * Loads the set of MARisk pattern from an ontology file.
	 * @param file The file that contains the ontology.
	 * @param flag a flag which elements should be loaded. 0 = all
	 * @return true, iff the data was successfully loaded.
	 */
	public final boolean createLawFromOntology(final File file, final LAWOnto flag) {
		GenericOntologyHelper goh = new GenericOntologyHelper();
		goh.loadOWLOntologyFromFile(file);
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		Set<OWLNamedIndividual> bsiIndividuals = null;
		switch (flag) {
			case All:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_LAWRULE, false);
				break;
			case LAW:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_LAW, false);
				break;
			case PARAGRAPH:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_PARAGRAPH, false);
				break;
			case SECTION:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_SECTION, false);
				break;
			case LAWPARAGRAPH:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_LAW, false);
				bsiIndividuals.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_PARAGRAPH, false));
				break;
			case LAWSECTION:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_LAW, false);
				bsiIndividuals.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_SECTION, false));
				break;
			case PARAGRAPHSECTION:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_PARAGRAPH, false);
				bsiIndividuals.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_SECTION, false));
				break;
			case NONE:
				//TH: evtl. hier break fuer BSI-Measure CSV-Export?;
				return true;
			default:
//				TODO KR: evtl Fehlermelung? Geht das hier ueberhaupt?
				return false;
		}
		for (OWLNamedIndividual curInd : bsiIndividuals) {
			String name = curInd.getIRI().getFragment();
//			System.out.println("Law Name : " + name);

//			String title = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER);
			String title = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_RULE_TITLE);
//			System.out.println("Law Titel: " + title);

//			String text = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT);
			String text = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_SECTION_CONTENT);
//			System.out.println("Law Text : " + text);

			Pattern newPattern = new Pattern(
					new Text(name, TextKind.PATTERNNAME), 
					new Text(title,TextKind.PATTERNTITLE), 
					new Text(text, TextKind.PATTERNTEXT));;
			this.add(newPattern);
			
//			TODO id sinvoller ersetzten
			List<de.fraunhofer.isst.wbmp.model.Text> texts = new ArrayList<de.fraunhofer.isst.wbmp.model.Text>();
//			de.fraunhofer.isst.wbmp.model.Text textName = new de.fraunhofer.isst.wbmp.model.Text("name", 0);
//			Sentence sentence = new Sentence();
//			for (String s : name.split(" ")) {
//				sentence.addWord(new Word(s));
//			}
//			textName.addSentence(sentence);
//			texts.add(textName);
//			
//			de.fraunhofer.isst.wbmp.model.Text textTitle = new de.fraunhofer.isst.wbmp.model.Text("title", 0);
//			Sentence sentence1 = new Sentence();
//			for (String s : title.split(" ")) {
//				sentence.addWord(new Word(s));
//			}
//			textTitle.addSentence(sentence1);
//			texts.add(textTitle);
//			
//			de.fraunhofer.isst.wbmp.model.Text textText = new de.fraunhofer.isst.wbmp.model.Text("text", 0);
//			Sentence sentence2 = new Sentence();
//			for (String s : text.split(" ")) {
//				sentence.addWord(new Word(s));
//			}
//			textText.addSentence(sentence2);
//			texts.add(textText);
//			
//			
//			Entity entity = new Entity(String.valueOf(Math.random()), name, texts);
//			Activator.patternDocument.addEntity(entity);
			
		}
		
		
		return true;
	}
	
	/**
	 * Loads the set of BSI pattern from an ontology file.
	 * @param file The file that contains the ontology.
	 * @param flag a flag which elements should be loaded. 0 = all
	 * @return true, iff the data was successfully loaded.
	 */
	public final boolean createBSIFromOntology(final File file, final BSIOnto flag) {
		GenericOntologyHelper goh = new GenericOntologyHelper();
		goh.loadOWLOntologyFromFile(file);
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		
		/*
		 * Übersetzung Ontologie -> Pattern:
		 * ID/IRI.getFragment -> Pattern.name
		 * Annotation "Name" -> Pattern.title
		 * Annotation "Text" -> Pattern.text
		 */
		
		Set<OWLNamedIndividual> bsiIndividuals = null;
		switch (flag) {
			case ALL: 
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIRULE, false);
				break;
			case ELEMENT:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIELEMENT, false);
				break;
			case MEASURE:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIMEASURE, false);
				break;
			case THREAT:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSITHREAT, false);
				break;
			case ELEMENTMEASURE:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIELEMENT, false);
				bsiIndividuals.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIMEASURE, false));
				break;
			case ELEMENTTHREAT:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIELEMENT, false);
				bsiIndividuals.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSITHREAT, false));
				break;
			case MEASURETHREAT:
				bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIMEASURE, false);
				bsiIndividuals.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSITHREAT, false));
				break;
			case NONE:
				return true;
		default:
//				TODO KR: evtl Fehlermelung? Geht das hier ueberhaupt?
				return false;
		}

		for (OWLNamedIndividual curInd : bsiIndividuals) {
//			System.out.println(i.getIRI().getFragment());
			String name = curInd.getIRI().getFragment(); //ist das der richtige Name?
//			System.out.println(name);
			
			String title = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_RULE_TITLE);
//			System.out.println(title);

			String text = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT);
//			System.out.println(text);
			
			//TODO TH: hier auch keywords auslesen und in pattern speichern
//			System.out.println(roh.getKeywordsAsSet(curInd));
			KeywordSet keywords = roh.getKeywordsAsSet(curInd);
			
			Pattern newPattern = new Pattern(
					new Text(name,TextKind.PATTERNNAME),
					new Text(title,TextKind.PATTERNTITLE),
					new Text(text,TextKind.PATTERNTEXT),
					keywords);
			this.add(newPattern);
//			TODO id sinvoller ersetzten
//			List<de.fraunhofer.isst.wbmp.model.Text> texts = new ArrayList<de.fraunhofer.isst.wbmp.model.Text>();
//			de.fraunhofer.isst.wbmp.model.Text textName = new de.fraunhofer.isst.wbmp.model.Text("name", 0);
//			Sentence sentence = new Sentence();
//			for (String s : name.split(" ")) {
//				sentence.addWord(new Word(s));
//			}
//			textName.addSentence(sentence);
//			texts.add(textName);
//			
//			de.fraunhofer.isst.wbmp.model.Text textTitle = new de.fraunhofer.isst.wbmp.model.Text("title", 0);
//			Sentence sentence1 = new Sentence();
//			for (String s : title.split(" ")) {
//				sentence.addWord(new Word(s));
//			}
//			textTitle.addSentence(sentence1);
//			texts.add(textTitle);
//			
//			de.fraunhofer.isst.wbmp.model.Text textText = new de.fraunhofer.isst.wbmp.model.Text("text", 0);
//			Sentence sentence2 = new Sentence();
//			for (String s : text.split(" ")) {
//				sentence.addWord(new Word(s));
//			}
//			textText.addSentence(sentence2);
//			texts.add(textText);
//			
//			de.fraunhofer.isst.wbmp.model.Text textKey = new de.fraunhofer.isst.wbmp.model.Text("keywords", 0);
//			Sentence sentence3 = new Sentence();
//			for (carisma.processanalysis.textmodel.Word w : keywords) {
//				sentence3.addWord(new Word(w.getContent()));
//			}
//			textKey.addSentence(sentence3);
//			texts.add(textKey);
//			
//			Entity entity = new Entity(String.valueOf(Math.random()), name, texts);
//			Activator.patternDocument.addEntity(entity);

		}
		return true;
	}
	
	/**
	 * Loads the set of Guideline pattern from an ontology file.
	 * @param file The file that contains the ontology.
	 * @param flag a flag which elements should be loaded. 0 = all
	 * @return true, if the data was successfully loaded.
	 */
	public final boolean createGuidelineFromOntology(final File file, final GUIDELINEOnto flag) {
		GenericOntologyHelper goh = new GenericOntologyHelper();
		goh.loadOWLOntologyFromFile(file);
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		
		/*
		 * Übersetzung Ontologie -> Pattern:
		 * ID/IRI.getFragment -> Pattern.name
		 * Annotation "Name" -> Pattern.title
		 * Annotation "Text" -> Pattern.text
		 */
		
		Set<OWLNamedIndividual> guidelineIndividuals = null;
		switch (flag) {
			case ALL: 
				guidelineIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_GUIDELINES, false);
				break;
			case ENTRY:
				guidelineIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_GUIDELINESENTRY, false);
				break;
			case NONE:
				return true;
		default:
				return false;
		}

		for (OWLNamedIndividual curInd : guidelineIndividuals) {
//			System.out.println(i.getIRI().getFragment());
			String name = curInd.getIRI().getFragment(); //ist das der richtige Name?
//			System.out.println(name);
			
			String title = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_GUIDELINESENTRY_TITLE);
//			System.out.println(title);

			String text = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_GUIDELINESENTRY_TEXT);
//			System.out.println(text);
			
			//TODO TH: hier auch keywords auslesen und in pattern speichern
//			System.out.println(roh.getKeywordsAsSet(curInd));
			KeywordSet keywords = roh.getKeywordsAsSet(curInd);
			
			Pattern newPattern = new Pattern(
					new Text(name,TextKind.PATTERNNAME),
					new Text(title,TextKind.PATTERNTITLE),
					new Text(text,TextKind.PATTERNTEXT),
					keywords);
			this.add(newPattern);
			
			
////			TODO id sinvoller ersetzten
//			List<de.fraunhofer.isst.wbmp.model.Text> texts = new ArrayList<de.fraunhofer.isst.wbmp.model.Text>();
//			de.fraunhofer.isst.wbmp.model.Text textName = new de.fraunhofer.isst.wbmp.model.Text("name", 0);
//			Sentence sentence = new Sentence();
//			for (String s : name.split(" ")) {
//				sentence.addWord(new Word(s));
//			}
//			textName.addSentence(sentence);
//			texts.add(textName);
//			
//			de.fraunhofer.isst.wbmp.model.Text textTitle = new de.fraunhofer.isst.wbmp.model.Text("title", 0);
//			Sentence sentence1 = new Sentence();
//			for (String s : title.split(" ")) {
//				sentence.addWord(new Word(s));
//			}
//			textTitle.addSentence(sentence1);
//			texts.add(textTitle);
//			
//			de.fraunhofer.isst.wbmp.model.Text textText = new de.fraunhofer.isst.wbmp.model.Text("text", 0);
//			Sentence sentence2 = new Sentence();
//			for (String s : text.split(" ")) {
//				sentence.addWord(new Word(s));
//			};
//			textText.addSentence(sentence2);
//			texts.add(textText);
//			
//			de.fraunhofer.isst.wbmp.model.Text textKey = new de.fraunhofer.isst.wbmp.model.Text("keywords", 0);
//			Sentence sentence3 = new Sentence();
//			for (carisma.processanalysis.textmodel.Word w : keywords) {
//				sentence3.addWord(new Word(w.getContent()));
//			}
//			textKey.addSentence(sentence3);
//			texts.add(textKey);
//			
//			Entity entity = new Entity(String.valueOf(Math.random()), name, texts);
//			Activator.patternDocument.addEntity(entity);

		}
		return true;
	}
	
	
	/**
	 * Retrieves a pattern by its name.
	 * @param name The name of the pattern to be found.
	 * @return A pattern with the given title or null, if no such pattern is in the catalogue. 
	 */
	public final Pattern getPatternByName(final String name) {
		for (Pattern curPattern : this) {
			if (curPattern.getName().equals(name)) {
				return curPattern;
			}
		}
		return null;
	}
	
	// Zum Exportieren der Ontologie als XML für WBMP
// FIXME: Was soll das? Warum werden hier einfach Dateistrukturen verwendet, die gar nicht in CARiSMA so vorhanden sind?
// TODO: Weg damit und absprechen, wie das wirklich gemacht werden soll
//	public static void main(final String[] args) {
//		PatternCatalog catalog = new PatternCatalog();
//		
//		//for BSI
//		catalog.createFromOntology("resources/ontology.owl", BSIOnto.ALL, MARiskOnto.NONE, LAWOnto.NONE, GUIDELINEOnto.NONE);
//		System.out.println("BSI-Katalog enthält " + catalog.size() + " BSI-Pattern.");
//		Factory factory = new Factory();
//		Document docBSI =  new Document("right", "BSI", factory);
//		if (catalog.size() > 0) {
//			for (Pattern curPattern : catalog) {
//				Entity newEntity = docBSI.createEntity(curPattern.getName().getEntityText(), curPattern.getName().getEntityText());
//	//			newEntity.createUnparsedText(newEntity.getId() + "_title", newEntity.getId() + "_title", 1.5f);
//				newEntity.createAndParseText(newEntity.getId() + "_title", newEntity.getId() + "_title", 1.5f, curPattern.getTitle().getEntityText());
//				newEntity.createAndParseText(newEntity.getId() + "_text", newEntity.getId() + "_text", .2f, curPattern.getText().getEntityText());			
//			}
//		}
//		(new XMLDocumentExporter.UnparsedTextXMLDocumentExporter()).export(docBSI, "resources/bsi.xml");
//		
//		//for MARisk
//		catalog = new PatternCatalog();
//		catalog.createFromOntology("resources/ontology.owl", BSIOnto.NONE, MARiskOnto.ALL, LAWOnto.NONE, GUIDELINEOnto.NONE);
//		System.out.println("MARisk-Katalog enthält " + catalog.size() + " MARisk-Pattern.");
//		Document docMARisk =  new Document("right", "MARisk", factory);
//		if (catalog.size() > 0) {
//			for (Pattern curPattern : catalog) {
//				Entity newEntity = docMARisk.createEntity(curPattern.getName().getEntityText(), curPattern.getName().getEntityText());
//	//			newEntity.createUnparsedText(newEntity.getId() + "_title", newEntity.getId() + "_title", 1.5f);
//				newEntity.createAndParseText(newEntity.getId() + "_title", newEntity.getId() + "_title", 1.5f, curPattern.getTitle().getEntityText());
//				newEntity.createAndParseText(newEntity.getId() + "_text", newEntity.getId() + "_text", .2f, curPattern.getText().getEntityText());			
//			}
//		}
//		(new XMLDocumentExporter.UnparsedTextXMLDocumentExporter()).export(docMARisk, "resources/marisk.xml");
//		
//		//for LAW
//		catalog = new PatternCatalog();
//		catalog.createFromOntology("resources/ontology.owl", BSIOnto.NONE, MARiskOnto.NONE, LAWOnto.All, GUIDELINEOnto.NONE);
//		System.out.println("LAW-Katalog enthält " + catalog.size() + " LAW-Pattern.");
//		Document docLAW =  new Document("right", "Law", factory);
//		if (catalog.size() > 0) {
//			for (Pattern curPattern : catalog) {
//				Entity newEntity = docLAW.createEntity(curPattern.getName().getEntityText(), curPattern.getName().getEntityText());
//	//			newEntity.createUnparsedText(newEntity.getId() + "_title", newEntity.getId() + "_title", 1.5f);
//				newEntity.createAndParseText(newEntity.getId() + "_title", newEntity.getId() + "_title", 1.5f, curPattern.getTitle().getEntityText());
//				newEntity.createAndParseText(newEntity.getId() + "_text", newEntity.getId() + "_text", .2f, curPattern.getText().getEntityText());			
//			}
//		}
//		(new XMLDocumentExporter.UnparsedTextXMLDocumentExporter()).export(docLAW, "resources/law.xml");
//		
//		//for Guideline
//		catalog = new PatternCatalog();
//		catalog.createFromOntology("resources/ontology.owl", BSIOnto.NONE, MARiskOnto.NONE, LAWOnto.NONE, GUIDELINEOnto.ALL);
//		System.out.println("Guideline-Katalog enthält " + catalog.size() + " Guideline-Pattern.");
//		Document docGUIDE =  new Document("right", "Guideluine", factory);
//		if (catalog.size() > 0) {
//			for (Pattern curPattern : catalog) {
//				Entity newEntity = docGUIDE.createEntity(curPattern.getName().getEntityText(), curPattern.getName().getEntityText());
//	//			newEntity.createUnparsedText(newEntity.getId() + "_title", newEntity.getId() + "_title", 1.5f);
//				newEntity.createAndParseText(newEntity.getId() + "_title", newEntity.getId() + "_title", 1.5f, curPattern.getTitle().getEntityText());
//				newEntity.createAndParseText(newEntity.getId() + "_text", newEntity.getId() + "_text", .2f, curPattern.getText().getEntityText());			
//			}
//		}
//		(new XMLDocumentExporter.UnparsedTextXMLDocumentExporter()).export(docGUIDE, "resources/guideline.xml");
//	}

}
