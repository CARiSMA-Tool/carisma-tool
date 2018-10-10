package carisma.check.riskdetector;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.eclipse.emf.ecore.resource.Resource;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import com.csvreader.CsvReader;


import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;
import carisma.processanalysis.texttools.KeywordSet;
import carisma.regulatory.ontology.utils.GenericOntologyHelper;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import de.fraunhofer.isst.wbmp.io.exporter.XMLDocumentExporter;
import de.fraunhofer.isst.wbmp.matching.EntityMatch;
import de.fraunhofer.isst.wbmp.matching.Result;
import de.fraunhofer.isst.wbmp.matching.TextMatch;
import de.fraunhofer.isst.wbmp.matching.WordMatch;
import de.fraunhofer.isst.wbmp.model.Document;
import de.fraunhofer.isst.wbmp.model.Entity;
import de.fraunhofer.isst.wbmp.model.ExtensionKind;
import de.fraunhofer.isst.wbmp.model.Factory;
import de.fraunhofer.isst.wbmp.pipeline.Pipeline;
import de.fraunhofer.isst.wbmp.pipeline.filter.DocumentAllWordsFilter;
import de.fraunhofer.isst.wbmp.pipeline.filter.StopWordFilter;
import de.fraunhofer.isst.wbmp.reporting.ConsoleReporter;
import de.fraunhofer.isst.wbmp.tool.Tool;

/**
 * This class checks a model for risks.
 * @author Klaus Rudack
 *
 */
public class RiskDetectorCheck implements CarismaCheck {

	/** AnalysisHost for structured output. */
	private AnalysisHost host; 
	
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters,
			final AnalysisHost host) {
		boolean optionalInfos = true;
		File ontology = null;
		File extensionMapFile = null;
		File originMapFile = null;
		char extensionDelimiter = ' ';
		char originDelimiter = ' ';
		if (host != null) {
			this.host = host;
		} else {
			this.host = new DummyHost(true);
		}
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			Logger.log(LogLevel.ERROR, "Empty model");
			return false;
		}
		try {
			ontology = ((InputFileParameter) parameters.get("carisma.check.riskdetector.ontologyFile")).getValue();
		} catch (NullPointerException e) {
			this.host.appendLineToReport("Couldn't find parameter carisma.check.riskdetector.ontologyFile!");
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Couldn't find parameter carisma.check.riskdetector.ontologyFile!"));
			return false;
		}
		if (!ontology.exists()) {
			this.host.appendLineToReport("Incorrect Ontology File: " + ontology.getAbsolutePath());
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Incorrect Ontology File."));
			return false;
		}
		Document modelDocument;
		try {
			modelDocument = getModelAsDocument(currentModel);
		} catch (RegisterNotInUseException e) {
			this.host.appendLineToReport("Model has not been loaded!\nRun the BPMN2 Importer for process analysis before running this check!");
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Model has not been loaded!"
					+ "\nRun the BPMN2 Importer for process analysis before running this check!"));
			return false;
		}
		
		
		Document patternDocument = getOntologyAsPattern(ontology);
//		Die nächsten 2 Zeilen um die Ontology als XML-File zu speichern
//		XMLDocumentExporter expo = new XMLDocumentExporter();
//		expo.export(patternDocument, "/resources/onto.xml");
//		TODO KR: pipeline anders erzeugen?
		Tool tool = null;
		try {
			Pipeline pipeline = new Pipeline(new DocumentAllWordsFilter(
					new StopWordFilter()));
			tool = new Tool(modelDocument, patternDocument, pipeline);
			ConsoleReporter.printResultToConsole(tool.execute());
		} catch (IOException ioe) {
			ioe.printStackTrace();
		}

		
		////////////////////////////////////////////////////////////////Config lesen
		try {
			extensionMapFile = ((InputFileParameter) parameters.get("carisma.check.riskdetector.extMapping")).getValue();
			if (extensionMapFile == null || !extensionMapFile.exists()) {
				optionalInfos = false;
			}
			String extDel = ((StringParameter) parameters.get("carisma.check.riskdetector.extDelimiter")).getValue();
			if (extDel != null && !extDel.equals("")) {
				extensionDelimiter = extDel.charAt(0);
			} else {
				optionalInfos = false;
			}		
			originMapFile = ((InputFileParameter) parameters.get("carisma.check.riskdetector.originMapping")).getValue();
			if (originMapFile == null || !originMapFile.exists()) {
				optionalInfos = false;
			}
			String originDel = ((StringParameter) parameters.get("carisma.check.riskdetector.originDelimiter")).getValue();
			if (originDel != null && !originDel.equals("")) {
				originDelimiter = originDel.charAt(0);
			} else {
				optionalInfos = false;
			}
			if (optionalInfos) {
				tool.setscoreMapping(readExtensionKindConfig(extensionMapFile.getAbsolutePath(), extensionDelimiter));
				tool.setOriginMapping(readConfig(originMapFile.getAbsolutePath(), originDelimiter));
			}
		} catch (NullPointerException e) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "No valid input for the optional mapping!"));
			host.appendLineToReport("No valid input for the optional mapping!");
		}
		////////////////////////////////////////////////////////////////
		
		Result result = tool.execute();
		ArrayList<WordMatch> wordMatches = new ArrayList<WordMatch>();
		for (EntityMatch entityMatch : result.getMatchings()) {
			for (TextMatch textMatch : entityMatch.getTextMatches()) {
				wordMatches.addAll(textMatch.getWordMatches());
			}
		}
		generateStructuredOutput(wordMatches);
		return true;
	}
	
	/**
	 * Loads the content of a model and stores it in a {@link Document}.
	 * The model has to be loaded by the BPMN2 Importer
	 * @param resource the modelResource
	 * @return {@link Document} with the content of the model/Resource
	 * @throws RegisterNotInUseException will be thrown if you didn't load the model via the BPMN2 Importer
	 */
	private Document getModelAsDocument(final Resource resource) throws RegisterNotInUseException {
		Document document = new Document(new Factory());

		ProcessDescription processDescription = null;
		processDescription = (ProcessDescription) host.getFromRegister(ProcessDescription.CARISMA_REGISTRY_KEY);
		for (ProcessEntity pe : processDescription.getEntities()) {
			Entity entity = document.createEntity(String.valueOf(Math.random()), pe.getName());
			
			entity.createText("", "name", 0, pe.getName(), "", "");
			
//			Sentence sentence = new Sentence();
//			for (String s : pe.getName().split(" ")) {
//				sentence.addWord(new Word(s));
//			}
//			textName.addSentence(sentence);
				
//				de.fraunhofer.isst.wbmp.model.Text textTitle = new de.fraunhofer.isst.wbmp.model.Text("title", 0);
//				Sentence sentence1 = new Sentence();
//				sentence1.addWord(new de.fraunhofer.isst.wbmp.model.Word(title));
//				textTitle.addSentence(sentence1);
//				texts.add(textTitle);
//				im entity kein titel gefunden
			
			StringBuffer allText = new StringBuffer();
			for (carisma.processanalysis.textmodel.Text text : pe.getTexts()) {
				for (carisma.processanalysis.textmodel.Word word : text.getWordList()) {
					allText.append(word.getContent());
					allText.append(".");
				}
			}
			entity.createText("", "text", 0, allText.toString(), "", "");
			
//				Sentence sentence2 = new Sentence();
//				for (String s : pe.getName().split(" ")) {
//					sentence2.addWord(new Word(s));
//				}
//			textText.addSentence(sentence2);
		}
		return document;
	}
	
	/**
	 * Loads an ontology and stores it as {@link Document}.
	 * @param file The ontology
	 * @return {@link Document} that holds the content of the ontology
	 */
	private Document getOntologyAsPattern(final File file) {
		Document document = new Document(new Factory());
		GenericOntologyHelper goh = new GenericOntologyHelper();
		goh.loadOWLOntologyFromFile(file);
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		Set<OWLNamedIndividual> mariskIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_MARISKRULE, false);
		Set<OWLNamedIndividual> bsiIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIRULE, false);
		Set<OWLNamedIndividual> lawIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_LAWRULE, false);
		Set<OWLNamedIndividual> guideIndividuals = goh.getIndividuals(RegulatoryOntologyHelper.CLS_GUIDELINES, false);
		for (OWLNamedIndividual curInd : mariskIndividuals) {
			String name = curInd.getIRI().getFragment();
			String title = roh.getAnnotationFromIndividual_th(curInd,
					RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER);
			String text = roh.getAnnotationFromIndividual_th(curInd,
					RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT);
			createEntity(name, title, text, null, document);
		}
		for (OWLNamedIndividual curInd : bsiIndividuals) {
			String name = curInd.getIRI().getFragment();
			String title = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_RULE_TITLE);
			String text = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT);
			KeywordSet keywords = roh.getKeywordsAsSet(curInd);
			createEntity(name, title, text, keywords, document);
		}
		for (OWLNamedIndividual curInd : lawIndividuals) {
			String name = curInd.getIRI().getFragment();
			String title = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_RULE_TITLE);
			String text = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_SECTION_CONTENT);
			createEntity(name, title, text, null, document);
		}
		for (OWLNamedIndividual curInd : guideIndividuals) {
			String name = curInd.getIRI().getFragment();
			String title = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_GUIDELINESENTRY_TITLE);
			String text = roh.getAnnotationFromIndividual_th(curInd, RegulatoryOntologyHelper.PROP_GUIDELINESENTRY_TEXT);
			KeywordSet keywords = roh.getKeywordsAsSet(curInd);
			createEntity(name, title, text, keywords, document);
		}
		return document;
	}
	
	/**
	 * Generates a structured output for the given pairs.
	 * @param pairs {@link List} with pairs that should be given as output
	 */
	private void generateStructuredOutput(final List<WordMatch> pairs) {
//		TODO KR: strukturierte Ausgabe erzeugen.
		host.appendLineToReport(pairs.size() + " matches found!");
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, pairs.size() + " matches found!"));
	}
	
	/**
	 * Creates an {@link Entity} with the given attributes.
	 * @param name name of the entity
	 * @param title title of the entity
	 * @param text text of the entity
	 * @param keywords keywords of the entity, can be null
	 * @return an {@link Entity} filled with the given attributes
	 */
	private Entity createEntity(final String name, final String title, final String text, final KeywordSet keywords, final Document document) {
//		TODO KR: id sinvoll setzten
		float defaultWeight = 1.0f;
		Entity entity = document.createEntity(String.valueOf(Math.random()), name);
//		Text textName = new Text("name", 0);
//		Sentence sentence = new Sentence();
//		for (String s : name.split(" ")) {
//			sentence.addWord(new Word(s));
//		}
//		textName.addSentence(sentence);
		
		String regEx = "";
		
		if (name != null && !name.trim().isEmpty()) {
			entity.createText("", "name", defaultWeight, name, regEx, "");
		}
		
//		Text textTitle = new Text("title", 0);
//		Sentence sentence1 = new Sentence();
//		for (String s : title.split(" ")) {
//			sentence.addWord(new Word(s));
//		}
//		textTitle.addSentence(sentence1);
		if (title != null && !title.trim().isEmpty()) {
			entity.createText("", "title", defaultWeight, title, regEx, "");
		}
		
//		Text textText = new Text("text", 0);
//		Sentence sentence2 = new Sentence();
//		for (String s : text.split(" ")) {
//			sentence.addWord(new Word(s));
//		}
//		textText.addSentence(sentence2);

		if (text != null && !text.trim().isEmpty()) {
			entity.createText("", "text", defaultWeight, text, regEx, "");
		}
		
		if (keywords != null) {
//			Text textKey = new Text("keywords", 0);
//			Sentence sentence3 = new Sentence();
//			for (carisma.processanalysis.textmodel.Word w : keywords) {
//				sentence3.addWord(new Word(w.getContent()));
//			}
//			textKey.addSentence(sentence3);
			
			StringBuffer keyWordTmp = new StringBuffer();
			for (carisma.processanalysis.textmodel.Word w : keywords) {
				keyWordTmp.append(w.getContent());
				keyWordTmp.append(" ");
			}

			if (title != null && !keyWordTmp.toString().trim().isEmpty()) {
				entity.createText("", "keywords", defaultWeight, keyWordTmp.toString(), regEx, "");
			}
		}
		
		return entity;
	}
	
	
	/**
	 * Converts a Map<String, Float> into a Map<ExtensionKind, Float>.
	 * 
	 * Throws an IllegalArgumentException if a String in the original Map doesn't match to an ExtensionKind.
	 * 
	 * @param pathName Path name.
	 * @param delimiter Character which indicates a new column. 
	 * @return HashMap mapping ExtensionKinds to Float values.
	 */
	private Map<ExtensionKind, Float> readExtensionKindConfig(final String pathName, final char delimiter) {
		Map<ExtensionKind, Float> config = new HashMap<ExtensionKind, Float>();
		for (Entry<String, Float> entry : readConfig(pathName, delimiter).entrySet()) {
			config.put(ExtensionKind.valueOf(entry.getKey()), entry.getValue());
		}
		return config;
	}
	
	/**
	 * Reads a configuration file, using a csv-reader, and converts it into a Map.
	 * The expected structure of the file is a text divided by one delimiter. After the delimiter are only numerical characters allowed.
	 * TODO modify in such way that it can read the configuration file within the jar file. 
	 * 
	 * @param pathName Path name.  
	 * @param delimiter Character which indicates a new column. 
	 * @return HashMap mapping Strings to Float values.
	 */
	private Map<String, Float> readConfig(final String pathName, final char delimiter) {
		Map<String, Float> config = new HashMap<String, Float>();
		
		BufferedReader br = null;
		CsvReader reader = null;
		try {
			while (ClassLoader.getSystemResources("ExtensionKindMapping.txt").hasMoreElements()) {
				this.getClass().getClassLoader();
				URL tmpURL = ClassLoader.getSystemResources("ExtensionKindMapping.txt").nextElement();
				System.out.println(tmpURL.toString());
			}
			InputStream inputStream = ClassLoader.getSystemResourceAsStream(pathName);
			br = new BufferedReader(new InputStreamReader(
					inputStream, "UTF-8"));
			reader = new CsvReader(br, delimiter);
				// read each line
			while (reader.readRecord()) {
				if (reader.getColumnCount() > 2) {
					throw new IllegalArgumentException("The specified file has too many delimiters(" + delimiter + "). Only ONE is allowed.");
				} else if (reader.getColumnCount() < 1) {
					throw new IllegalArgumentException("The specified file has not enough delimiters(" + delimiter + "). Only ONE is allowed.");
				}
				config.put(reader.getValues()[0], new Float(reader.getValues()[1]));
			}
			br.close();
			reader.close();
		} catch (IOException e1) {
			e1.printStackTrace();
			return null;
		} finally {
			try {
				br.close();
				reader.close();
			} catch (IOException e) {
				e.printStackTrace();
			} catch (NullPointerException e) {
				e.printStackTrace();
			}
		}
		return config;
	}

}
