/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   {SecSE group} - initial API and implementation and/or initial documentation
 ******************************************************************************/
package carisma.check.riskfinder;

import java.awt.Color;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Observable;
import java.util.Observer;
import java.util.TreeSet;

import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Stereotype;

import carisma.check.processanalysis.texttools.wortschatz.WSExpander;
import carisma.check.processanalysis.texttools.wortschatz.WSNormalizer;
import carisma.check.processanalysis.texttools.wortschatz.WrongElementException;
import carisma.check.processanalysis.texttools.wortschatz.WSNormalizer.NormalizerMode;
import carisma.check.riskfinder.PatternCatalog.BSIOnto;
import carisma.check.riskfinder.PatternCatalog.GUIDELINEOnto;
import carisma.check.riskfinder.PatternCatalog.LAWOnto;
import carisma.check.riskfinder.PatternCatalog.MARiskOnto;
import carisma.check.riskfinder.processors.DomainSpecificTaxonomyExpanderProcessor;
import carisma.check.riskfinder.processors.StringSetProcessorChain;
import carisma.check.riskfinder.processors.WSExpanderProcessor;
import carisma.check.riskfinder.processors.WSNormalizerProcessor;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.IntegerParameter;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.bpmn2.yaoqiang.YaoqiangHelper;
import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;
import carisma.processanalysis.textmodel.Word;
import carisma.processanalysis.texttools.DomainSpecificTaxonomyExpander;
import de.fraunhofer.isst.wbmp.model.*;


/**
 * Main class for
 * 
 * @author thumberg
 * 
 */

public class Check extends Observable implements CarismaCheck {
//public class Check implements CarismaCheck {

	/** The host of the check. **/
	private AnalysisHost host;

	private boolean synonymsOnly = false;
	private boolean append = true;
	private File stopwordFile = null;
	private File ontologyFile = null;
	private File xmlResultFile = null;
//	TODO KR: hier muessen auch noch die Enumeration für die ontology-elemente stehen.
//	private File extCacheFile = null;


	//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		private ArrayList<AnalyserResult> resultAr = new ArrayList<AnalyserResult>();
		private static Check instance;
		private Resource currentModel;
		private ArrayList<String> entityNames = new ArrayList<String>();
		private ProcessDescription pd;
	//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
	/**
	 * Execute the riskfinder plugin.
	 * 
	 * @param parameters
	 *            A list of parameters.
	 * @param host
	 *            The host.
	 * @return ??? //TODO
	 */
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		if (Activator.patternDocument == null) {
			Activator.patternDocument = new Document();
		}
		if (Activator.processDocument == null) {
			Activator.processDocument = new Document();
		}
		HTMLReport htmlWriter = null;
		this.host = host;
		register();
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		instance = this;
		currentModel = host.getAnalyzedModel();
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		// Set given parameters for this analysis
		synonymsOnly = ((BooleanParameter) parameters.get("carisma.check.riskfinder.synonymsOnly")).getValue();
		append = ((BooleanParameter) parameters.get("carisma.check.riskfinder.append")).getValue();
		boolean writeback = ((BooleanParameter) parameters.get("carisma.check.riskfinder.writeback")).getValue();

		stopwordFile = ((InputFileParameter) parameters.get("carisma.check.riskfinder.stopwordFile")).getValue();
		ontologyFile = ((InputFileParameter) parameters.get("carisma.check.riskfinder.ontologyFile")).getValue();
		xmlResultFile =  ((OutputFileParameter) parameters.get("carisma.check.riskfinder.xmlresultfile")).getValue();
//		extCacheFile = ((InputFileParameter) parameters.get("carisma.check.riskfinder.extcachefile")).getValue();
//		System.err.println("DEBUG: The parameter 'external cache file' will be ignored!"); //TODO
		int minScore = ((IntegerParameter) parameters.get("carisma.check.riskfinder.minscore")).getValue();

		if (stopwordFile == null || ontologyFile == null) {
			return false;
		}
		// TODO: warum hier nochmal? currentModel gibt es schon als member-variable
		Resource currentModel = host.getAnalyzedModel();

		//TODO: wird das ueberhaupt noch verwendet? kommt das modell nicht im naechsten Absatz aus der Registry?
		// abort, if there is no model to check
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			Logger.log(LogLevel.ERROR, "Empty model");
			return false;
		}

		//Use one of the imports to fill the data model
		//choose the right one by determining via instanceOf
		ProcessDescription processDescription = null;
		
		try {
			processDescription = (ProcessDescription) host.getFromRegister(ProcessDescription.CARISMA_REGISTRY_KEY);
//			KR: hier für den neuen Riskfinder testen
			for (ProcessEntity pe : processDescription.getEntities()) {
				
				List<de.fraunhofer.isst.wbmp.model.Text> texts = new ArrayList<de.fraunhofer.isst.wbmp.model.Text>();
				
				de.fraunhofer.isst.wbmp.model.Text textName = new de.fraunhofer.isst.wbmp.model.Text("name", 0);
				Sentence sentence = new Sentence();
				for (String s : pe.getName().split(" ")) {
					sentence.addWord(new de.fraunhofer.isst.wbmp.model.Word(s));
				}
				textName.addSentence(sentence);
				texts.add(textName);
				
//				de.fraunhofer.isst.wbmp.model.Text textTitle = new de.fraunhofer.isst.wbmp.model.Text("title", 0);
//				Sentence sentence1 = new Sentence();
//				sentence1.addWord(new de.fraunhofer.isst.wbmp.model.Word(title));
//				textTitle.addSentence(sentence1);
//				texts.add(textTitle);
//				im entity kein titel gefunden
				
				de.fraunhofer.isst.wbmp.model.Text textText = new de.fraunhofer.isst.wbmp.model.Text("text", 0);
				Sentence sentence2 = new Sentence();
				for (String s : pe.getName().split(" ")) {
					sentence2.addWord(new de.fraunhofer.isst.wbmp.model.Word(s));
				}
				textText.addSentence(sentence2);
				texts.add(textText);
				Entity entity = new Entity(String.valueOf(Math.random()), pe.getName(), texts);
				Activator.processDocument.addEntity(entity);
			}
			//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			// TODO TH: wofuer braucht man pd? bzw. wenn pd, wofuer dann noch processDescription?
			pd = processDescription;
			pd.addUmlaute();
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		} catch (RegisterNotInUseException e1) {
			e1.printStackTrace();
			return false;
		}
		
		// Workaround Postprocessing:
		processDescription.addUmlaute();

		// load pattern catalog
		PatternCatalog catalog = new PatternCatalog();
		//catalog.createFromOntology(ontologyFile.getAbsolutePath(), BSIOnto.ALL, MARiskOnto.ALL, LAWOnto.All);
		catalog.createFromOntology(ontologyFile.getAbsolutePath(), BSIOnto.ALL, MARiskOnto.CLAUSE, LAWOnto.All, GUIDELINEOnto.ALL);
//		TODO KR: hier muessen dann die enumeration ermittelt werden

		// load stop words
		StopwordFilter stopwords = new StopwordFilter();		
		stopwords.loadFromFile(stopwordFile.getAbsolutePath());
//		ArrayList<StringFilter> filters = new ArrayList<StringFilter>();
//		filters.add(stopwords);		
		StringSetProcessorChain patternChain = new StringSetProcessorChain();
		StringSetProcessorChain processChain = new StringSetProcessorChain();
		patternChain.addLast(stopwords);
//		processChain.addLast(stopwords); TODO: nocht Testen! hat Einfluss auf Ergebnis!
		
		WSNormalizerProcessor normalizerProcessor = null;
		WSNormalizer normalizer = null;
		try {
			normalizer = new WSNormalizer(NormalizerMode.USE_CACHE_AND_NET);
			normalizerProcessor = new WSNormalizerProcessor(normalizer);
			patternChain.add(normalizerProcessor);
			processChain.add(normalizerProcessor);
		} catch (WrongElementException e1) {
			e1.printStackTrace();
		}
		
		
		WSExpander expander = null;
		WSExpanderProcessor expanderProcessor;
		try {
			expander = new WSExpander(WSExpander.ExpanderMode.USE_CACHE_AND_NET);
//			expander = new WSExpander(WSExpander.USE_ONLY_CACHE);
			// expander.calcExtensions(processDescription, synonymsOnly, append); mit #1446 entfallen
			expanderProcessor = new WSExpanderProcessor(expander);
			expanderProcessor.setAppend(append);
			expanderProcessor.setSynonymsOnly(synonymsOnly);
			processChain.addLast(expanderProcessor);
//			expander.storeCache(); mit #1446 entfallen bzw. verschoben
		} catch (WrongElementException e) {
			e.printStackTrace();
		}
				
		// eigene Taxonomie zur Erweiterung -> hier? -> so korrekt?		 
		DomainSpecificTaxonomyExpander dste = new DomainSpecificTaxonomyExpander();
		DomainSpecificTaxonomyExpanderProcessor dstProcessor = new DomainSpecificTaxonomyExpanderProcessor(dste);
		dstProcessor.setAppend(true);
		dstProcessor.setSynonymsOnly(synonymsOnly);
//		dste.calcExtensions(processDescription, synonymsOnly, true);
		processChain.addLast(dstProcessor);
		processChain.addLast(stopwords);
		processChain.add(normalizerProcessor);
		
		processChain.addFirst(stopwords); // test
		
		host.appendLineToReport("ProcessChain: " + processChain.getDescription());
		host.appendLineToReport("PatternChain: " + patternChain.getDescription());
		
		// prepare analysis
		PeschkeAnalyser analyser = new PeschkeAnalyser();
		AnalyserResult result;
		

		//prepare results, start with a short overview for the user 
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO,
				"Finished checking of " + processDescription.getEntities().size() + " entities. See report for details."));
		
		int yellowThreshold = 250;
		int redThreshold = 500;
		
		if (writeback) {
			YaoqiangHelper.clearAllWarningAndInfoFlags(currentModel.getContents().get(0));
		}
		
		/// XML-Export
		XMLResult xmlResult = new XMLResult();
		xmlResult.init(ontologyFile, synonymsOnly, stopwordFile, append, minScore, writeback, xmlResultFile);
		
		// HTML Export 
		try {
			htmlWriter = new HTMLReport(xmlResultFile.getAbsolutePath().replace("xml", "html"), host);
		} catch (IOException e1) {	// TODO handle the exception
			e1.printStackTrace();
		}
		//then gather detailed results
//		for (ProcessEntity curEntity : processDescription.getEntities()) {	
//			
//			//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//			result = analyser.getRelevantPatterns(catalog, curEntity, processChain, patternChain);
//			resultAr.add(result);
//			entityNames.add(curEntity.getTexts().get(0).getEntityText());
//		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//			
//			// Fuer Report
//			AnalyserResult filteredResult = result.getFilteredByMinscore(minScore);
////			result = analyser.getRelevantPatterns(catalog, curEntity, filters, minScore);
////			/*debug:*/ host.appendLineToReport("Score: " + result.getScore_method1(3));
////			host.appendLineToReport("[" + curEntity.getTexts().get(0).getEntityText() + " (" + ((int) result.getScore_method1(3)) + ")]");
//			host.appendLineToReport("[" + curEntity.getAllEntityTexts() + " (" + ((int) filteredResult.getScore_method1(3)) + ")]");
//			host.appendLineToReport(filteredResult.getAllRelevantWords().size() + " Relevante Worte: " /* + filteredResult.getAllRelevantWordsSorted()*/);
//
//			// Sehr ausführlich:
//			NavigableSet<Word> relWords = filteredResult.getAllRelevantWordsSorted(); //.descendingSet();
//			for(Word curWord : relWords)
//				host.appendLineToReport(curWord.getWordsOnPathToRootAsString());
//			host.appendLineToReport("");
//			
//			host.appendLineToReport(filteredResult.size() + " relevante Pattern:");
//			host.appendLineToReport(filteredResult.toPrettyString("    "));
//			
//			
//			xmlResult.appendXmlResultComplete(filteredResult.toXMLString());
//			xmlResult.appendXmlSimpleResult(filteredResult.toSimpleXMLString());
//			
//			// append html part
//			htmlWriter.appendEntry(curEntity, filteredResult);
//
//			// Einfaerben im Modell:
//			if (writeback) {
//				double curScore = filteredResult.getScore_method1(3);
//				if (curScore >= redThreshold) {
//					YaoqiangHelper.setWarningFlagToBpmn2Element((org.eclipse.bpmn2.impl.TaskImpl) curEntity.getObject(), null, Color.red);
//				} else if (curScore >= yellowThreshold) {
//					YaoqiangHelper.setWarningFlagToBpmn2Element((org.eclipse.bpmn2.impl.TaskImpl) curEntity.getObject(), null, Color.yellow);
//				}
//			}
//		}
	
		xmlResult.finish();
		
		htmlWriter.finish();
		
		if(normalizer != null)
			normalizer.storeCache();
		
		if(expander != null)
			expander.storeCache();

		//System.out.println(xmlResultComplete);
		// XML-Datei schreiben:
		// TODO TH: in XML-Logger-Klasse auslagern, Encoding setzen!
		if (xmlResultFile != null) {
			XMLReport.createReport(xmlResult, xmlResultFile);
		}
		setChanged();
		// visualize results in diagram, or write it as text to the console
		if (writeback) {
			try {
				currentModel.save(Collections.EMPTY_MAP);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		notifyObservers();
//		TODO Klaus R.: hier die Riskfinderview aufrufen.
//		Problem: Der Riskfinder kennt seine GUI nicht. Gemeint ist nicht die GUI upzudaten, sondern sie anzuzeigen.
		return true;
	}
	
	/**
	 * Print some content of the examined model?
	 * 
	 * @param element
	 *            ??? TODO
	 * @param indent
	 *            ??? TODO
	 */
	public final void printContent(final Element element, final String indent) {
		host.appendToReport(indent + element.eClass().getName() + ": ");
		if (!element.getAppliedStereotypes().isEmpty()) {
			host.appendToReport("<<");
			for (Stereotype st : element.getAppliedStereotypes()) {
				host.appendToReport(st.getName() + ",");
			}
			host.appendToReport(">> ");
		}
		if (element instanceof NamedElement) {
			NamedElement namedElement = (NamedElement) element;
			host.appendToReport(namedElement.getName());
		}
		host.appendLineToReport("");
		for (Element child : element.allOwnedElements()) {
			printContent(child, indent + "  ");
		}
	}
	
	//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		public ArrayList<AnalyserResult> getResultAr() {
			return resultAr; 
		}
		public Resource getCurrentModel() {
			return currentModel;
		}
		public ArrayList<String> getActivityNames(){
			return entityNames;
		}
		public ProcessDescription getDescription(){
			return pd;
		}
		public static Check getInstance(){
			return instance;
		}
		public Boolean getSynonyms(){
			return synonymsOnly;
		}
		public Boolean getAppend(){
			return append;
		}
		public File getOntology(){
			return ontologyFile;
		}
		public File getStopwords(){
			return stopwordFile;
		}
	//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		
		/**
		 * registers the observer that is stored in the  activator-instance.
		 * This is a workaround because the this check isn't there  when the riskfinderGUI starts. With this workaround,
		 * the riskfinderGUI can now be an observer in this check.
		 */
		private void register() {
			return;
//			Observer obs = Activator.getDefault().getRiskfinderGui();
//			if (obs != null) {
//				addObserver(obs);
//			}
		}
}
