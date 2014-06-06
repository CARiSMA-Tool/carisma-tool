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
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Stereotype;

import carisma.check.processanalysis.texttools.wortschatz.WSExpander;
import carisma.check.processanalysis.texttools.wortschatz.WrongElementException;
import carisma.check.riskfinder.PatternCatalog.BSIOnto;
import carisma.check.riskfinder.PatternCatalog.LAWOnto;
import carisma.check.riskfinder.PatternCatalog.MARiskOnto;
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


/**
 * Main class for
 * 
 * @author thumberg
 * 
 */

public class Check implements CarismaCheck {

	/** The host of the check. **/
	private AnalysisHost host;

	private boolean synonymsOnly = false;
	private boolean append = true;
	private File stopwordFile = null;
	private File ontologyFile = null;
	private File xmlResultFile = null;
//	TODO KR: hier müssen auch noch die Enumeration für die ontology-elemente stehen.
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
		this.host = host;

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
		
		Resource currentModel = host.getAnalyzedModel();

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
			//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
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
		catalog.createFromOntology(ontologyFile.getAbsolutePath(), BSIOnto.ALL, MARiskOnto.CLAUSE, LAWOnto.All);
//		TODO KR: hier muessen dann die enumeration ermittelt werden

		// load stop words
		StopwordFilter stopwords = new StopwordFilter();		
		stopwords.loadFromFile(stopwordFile.getAbsolutePath());

		ArrayList<StringFilter> filters = new ArrayList<StringFilter>();
		filters.add(stopwords);

		// start analysis
		PeschkeAnalyser analyser = new PeschkeAnalyser();
		WSExpander expander;
		try {
			expander = new WSExpander(WSExpander.ExpanderMode.USE_CACHE_AND_NET);
//			expander = new WSExpander(WSExpander.USE_ONLY_CACHE);
			expander.calcExtensions(processDescription, synonymsOnly, append);
			expander.storeCache();
		} catch (WrongElementException e) {
			e.printStackTrace();
		}
		AnalyserResult result;

		

		//prepare results, start with a short overview for the user 
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO,
				"Finished checking of " + processDescription.getEntities().size() + " entities. See report for details."));
		
		int yellowThreshold = 200;
		int redThreshold = 500;
		
		if (writeback) {
			YaoqiangHelper.clearAllWarningAndInfoFlags(currentModel.getContents().get(0));
		}
		
		/// XML-Export
		String xmlResultComplete = ""; // xml-String mit den gleichen Informationen, die auch im report stehen 
		xmlResultComplete += "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n";
		xmlResultComplete += "<RiskfinderResult ";
		xmlResultComplete += "writeback=\"" + writeback;
		xmlResultComplete += "\"  append=\"" + append;
		xmlResultComplete += "\"  stopwords=\"" + stopwordFile;
		xmlResultComplete += "\"  synonymsOnly=\"" + synonymsOnly;
		xmlResultComplete += "\"  ontology=\"" + ontologyFile;
		xmlResultComplete += "\"  minScore=\"" + minScore;		
		xmlResultComplete += "\" >\n";
		String xmlSimpleResult = ""; // xml-String mit vereinfachtem Format, s. Ticket #1138
		xmlSimpleResult += "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n";
		xmlSimpleResult += "<threats>\n";
		
		//then gather detailed results
		for (ProcessEntity curEntity : processDescription.getEntities()) {	
			
			//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			result = analyser.getRelevantPatterns(catalog, curEntity, filters);
			resultAr.add(result);
			entityNames.add(curEntity.getTexts().get(0).getEntityText());
		//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			
			// Für Report
			result = analyser.getRelevantPatterns(catalog, curEntity, filters, minScore);
//			/*debug:*/ host.appendLineToReport("Score: " + result.getScore_method1(3));
			host.appendLineToReport("[" + curEntity.getTexts().get(0).getEntityText() + " (" + ((int) result.getScore_method1(3)) + ")]");
			host.appendLineToReport(result.getAllRelevantWords().size() + " Relevante Worte: " + result.getAllRelevantWordsSorted());
			host.appendLineToReport(result.size() + " relevante Pattern:");
			host.appendLineToReport(result.toPrettyString("    "));
			
			xmlResultComplete += result.toXMLString();

			xmlSimpleResult += result.toSimpleXMLString();

			// Einfärben im Modell:
			if (writeback) {
				double curScore = result.getScore_method1(3);
				if (curScore >= redThreshold) {
					YaoqiangHelper.setWarningFlagToBpmn2Element((org.eclipse.bpmn2.impl.TaskImpl) curEntity.getObject(), null, Color.red);
				} else if (curScore >= yellowThreshold) {
					YaoqiangHelper.setWarningFlagToBpmn2Element((org.eclipse.bpmn2.impl.TaskImpl) curEntity.getObject(), null, Color.yellow);
				}
			}
		}
		
		xmlResultComplete += "</RiskfinderResult>";
		xmlSimpleResult += "</threats>";

		//System.out.println(xmlResultComplete);
		// XML-Datei schreiben:
		if (xmlResultFile != null) {
			try {
				Writer fw = new FileWriter(xmlResultFile);
				//fw.write(xmlResultComplete);
				fw.write(xmlSimpleResult);
						fw.close();
			} catch (IOException e) {
				System.err.println("Konnte Datei nicht erstellen");
			} 
		}

		// visualize results in diagram, or write it as text to the console
		if (writeback) {
			try {
				currentModel.save(Collections.EMPTY_MAP);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

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
}
