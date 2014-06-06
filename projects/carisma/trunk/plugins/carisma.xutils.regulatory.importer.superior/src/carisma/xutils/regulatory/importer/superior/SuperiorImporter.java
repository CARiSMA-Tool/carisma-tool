package carisma.xutils.regulatory.importer.superior;

import java.io.File;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.OntologyHelperException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.importer.bsi.BSIExtractor;
import carisma.xutils.regulatory.importer.juris.JurisExtractor;
import carisma.xutils.regulatory.importer.marisk.MaRiskExtractor;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntry;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;
import carisma.xutils.regulatory.importer.csv.*;

/**
 * This class parses the BDSG, BGB, BSI catalog and MARisk
 * for regulatory ontology individuals and stores them in one ontology.
 * @author dbuerger
 *
 */
public class SuperiorImporter {
	private static final String DEFAULT_MARISK_SUBFOLDER = "MaRisk"+System.getProperty("file.separator");
	private static final String DEFAULT_BSI_SUBFOLDER = "bsisource"+System.getProperty("file.separator");
	private static final String DEFAULT_JURIS_SUBFOLDER = "Gesetze"+System.getProperty("file.separator");
	private static final String DEFAULT_GUIDELINES_FOLDER = "Grundsaetze" + File.separator;
	
	/** Name for the main importer. */
	public static final String SUPERIOR_IMPORTER_NAME = "Main Extractor";
	private static final String CSVEXTRACTOR_NAME = "CSVExtractor";

	/** the regulatory ontology helper. */
	private RegulatoryOntologyHelper roh = null;
		/** the file path to store the ontology. */
	private String ontologyFilepath = "";

		/** reference to the logView output (GUI). */
	private LogViewEntrySet logInput = null;
		/** path to the input data. */
	private String resourceFolderPath = "";

	/**
	 * Creates a new ontology and prepares it for the import.
	 * Also initializes the various regulatory importers.
	 * Uses the standard path for the input.
	 * @param ontologyFilename the name of the file to store the ontology
	 */
	public SuperiorImporter(final String ontologyFilename) {
		this(null, null, ontologyFilename);
	}

	/**
	 * Creates a new ontology and prepares it for the import.
	 * Also initializes the various regulatory importers.
	 * Constructor is normally called from the corresponding GUI.
	 */
	public SuperiorImporter(final LogViewEntrySet logInput,
			final String newSourceFolderPath, final String ontologyFilename) {
		this.ontologyFilepath = ontologyFilename;
		this.logInput = logInput;
		if (newSourceFolderPath != null) {
			this.resourceFolderPath = newSourceFolderPath;
		} else {
			outputMessage("ERROR", "No source folder given!", 1);
		}
		roh = new RegulatoryOntologyHelper();
		roh.createNewRegulatoryOntology();
	}


	/**
	 * Parses the currently supported laws for individuals
	 * and stores the ontology under the given ontology file path.
	 * Parses without a GUI.
	 * @throws NoSuchPropertyException
	 */
	public final void createRegulatoryOntology() {
		if (!ontologyFilepath.endsWith(".owl")) {
			System.err.println("Wrong file format! This importer outputs an .owl ontology.");
			return;
		}
		outputMessage(SUPERIOR_IMPORTER_NAME, "Path to input data: " + this.resourceFolderPath, 4);
		outputMessage(SUPERIOR_IMPORTER_NAME, "Outputfile: " + this.ontologyFilepath, 4);
	
		parseMARisk();
		parseJurisLaws();
		parseBSI();
		parseGuidelines();
		//TODO: nach Demo entfernen
//				DemoConstraintCreator dcc = new DemoConstraintCreator();
//				dcc.createDemoStructure(roh);
				
		createRuleElementIndividuals();
		
	}
	
	/**
	 *Parses the BDSG and the BGB from the juris website for the corresponding ontology individuals.
	 */
	private void parseJurisLaws() {
		outputMessage(SUPERIOR_IMPORTER_NAME, "Start parsing juris html files.", 4);
		JurisExtractor extractor = new JurisExtractor(this.logInput);
		extractor.extract(new File(resourceFolderPath + DEFAULT_JURIS_SUBFOLDER), roh);
		outputMessage(SUPERIOR_IMPORTER_NAME, "Done parsing juris html files.", 4);
	}

	/**
	 * Parses the BSI catalog for BSI element individuals. 
	 */
	private void parseBSI() {
		outputMessage(SUPERIOR_IMPORTER_NAME, "Start parsing BSI files", 3);
		BSIExtractor bsiOntologyImporter = new BSIExtractor(logInput);
		bsiOntologyImporter.extract(new File(resourceFolderPath + DEFAULT_BSI_SUBFOLDER), roh);
		outputMessage(SUPERIOR_IMPORTER_NAME, "Done parsing BSI files", 3);
	}

	/**
	 * Parses the MARisk for the corresponding ontology individuals.
	 */
	private void parseMARisk() {
		/** the marisk-parser */
		outputMessage(SUPERIOR_IMPORTER_NAME, "Start parsing MARisk files...", 3);
		MaRiskExtractor mariskParser = new MaRiskExtractor(logInput);
		mariskParser.extract(new File(resourceFolderPath + DEFAULT_MARISK_SUBFOLDER), roh);
		outputMessage(SUPERIOR_IMPORTER_NAME, "Done parsing MARisk files...", 3);		
	}
	
	private void parseGuidelines() {
		outputMessage(CSVEXTRACTOR_NAME, "Start parsing Guidelines...");
		CSVExtractor csvExtractor = new CSVExtractor(logInput);
		csvExtractor.extract(new File(resourceFolderPath + DEFAULT_GUIDELINES_FOLDER), roh);
	}

	/**
	 * Create the rule element individuals by using the RuleElementCreator.
	 */
	private void createRuleElementIndividuals() {
		outputMessage(SUPERIOR_IMPORTER_NAME, "Start creating rule element individuals ...", 3);
		RuleElementsCreator rec = new RuleElementsCreator(roh, resourceFolderPath, logInput);
		try {
			rec.run();
		} catch (NoSuchPropertyException e) {
			outputMessage("ERROR", "ERROR! Couldn't find a property.", 1);
			e.printStackTrace();
		}
		outputMessage(SUPERIOR_IMPORTER_NAME, "Done creating rule element individuals ...", 3);
	}
	/**
	 * Saves the ontology using the given filepath.
	 * @param filepath - the path to save the ontology to
	 */
	public void saveOntology(final String filepath) {
		outputMessage(SUPERIOR_IMPORTER_NAME, "Saving ontology at '" + filepath + "'", 4);
		try {
			roh.saveOntologyToFile(new File(ontologyFilepath));
		} catch (OntologyHelperException e) {
			outputMessage(SUPERIOR_IMPORTER_NAME, "Failed to store the ontology ... (" + e.getLocalizedMessage() + ")", 1);
		} catch (NullPointerException e) {
			outputMessage(SUPERIOR_IMPORTER_NAME, "Error occured while saving to file " + filepath, 1);
		}
		outputMessage(SUPERIOR_IMPORTER_NAME, "Ontology successfully saved.", 3);		
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

	/**
	 * main.
	 * @param args array where the first argument should be the source-folder, the second argument the filepath
	 * for the ontology
	 * @throws NoSuchPropertyException
	 */
	public static void main(final String[] args) {
		String sourceFolder = "";
		String ontologySavepath = "";
		if (args.length != 2) {
			System.err.println("No path for ontology and/or no source folder given!");
			return;
		} else {
			sourceFolder = args[0];
			ontologySavepath = args[1];
		}

		SuperiorImporter importer = null;
		importer = new SuperiorImporter(null, sourceFolder, ontologySavepath);
		
		importer.createRegulatoryOntology();
		importer.saveOntology(ontologySavepath);
	}
}
