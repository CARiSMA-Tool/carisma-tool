package carisma.xutils.regulatory.importer.juris;


import java.io.File;

import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntry;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;

/**
 * This class imports law texts from a given URL into an ontology file.
 * DW: This seems to be an (old?) standalone class that creates an ontology just
 * with the juris content.
 * @author wessel
 *
 */
public class JurisExtractor {
		/** Name for the importer */
	private static final String EXTRACTOR_NAME = "JurisExtractor";
//		/** Default subfolder path where the juris input is */
//	public static final String DEFAULT_JURIS_SUBFOLDER = "Gesetze" + File.separator;
//		/** Default path for the ontology file */
//	public static final String DEFAULT_JURIS_ONTOLOGY_SAVE_PATH = ExtractorConstants.DEFAULT_ONTOLOGY_OUTPUT_FOLDER + "Juris_Ontology.owl";
		/** Output for the GUI **/
	private LogViewEntrySet logInput = null;
	
	public JurisExtractor() {
		super();
	}
	
	public JurisExtractor(LogViewEntrySet logInput) {
		this();
		this.logInput = logInput;
	}
	
	public RegulatoryOntologyHelper extract(final File sourceFolder) {
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper();
		roh.createNewRegulatoryOntology();
		extract(sourceFolder, roh);
		return roh;			
	}
	
	public void extract(final File sourceFolder, final RegulatoryOntologyHelper ontologyContainer) {
		File jurisFolder = null;
		if (sourceFolder != null) {
			jurisFolder = sourceFolder;
		} else {
			outputMessage("ERROR", "No source folder for Juris Extractor given!", 1);
			return;
		}
		if (ontologyContainer == null) {
			outputMessage(EXTRACTOR_NAME, "No RegulatoryHelper present!", 1);
			return;
		}
		if (!jurisFolder.exists() || !jurisFolder.isDirectory()) {
			outputMessage(EXTRACTOR_NAME, "Juris source folder non-existent or not a directory!", 1);
			return;			
		}
		try {
			parse(jurisFolder, ontologyContainer);
		} catch (ImportException e) {
			outputMessage(EXTRACTOR_NAME, "Error extracting the ontology elements from the juris files.", 1);
		}
	}

	/**
	 * Parses the given URL, to retrieve all paragraphs from.
	 *
	 * @param jurisFolderPath The target URL given as {@link String} which text should be parsed. 
	 */
	public final void parse(final File jurisFolder, final RegulatoryOntologyHelper ontologyContainer) throws ImportException {
		boolean isFinished = false;
		outputMessage(EXTRACTOR_NAME, "Reading content of " + jurisFolder, 3);
		
		// start the SaxReader
		File dtdFolder = new File(jurisFolder.getParentFile().getAbsolutePath() + File.separator + "dtds" + File.separator);
//        FIXME KR: Ueberpruefen ob der dtdFolder existiert
		JurisFileParser jurisFileParser = new JurisFileParser(dtdFolder, logInput);
        for (File jurisFile : jurisFolder.listFiles()) {
        	if (jurisFile.getName().endsWith(".html")) {
				isFinished = jurisFileParser.parse(jurisFile, ontologyContainer);
        	} else {
        		outputMessage(EXTRACTOR_NAME, "File " + jurisFile + " is not an HTML file", 1);
        	}
        }
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
	 * @param args
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

		System.out.println("Starting parse process from " + sourceFolder + "...");
		JurisExtractor importer = new JurisExtractor();
		
		RegulatoryOntologyHelper roh = importer.extract(new File(sourceFolder));
		
		roh.saveOntologyToFile(new File(ontologySavepath));	
		
		System.out.println("Parse process ended normally");
		
//		System.out.println("Beispiel für MARisk...");
// FIXME: (DW) Why is this example using MARisk and not the juris part?
/*
		OWLNamedIndividual ind_c7 = importer.roh.createMARiskClause("MARiskVA_7", "7", "Elemente eines angmessensen Risikomanagements");
		OWLNamedIndividual ind_c7_1 = importer.roh.createMARiskClause("MARiskVA_7.1", "7.1", "Risikostrategie");
		OWLNamedIndividual ind_e7b1 = importer.roh.createMARiskBindingEntry("MARiskVA_7_B1", "1", "Unternehmen müssen ein Risikomanagement einrichten, welches die in § 64a Abs. 1 Satz 4 VAG genannten Elemente enthält.");
		OWLNamedIndividual ind_e7c1 = importer.roh.createMARiskCommentEntry("MARiskVA_7_C1", "1", "Der ganzheitliche Ansatz verlangt, dass die dem Gesamtrisikoprofil angemessene Risikostrategie...");
		OWLNamedIndividual ind_e7_1b1 = importer.roh.createMARiskBindingEntry("MARiskVA_7.1_B1", "1", "Die Festlegung der Geschäftsstrategie und der daraus abgeleiteten adäquaten Risikostrategie...");
		OWLNamedIndividual ind_e7_1c1 = importer.roh.createMARiskCommentEntry("MARiskVA_7.1_C1", "1", "Unter Geschäftsstrategie versteht die Aufsicht die geschäftspolitische Ausrichtung, ...");
		
		importer.roh.createMARiskSubClause(ind_c7, ind_c7_1);
		importer.roh.createMARiskEntry(ind_c7, ind_e7b1);
		importer.roh.createMARiskEntry(ind_c7, ind_e7c1);
		importer.roh.createMARiskEntry(ind_c7_1, ind_e7_1b1);
		importer.roh.createMARiskEntry(ind_c7_1, ind_e7_1c1);

		System.out.println("Writing ontology to " + ontologySavepath + "...");
		importer.goh.saveOntologyToFile(new File(ontologySavepath));
		System.out.println("Done.");
*/
	}
}