package carisma.xutils.regulatory.importer.bsi;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.importer.bsi.datamodel.BSICategory;
import carisma.xutils.regulatory.importer.bsi.datamodel.BSIEntry;
import carisma.xutils.regulatory.importer.bsi.datamodel.DataRoot;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntry;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;

public class BSIExtractor {

	/** Name for the importer. */
	private static final String EXTRACTOR_NAME = "BSI-Extractor";
	/** reference to the logView output (GUI). */
	private LogViewEntrySet logInput;
	
	private DataRoot catalogueRoot;
		
	
	public BSIExtractor() {
		this(null);
	}
	
	public BSIExtractor(LogViewEntrySet logInput) {
		this.logInput = logInput;
	}
	
	public RegulatoryOntologyHelper extract(final File sourceFolder) {
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper();
		roh.createNewRegulatoryOntology();
		extract(sourceFolder, roh);
		return roh;			
	}
	
	public void extract(final File sourceFolder, final RegulatoryOntologyHelper ontologyContainer) {
		File bsiFolder = null;
		if (ontologyContainer == null) {
			outputMessage("ERROR", "No container for the regulatory ontology present!", 1);
			return;
		}
		if (sourceFolder == null || !sourceFolder.exists()) {
			outputMessage("ERROR", "No or invalid source folder given!", 1);
			return;
		}
		if (sourceFolder != null) {
			bsiFolder = new File(sourceFolder.getAbsolutePath() + File.separator);
		}
		outputMessage(EXTRACTOR_NAME, "BSI source path: " + bsiFolder.getAbsolutePath() + File.separator, 3);
		if (!bsiFolder.exists() || !bsiFolder.isDirectory()) {
			outputMessage("ERROR", "BSI source folder non-existent or not a directory!", 1);
			return;			
		}
		outputMessage(EXTRACTOR_NAME, "Start loading BSI catalogue", 3);
		if (!loadBsiCatalogue(bsiFolder)) {
			outputMessage("ERROR", "Cannot load the BSI-catalog! Did you extract "
					+ File.separator + "bsisource.7z properly?", 1);
			return;
		} else {	
			// start the bsi-importer with the imported bsi-catalog
			outputMessage(EXTRACTOR_NAME, "Bsi catalogue successfully loaded", 3);
			Map<String, OWLNamedIndividual> idEntryMapping = createBSIIndividuals(catalogueRoot, ontologyContainer);
			createRuleReferences(catalogueRoot, idEntryMapping, ontologyContainer);
		}		
	}
	
	/**
	 * Tries to automatically set a working proxy configuration for internet access.
	 * @return true, iff a connection could be established.
	 */
	private static boolean setProxyAuto() {
		URL url;
		try {
			url = new URL("http://www.google.de");
		} catch (MalformedURLException e1) {
			e1.printStackTrace();
			return false;
		}

		try {
			new Scanner(url.openStream()).useDelimiter("\\Z").next();
			return true;
		} catch (IOException e) {
			// System.out.println("No-proxy connection failed.");
		}

		try {
			System.setProperty("proxySet", "true");
			System.setProperty("proxyHost", "192.102.161.12");
			System.setProperty("proxyPort", "3128");

			new Scanner(url.openStream()).useDelimiter("\\Z").next();
			return true;
		} catch (IOException e) {
			System.out.println("Proxy 1 connection failed.");
		}

		return false;
	}
	
	public boolean loadBsiCatalogue(final File sourceFolder) {
		this.setCatalogueRoot(new DataRoot());
		BSICatalogueImporter bsiImporter = new BSICatalogueImporter(this.logInput); // FIXME 
//		BSIELCatalogueImporter bsiImporter = new BSIELCatalogueImporter(this.logInput);
		try {
			bsiImporter.doImport(sourceFolder);
		} catch (Exception e) {
			outputMessage("ERROR", "Import BSI Catalogue failed:", 1);
			e.printStackTrace();
			return false;
		}
		this.getCatalogueRoot().setBsiCatalogue(bsiImporter.getCurrentCatalogue());
		return true;
	}
	
	
	/**
	 * Parses the internal BSI catalogue structure and creates new BSI individuals in the ontology contained
	 * within the given regulatory helper.
	 * @param catalogueRoot - the BSI catalogue root
	 * @param ontologyContainer - the helper containing the ontology to adapt
	 * @return the mapping from ontology id's to the created BSI individuals
	 */
	public final Map<String, OWLNamedIndividual> createBSIIndividuals(final DataRoot catalogueRoot, final RegulatoryOntologyHelper ontologyContainer) {
		if (catalogueRoot == null || catalogueRoot.getBsiCatalogue() == null) {
			outputMessage("ERROR", "Given BSI catalogue root is invalid or empty.", 1);
			return null;
		}
		if (ontologyContainer == null) {
			outputMessage("ERROR", "Given RegulatoryOntologyHelper is null.", 1);
			return null;
		}
		Map<String, OWLNamedIndividual> idBsiEntryMapping = new HashMap<String, OWLNamedIndividual>();
		for (BSICategory curCat : catalogueRoot.getBsiCatalogue().getCategories()) {
			for (BSIEntry curEntry : curCat.getEntries()) {
				outputMessage(EXTRACTOR_NAME, "Konvertieren: " + curEntry.getId() + " : "	+ curEntry.getTitle(), 7);
				String newID = RegulatoryOntologyHelper.convertToValidOwlId(curEntry.getId());
				switch (curEntry.getTypeByIDPrefix()) {
				case element:
					OWLNamedIndividual bsielem = ontologyContainer.createBSIElement(newID, curEntry.getTitle(), curEntry.getText());
					idBsiEntryMapping.put(newID, bsielem);
					break;
				case threat:
					OWLNamedIndividual bsithreat = ontologyContainer.createBSIThreat(newID, curEntry.getTitle(), curEntry.getText());
					idBsiEntryMapping.put(newID, bsithreat);
					break;
				case measure:
					OWLNamedIndividual bsimeasure = ontologyContainer.createBSIMeasure(newID, curEntry.getTitle(), curEntry.getText());
					idBsiEntryMapping.put(newID, bsimeasure);
					break;
				default:
					outputMessage(EXTRACTOR_NAME, "WARNING: Unknown BSI entry type", 1);
					break;
				}
			}
		}
		return idBsiEntryMapping;
	}
	
	/**
	 * Creates the references between the elements and their corresponding threats and measures.
	 * @param catalogueRoot - the internal BSI catalogue structure
	 * @param idBsiEntryMapping - id<->BSI entry mapping for easy access
	 * @param ontologyContainer - the ontology container
	 */
	private void createRuleReferences(
			final DataRoot catalogueRoot, 
			final Map<String, OWLNamedIndividual> idBsiEntryMapping, 
			final RegulatoryOntologyHelper ontologyContainer) {
		for (BSICategory curCat : catalogueRoot.getBsiCatalogue().getCategories()) {
			for (BSIEntry curEntry : curCat.getEntries()) {
				for (String curRef : curEntry.getRefs()) {
					outputMessage(EXTRACTOR_NAME, "Entry " + curEntry.getId() + " hat Verweis auf " + curRef, 7);
					OWLNamedIndividual source = idBsiEntryMapping.get(RegulatoryOntologyHelper.convertToValidOwlId(curEntry.getId()));
					OWLNamedIndividual target = idBsiEntryMapping.get((RegulatoryOntologyHelper.convertToValidOwlId(curRef)));
					if (BSIEntry.getTypeByIDPrefix(curEntry.getId()) == BSIEntry.EntryType.element) {
						switch (BSIEntry.getTypeByIDPrefix(curRef)) {
						case threat:
							ontologyContainer.assignThreat(source, target);
							break;
						case measure:
							ontologyContainer.assignMeasure(source, target);
							break;
						}
					} else {
						ontologyContainer.createRuleReference(source, target);
					}
				}
			}
		}
	}
				

	public DataRoot getCatalogueRoot() {
		return catalogueRoot;
	}

	private void setCatalogueRoot(DataRoot newCatalogueRoot) {
		this.catalogueRoot = newCatalogueRoot;
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
	
	
	private void fuerThorsten() {
//		TODO: Auskommentierter code aus der Main-methode den Thorsten sortieren will
		
		
		// Tests f�r die create-Methoden f�r Regelelemente:
//		OWLNamedIndividual re = roh.createRE("id_dsfjlsjlf", "name des regelelements", "eine instanz der oberklasse regelelement");
//		OWLNamedIndividual process1 = roh.createREProcess("id_klsddflks", "name f�r ersten testprozess", "dieser prozess ist ganz wichtig");
//		OWLNamedIndividual process2 = roh.createREProcess("id_klsfsdfddflks", "name f�r zweiten testprozess", "dieser prozess ist ein teilprozess");
//		OWLNamedIndividual role1 = roh.createRERole("id_lsdfjlslfs", "test-rolle vorgesetzer", "eine rolle (vorgesetzter), die einen prozess ausf�hrt oder zumindest irgendwie daran beteiligt ist");
//		OWLNamedIndividual role2 = roh.createRERole("id_lsdfdsdfsjlslfs", "test-rolle", "eine rolle, die einen prozess ausf�hrt oder zumindest irgendwie daran beteiligt ist");
//		OWLNamedIndividual act1 = roh.createREActivity("id_sdlkjflsj", "�bergeordnete aktivit�t als regelelement", "irgendeine aktivit�t, die zu einem prozess geh�rt");
//		OWLNamedIndividual act2 = roh.createREActivity("id_sdlsfskjflsj", "teilaktivit�t als regelelement", "irgendeine aktivit�t, die teilaktivit�t in einem prozess ist");
//		OWLNamedIndividual obj = roh.createREObject("id_dksjflsjlfj", "ein objekt", "ein objekt das innerhalb eines prozesses ben�tigt/verwendet wird");
//		OWLNamedIndividual prop = roh.createREProperty("id_dlkjflsalf", "eine property", "propberty, analog zu sicherheitseigenschaften?");
//		
//		roh.createREReference(process1, process2, RegulatoryOntologyHelper.REL_SUBPROCESS);
//		roh.createREReference(process1, act1, RegulatoryOntologyHelper.REL_ACTIVITY);
//		roh.createREReference(process1, role1, RegulatoryOntologyHelper.REL_PARTICIPANT);
//		roh.createREReference(process1, obj, RegulatoryOntologyHelper.REL_UTILIZES);
//		roh.createREReference(process1, prop, RegulatoryOntologyHelper.REL_PROPERTY);
//		roh.createREReference(role1, role2, RegulatoryOntologyHelper.REL_SUPERIOR);
//		roh.createREReference(role1, act1, RegulatoryOntologyHelper.REL_PERFORMS);
//		roh.createREReference(role1, obj, RegulatoryOntologyHelper.REL_MANAGES);
//		roh.createREReference(act1, act2, RegulatoryOntologyHelper.REL_SUBACTIVITY);
//		roh.createREReference(act1, obj, RegulatoryOntologyHelper.REL_UTILIZES);
//		roh.createREReference(act1, prop, RegulatoryOntologyHelper.REL_PROPERTY);

		// Tests f?r die create-Methoden f?r Regelelemente:
//		OWLNamedIndividual re = roh.createRE("id_dsfjlsjlf", "name des regelelements", "eine instanz der oberklasse regelelement");
//		OWLNamedIndividual process1 = roh.createREProcess("id_klsddflks", "name f�r ersten testprozess", "dieser prozess ist ganz wichtig");
//		OWLNamedIndividual process2 = roh.createREProcess("id_klsfsdfddflks", "name f�r zweiten testprozess", "dieser prozess ist ein teilprozess");
//		OWLNamedIndividual role1 = roh.createRERole("id_lsdfjlslfs", "test-rolle vorgesetzer", "eine rolle (vorgesetzter), die einen prozess ausf�hrt oder zumindest irgendwie daran beteiligt ist");
//		OWLNamedIndividual role2 = roh.createRERole("id_lsdfdsdfsjlslfs", "test-rolle", "eine rolle, die einen prozess ausf�hrt oder zumindest irgendwie daran beteiligt ist");
//		OWLNamedIndividual act1 = roh.createREActivity("id_sdlkjflsj", "�bergeordnete aktivit�t als regelelement", "irgendeine aktivit�t, die zu einem prozess geh�rt");
//		OWLNamedIndividual act2 = roh.createREActivity("id_sdlsfskjflsj", "teilaktivit�t als regelelement", "irgendeine aktivit�t, die teilaktivit�t in einem prozess ist");
//		OWLNamedIndividual obj = roh.createREObject("id_dksjflsjlfj", "ein objekt", "ein objekt das innerhalb eines prozesses ben�tigt/verwendet wird");
//		OWLNamedIndividual prop = roh.createREProperty("id_dlkjflsalf", "eine property", "propberty, analog zu sicherheitseigenschaften?");
//		
//		roh.createREReference(process1, process2, RegulatoryOntologyHelper.REL_SUBPROCESS);
//		roh.createREReference(process1, act1, RegulatoryOntologyHelper.REL_ACTIVITY);
//		roh.createREReference(process1, role1, RegulatoryOntologyHelper.REL_PARTICIPANT);
//		roh.createREReference(process1, obj, RegulatoryOntologyHelper.REL_UTILIZES);
//		roh.createREReference(process1, prop, RegulatoryOntologyHelper.REL_PROPERTY);
//		roh.createREReference(role1, role2, RegulatoryOntologyHelper.REL_SUPERIOR);
//		roh.createREReference(role1, act1, RegulatoryOntologyHelper.REL_PERFORMS);
//		roh.createREReference(role1, obj, RegulatoryOntologyHelper.REL_MANAGES);
//		roh.createREReference(act1, act2, RegulatoryOntologyHelper.REL_SUBACTIVITY);
//		roh.createREReference(act1, obj, RegulatoryOntologyHelper.REL_UTILIZES);
//		roh.createREReference(act1, prop, RegulatoryOntologyHelper.REL_PROPERTY);

		// nur f�r Beispiele:
//		OWLNamedIndividual b1_10 = entryMap.get("B_1.10");
//		OWLNamedIndividual b1_9 = entryMap.get("B_1.9");
//		OWLNamedIndividual b1_15 = entryMap.get("B_1.15");
//		OWLNamedIndividual b1_4 = entryMap.get("B_1.4");
//		OWLNamedIndividual g5_2 = entryMap.get("G_5.2");
//		OWLNamedIndividual g5_64 = entryMap.get("G_5.64");
//		OWLNamedIndividual g5_11 = entryMap.get("G_5.11");
//		OWLNamedIndividual g5_128 = entryMap.get("G_5.128");
		
		
		// Beispiel f�r Methode I
		// jeweils ein Individuul f�r jedes Vorkommen eines Regelelements
//		OWLNamedIndividual id_SW_B1_10 = roh.createRESoftware("id_SW_B1_10", "Software B1.10", "Individuum, das Software repr�sentiert, die im Baustein 1.10 behandelt wird");
//		OWLNamedIndividual id_SW_B1_9 = roh.createRESoftware("id_SW_B1_9", "Software B1.9", "Individuum, das Software repr�sentiert, die im Baustein 1.9 behandelt wird");
//		OWLNamedIndividual id_DA_B1_15 = roh.createREData("id_DA_B1_15", "Daten B1.15", "Individuum, das Daten repr�sentiert, die im Baustein 1.15 behandelt wird");
//		OWLNamedIndividual id_DA_B1_4 = roh.createREData("id_DA_B1_4", "Daten B1.4", "Individuum, das Daten repr�sentiert, die im Baustein 1.4 behandelt wird");
//		
//		roh.createREReference(b1_10, id_SW_B1_10, roh.REL_OBJECT);
//		roh.createREReference(b1_9, id_SW_B1_9, roh.REL_OBJECT);
//		roh.createREReference(b1_15, id_DA_B1_15, roh.REL_OBJECT);
//		roh.createREReference(b1_4, id_DA_B1_4, roh.REL_OBJECT);
//
//		roh.createREReference(id_SW_B1_10, g5_2, roh.REL_THREAT);
//		roh.createREReference(id_SW_B1_10, g5_64, roh.REL_THREAT);
//		roh.createREReference(id_SW_B1_9, g5_2, roh.REL_THREAT);
//		roh.createREReference(id_SW_B1_9, g5_64, roh.REL_THREAT);
//		roh.createREReference(id_DA_B1_15, g5_64, roh.REL_THREAT);
//		roh.createREReference(id_DA_B1_15, g5_11, roh.REL_THREAT);
//		roh.createREReference(id_DA_B1_15, g5_128, roh.REL_THREAT);
//		roh.createREReference(id_DA_B1_4, g5_64, roh.REL_THREAT);
//		roh.createREReference(id_DA_B1_4, g5_11, roh.REL_THREAT);
//		roh.createREReference(id_DA_B1_4, g5_128, roh.REL_THREAT);
//

		
		
		// Beispiel f�r Methode II
		// pauschal erzeugte Individuel f�r RE-Klassen:
//		OWLNamedIndividual ind_sw = roh.createRESoftware("id_software", "I_" + roh.CLS_SW, "Repr�sentant f�r Klasse Software");
//		OWLNamedIndividual ind_data = roh.createREData("id_data", "I_" + roh.CLS_DATA, "Repr�sentant f�r Klasse Data");
//
//		
//		roh.createREReference(b1_10, ind_sw, roh.REL_OBJECT);
//		roh.createREReference(b1_9, ind_sw, roh.REL_OBJECT);
//		roh.createREReference(b1_15, ind_data, roh.REL_OBJECT);
//		roh.createREReference(b1_4, ind_data, roh.REL_OBJECT);
//		roh.createREReference(ind_sw, g5_2, roh.REL_THREAT);
//		roh.createREReference(ind_sw, g5_64, roh.REL_THREAT);
//		roh.createREReference(ind_data, g5_64, roh.REL_THREAT);
//		roh.createREReference(ind_data, g5_11, roh.REL_THREAT);
//		roh.createREReference(ind_data, g5_128, roh.REL_THREAT);
//
//		
//		

//		OWLNamedIndividual test1= goh.getIndividualByName("test");
//		OWLNamedIndividual ind1= goh.getIndividualByName("B_1.0");
//		
//		System.out.println(test1);
//		System.out.println(ind1);
//		
		
//		OWLNamedIndividual obj = roh.createREObject("id_dksjflsjlfj", "SAPSystem" ,"ein objekt", "ein objekt das innerhalb eines prozesses ben�tigt/verwendet wird");
//		roh.createREReference(ind1, obj, roh.REL_THREAT);
		
		
		
		
		
// kein debug mehr:		
		
		
		
		
		
		//***** Wenzel auskommentiert  ****//		
//		GenericOntologyHelper goh = new GenericOntologyHelper();
//		goh.createNewOntology(RegulatoryOntologyHelper.ONTOLOGY_IRI_BASE);
//		
//		RegulatoryOntologyHelper.initializeOntology(goh);
//		
//		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
//		
//		OWLNamedIndividual bdsg = roh.createLaw("BDSG", "Bundesdatenschutzgesetz");
//		
//		OWLNamedIndividual par1 = roh.createParagraph(bdsg, "1", "Ziel und Zweck des Gesetzes");
//		
//		OWLNamedIndividual sec1 = roh.createSection(par1, "1", "Alle Daten sind geheim. Sie dürfen nur von Sven gelesen werden.");
//		
//
//		
//		OWLNamedIndividual bsielem = roh.createBSIElement("B1", "Telefon", "Gerät zum Sprechen über große Entfernungen.");
//		OWLNamedIndividual bsithreat = roh.createBSIThreat("G1", "Klingeln", "Das Klingeln des Geräts kann erschrecken und das Sicherheitspersonal vom Monitor ablenken.");
//		OWLNamedIndividual bsimeasure = roh.createBSIElement("M1", "Hörer danebenlegen", "Wird der Hörer daneben gelegt kann das Telefon nicht mehr klingeln.");
//		roh.assignThreat(bsielem, bsithreat);
//		roh.assignMeasure(bsielem, bsimeasure);
//
//		goh.saveOntologyToFile(new File("/tmp/test.owl"));
//		
//		System.out.println("READ");
//		
//		Ontology loadedOnto = OWL2Ontology.loadFromFile(new File("/tmp/test.owl"));
//		for (Rule r : loadedOnto.getRules()) {
//			String n = r.getName();
//			System.out.println(r.getClass().getSimpleName()+": "+n);
//			if (r instanceof Section) {
//				Section s = (Section)r;
//				System.out.println("text "+s.getText());
//			}
//		}
		
		
	}
	
	/**
	 * Running this expects the BSI source files to be in "resources/bsisource"
	 * and outputs the ontology per default to "output/ontologies/Bsi_Catalogues.owl".
	 * @param args array where the first argument should be the source-folder, the second argument the filepath
	 * for the ontology
	 * @throws Exception 
	 */
//	TODO KR: JavaDoc anpassen, BSI source files Ordner wird doch nun mit angegeben, oder?
	public static void main(final String[] args) throws Exception {	
		String ontologySavepath = "";
		String sourceFolder = "";
		if (args.length != 2) {
			System.err.println("No path for ontology and/or no source folder given!");
			return;
		} else {
			sourceFolder = args[0];
			ontologySavepath = args[1];
		}
		System.out.println("Storing the Bsi ontology to " + ontologySavepath + "!");
		System.out.println("Source folder is " + sourceFolder);
		// mit altem Parser: Webseite -> altes Datenmodell
//		System.out.println("Set proxy automatically: " + BSIExtractor.setProxyAuto());
		
		BSIExtractor bsiExtractor = new BSIExtractor();
		
		RegulatoryOntologyHelper roh = bsiExtractor.extract(new File(sourceFolder));
		
		roh.saveOntologyToFile(new File(ontologySavepath));
		
		System.out.println("fertig");
		
	}

	
}
