package carisma.xutils.regulatory.importer.csv;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.ArrayList;
import com.csvreader.CsvReader;

import carisma.regulatory.ontology.utils.IndividualIDGenerator;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntry;
import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;

public class CSVExtractor {
	/** reference to the logView output (GUI) */
	private LogViewEntrySet logInput = null;
	private static final String EXTRACTOR_NAME = "CSVExtractor";
	
	/**
	 * the constructor of the CSVExtractor.
	 * @param logInput
	 */
	public CSVExtractor(final LogViewEntrySet logInput) {
		this.logInput = logInput;
	}

	/**
	 * extracts the csv content from the files in the given directory.
	 * @param csvFolder the directory
	 * @param roh the Regulatory Ontology Helper
	 */
	public void extract(final File csvFolder, final RegulatoryOntologyHelper roh) {
		File guidelineFolder = null;
		if (csvFolder != null) {
			guidelineFolder = csvFolder;
		} else {
			outputMessage("ERROR", "No source folder for Guideline Extractor given!", 1);
			return;
		}
		outputMessage(EXTRACTOR_NAME, "Guideline source path: " + guidelineFolder.getAbsolutePath() + File.separator, 1);		
		if (roh == null) {
			outputMessage(EXTRACTOR_NAME, "No RegulatoryHelper present!", 1);
			return;
		}
		if (!guidelineFolder.exists() || !guidelineFolder.isDirectory()) {
			outputMessage(EXTRACTOR_NAME, "Source folder non-existent or not a directory!", 1);
			return;			
		}
		parse(csvFolder, roh);
	}
	
	/**
	 * parses the csv files and computes the corresponding owl individual.
	 * @param csvFolder the directory
	 * @param roh the Regulatory Ontology Helper
	 */
	private void parse(final File csvFolder, final RegulatoryOntologyHelper roh) {
		for (File file : csvFolder.listFiles()) {
			ArrayList<String[]> listOfArrays = new ArrayList<String[]>();
			String document = "";
			String source = "";
			String requirements = "";
			int documentNumber = 0;
			int sourceNumber = 0;
			int requirementsNumber = 0;
			int keywordsNumber = 0;
			int restrictionsNumber = 0;
			int threatNumber = 0;
			int descriptionNumber = 0;
			int idNumber = 0;
			if (!file.getName().endsWith("csv")) {
				continue;
			}
			try {
					// use UTF8 encoding
				InputStreamReader inReader = new InputStreamReader(new FileInputStream(file), "UTF-8");
				CsvReader reader = new CsvReader(inReader, ';');
				while (reader.readRecord()) {
					listOfArrays.add(reader.getValues());
				}
				String[] firstString = listOfArrays.iterator().next();
					/*
					 * because the structure is not fix yet. 
					 * check on which position the elements resist in the array
					 */
				for (int i = 0; i < firstString.length; i++) {
					// check on which places the 
					if (firstString[i].contains("Dokument")) {
						documentNumber = i;
					} else if (firstString[i].contains("Quelle")) {
						sourceNumber = i;
					} else if (firstString[i].contains("Anforderung")) {
						requirementsNumber = i;
					} else if (firstString[i].contains("Keywords")) {
						keywordsNumber = i;
					} else if (firstString[i].contains("Einschränkungen")) {
						restrictionsNumber = i;
					} else if (firstString[i].contains("Gefährdung")) {
						threatNumber = i;
					} else if (firstString[i].contains("Beschreibung")) {
						descriptionNumber = i;
					} else if (firstString[i].contains("Nr./ID")) {
						idNumber = i;
					}
				}
				if (documentNumber == 0 || sourceNumber == 0) {
					outputMessage("ERROR", "Failed to read the column of the csv file.", 0);
				}
					// fill the empty columns with the corresponding values
				for (String[] str: listOfArrays) {
					if (str[documentNumber].equals("")) {
						str[documentNumber] = document;
					} else {
						document = str[documentNumber];
					}
					if (str[sourceNumber].equals("")) {
						str[sourceNumber] = source;
					} else {
						source = str[sourceNumber];
					}
					if (str[requirementsNumber].equals("")) {
						str[requirementsNumber] = requirements;
					} else {
						requirements = str[requirementsNumber];
					}
				}
					// remove the first row
				if (listOfArrays.iterator().next()[0].contains("Nr./ID")) {
					listOfArrays.remove(0);
				}
				// iterate over the elements and create new members in the Ontology
				for (String[] str : listOfArrays) {
					StringBuffer text = new StringBuffer("Anforderung\n");
					text.append(str[requirementsNumber] + "\n\n");
					text.append("Einschränkungen\n");
					text.append(str[restrictionsNumber] + "\n\n");
					text.append("Beschreibung\n");
					text.append(str[descriptionNumber]);
					roh.createGuidelinesEntry(
							IndividualIDGenerator.generateGuidelineEntryID(str[documentNumber],
									str[sourceNumber], str[idNumber]), // the individual id
							str[documentNumber] + " " + str[sourceNumber], 		// the number inside the document
							str[sourceNumber] + " " + str[threatNumber], 		// the title
							text.toString(),   		// the text
							str[keywordsNumber]);	// the keywords
				}
				outputMessage(EXTRACTOR_NAME, "Succesfully extracted " + listOfArrays.size()
						+ " Elements from the " + listOfArrays.iterator().next()[documentNumber], 0);
			} catch (IOException e) {
				outputMessage("ERROR", e.getMessage(), 0);
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
	
//	public static void main(String[] args) {
//		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper();
//		CSVExtractor importer = new CSVExtractor(new LogViewEntrySet(null, false, null, null));
//		importer.extract(new File("resources"), roh);
//	}
}
