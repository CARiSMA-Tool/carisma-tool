package carisma.processanalysis.loader.misc;

import java.io.File;
import java.util.Map;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;

public class CSVImporterCheck implements CarismaCheck {

	@Override
	public boolean perform(Map<String, CheckParameter> parameters,
			AnalysisHost host) {
		
		File csvFile = ((InputFileParameter) parameters.get("carisma.processanalysis.loader.misc.csvfile")).getValue();
		
		host.appendLineToReport("Loading process file: " + csvFile.getAbsolutePath());
		
		CSVImporter importer = new CSVImporter(csvFile.getAbsolutePath(), ',');
		ProcessDescription processDescription = importer.doImport();
		
		
		try {
			host.putToRegister(ProcessDescription.CARISMA_REGISTRY_KEY, processDescription);
		} catch (RegisterInUseException e) {
			host.displayError("There is already a model to be analysed. Have you used two model importer plugins?");
			return false;
		}
		
		host.appendLineToReport("Successfully loaded process with " + processDescription.getEntities().size() + " entities into registry.");
		
		return true;
	}

}
