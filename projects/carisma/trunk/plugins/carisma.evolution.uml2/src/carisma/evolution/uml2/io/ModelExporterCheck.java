package carisma.evolution.uml2.io;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.uml2.uml.Model;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.Delta;
import carisma.evolution.DeltaList;
import carisma.evolution.uml2.ModifierMap;
import carisma.evolution.uml2.io.datatype.ExportAddElement;
import carisma.evolution.uml2.io.datatype.ExportCopyElement;
import carisma.evolution.uml2.io.datatype.ExportDelElement;
import carisma.evolution.uml2.io.datatype.ExportDelta;
import carisma.evolution.uml2.io.datatype.ExportDeltaElement;
import carisma.evolution.uml2.io.datatype.ExportEditElement;
import carisma.evolution.uml2.io.datatype.ExportSubstElement;


/** Check for Exporting Delta.
 */
public class ModelExporterCheck implements CarismaCheck {
	
	/** Name of the Register key where the DeltaList is stored.
	 */
	public static final String DELTAS_REGISTER_KEY = "carisma.data.evolution.deltas";

	/** Name of the Register key where the ModifierMap is stored.
	 */
	public static final String MODIFIERS_REGISTRY_KEY = "carisma.data.evolution.modifiers";
	
	/** The AnalysisHost.
	 */
	private AnalysisHost host = null;
	
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters,
			final AnalysisHost newHost) {
		ModifierMap deltaModifiers = null;
		DeltaList deltaList = null;
		File outputFolder = null;
		host = newHost;
		FolderParameter outputFolderParameter = (FolderParameter) parameters.get("carisma.check.modelexporter.outputfolder");
		BooleanParameter onlyMaxSuccessfulParameter = (BooleanParameter) parameters.get("carisma.check.modelexporter.onlyMaxSuccessfulDeltas");
		if (outputFolderParameter == null) {
			return false;
		}
		boolean onlyMaxSuccessfulDeltas = onlyMaxSuccessfulParameter.getValue();
		outputFolder = outputFolderParameter.getValue();
		try {
			deltaList = (DeltaList) host.getFromRegister(DELTAS_REGISTER_KEY);
			deltaModifiers = (ModifierMap) host.getFromRegister(MODIFIERS_REGISTRY_KEY);
		} catch (RegisterNotInUseException e) {
			Logger.log(LogLevel.ERROR, "", e);
			return false;
		}
		if (deltaList == null || deltaModifiers == null) {
			return false;
		}
		List<Delta> deltasToExport = new ArrayList<Delta>();
		if (onlyMaxSuccessfulDeltas) {
			for (Delta d : deltaList.getRemainingDeltas()) {
				if (d.getNumberOfUsedChanges() == deltaList.getHighestChangeCountNow()) {
					deltasToExport.add(d);
				}
			}
			if (deltasToExport.isEmpty()) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "No successful Deltas left to export."));
			} else {
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Exporting " + deltasToExport.size() + " maximum successful Delta(s)."));
			}
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Exporting all successful Deltas (" + deltaList.remainingSize() + " Delta(s))."));
			deltasToExport.addAll(deltaList.getRemainingDeltas());
		}
		ModelExporter exporter = new ModelExporter();
		int fileCounter = 1;
		for (Delta d : deltasToExport) {
			String deltaFilename = host.getCurrentModelFilename().replaceAll("\\.uml$", "_delta" + fileCounter + ".xml");
			File deltaFile = new File(outputFolder.getAbsolutePath() + File.separator + deltaFilename);
			if (exporter.writeDeltaToFile(deltaFile, d, host.getAnalyzedModel())) {
				host.addResultMessage(new AnalysisResultMessage(
						StatusType.INFO, "Exported successful (" + d.getNumberOfUsedChanges() + " Changes) Delta " + deltaFilename + "."));
				
			} else {					
				host.addResultMessage(new AnalysisResultMessage(
						StatusType.ERROR, "Something went wrong while exporting Delta " + deltaFilename + "."));					
			}
			String modifiedModelFilename = host.getCurrentModelFilename().replaceAll("\\.uml$", "_modified" + fileCounter + ".uml");
			System.out.println(modifiedModelFilename);
			Model modifiedModel = deltaModifiers.get(d).getModifiedModel();
			System.out.println(modifiedModel.eResource().getURI().toString());
			deltaModifiers.get(d).saveModel(modifiedModel, outputFolder, modifiedModelFilename, true);
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Exported modified model " + modifiedModelFilename + "."));
			createReport(exporter.generateXMLOutput(d, host.getAnalyzedModel()));
			fileCounter++;
		}
		return true;
	}
	

	/** Creating the Report for this Check.
	 * @param deltaToExport The Delta which is exported.
	 */
	private void createReport(final ExportDelta deltaToExport) {
		host.appendLineToReport("It contains " + deltaToExport.getUsedChangesID().size() + " change(s)");
		for (String changeID : deltaToExport.getUsedChangesID()) {
			host.appendLineToReport("- " + changeID);
		}
		host.appendLineToReport("Applying the Delta has evolved the model as follows:");
		
		for (ExportDeltaElement deltaEle : deltaToExport.getContent()) {
			if (deltaEle instanceof ExportAddElement) {
				host.appendLineToReport(((ExportAddElement) deltaEle).toString());
			} else if (deltaEle instanceof ExportCopyElement) {
				host.appendLineToReport(((ExportCopyElement) deltaEle).toString());
			} else if (deltaEle instanceof ExportDelElement) {
				host.appendLineToReport(((ExportDelElement) deltaEle).toString());
			} else if (deltaEle instanceof ExportEditElement) {
				host.appendLineToReport(((ExportEditElement) deltaEle).toString());
			} else if (deltaEle instanceof ExportSubstElement) {
				host.appendLineToReport(((ExportSubstElement) deltaEle).toString());
			} else {
				host.appendLineToReport("Unsupported DeltaElement");
			}
		}
	}
}
