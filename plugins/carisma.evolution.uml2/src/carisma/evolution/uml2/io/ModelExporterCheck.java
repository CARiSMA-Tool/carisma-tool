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
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.Delta;
import carisma.evolution.DeltaList;
import carisma.evolution.uml2.ModifierMap;
import carisma.evolution.uml2.UMLModifier;
import carisma.evolution.uml2.io.datatype.ExportAddElement;
import carisma.evolution.uml2.io.datatype.ExportCopyElement;
import carisma.evolution.uml2.io.datatype.ExportDelElement;
import carisma.evolution.uml2.io.datatype.ExportDelta;
import carisma.evolution.uml2.io.datatype.ExportDeltaElement;
import carisma.evolution.uml2.io.datatype.ExportEditElement;
import carisma.evolution.uml2.io.datatype.ExportSubstElement;


/** Check for Exporting Delta.
 */
public class ModelExporterCheck implements CarismaCheckWithID {
	
	public static final String CHECK_ID = "carisma.evolution.io.ModelExporterCheck";
	
	public static final String PARAM_MODELEXPORTER_ONLY_MAX_SUCCESSFUL_DELTAS = "carisma.check.modelexporter.onlyMaxSuccessfulDeltas";

	public static final String PARAM_MODELEXPORTER_OUTPUTFOLDER = "carisma.check.modelexporter.outputfolder";

	/** Name of the Register key where the DeltaList is stored.
	 */
	public static final String PRECONDITION_DELTAS_REGISTER_KEY = "carisma.data.evolution.deltas";

	/** Name of the Register key where the ModifierMap is stored.
	 */
	public static final String PRECONDITION_MODIFIERS_REGISTRY_KEY = "carisma.data.evolution.modifiers";
	
	public static final String CHECK_NAME = "Export of Modified Models and Deltas";
	
	/** The AnalysisHost.
	 */
	private AnalysisHost host = null;
	
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters,
			final AnalysisHost newHost) {
		ModifierMap deltaModifiers = null;
		DeltaList deltaList = null;
		File outputFolder = null;
		this.host = newHost;
		FolderParameter outputFolderParameter = (FolderParameter) parameters.get(PARAM_MODELEXPORTER_OUTPUTFOLDER);
		BooleanParameter onlyMaxSuccessfulParameter = (BooleanParameter) parameters.get(PARAM_MODELEXPORTER_ONLY_MAX_SUCCESSFUL_DELTAS);
		if (outputFolderParameter == null) {
			return false;
		}
		boolean onlyMaxSuccessfulDeltas = onlyMaxSuccessfulParameter.getValue();
		outputFolder = outputFolderParameter.getValue();
		try {
			deltaList = (DeltaList) this.host.getFromRegister(PRECONDITION_DELTAS_REGISTER_KEY);
			deltaModifiers = (ModifierMap) this.host.getFromRegister(PRECONDITION_MODIFIERS_REGISTRY_KEY);
		} catch (RegisterNotInUseException e) {
			Logger.log(LogLevel.ERROR, "", e);
			return false;
		}
		if (deltaList == null || deltaModifiers == null) {
			return false;
		}
		List<Delta> deltasToExport = new ArrayList<>();
		if (onlyMaxSuccessfulDeltas) {
			for (Delta d : deltaList.getRemainingDeltas()) {
				if (d.getNumberOfUsedChanges() == deltaList.getHighestChangeCountNow()) {
					deltasToExport.add(d);
				}
			}
			if (deltasToExport.isEmpty()) {
				this.host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "No successful Deltas left to export."));
			} else {
				this.host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Exporting " + deltasToExport.size() + " maximum successful Delta(s)."));
			}
		} else {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Exporting all successful Deltas (" + deltaList.remainingSize() + " Delta(s))."));
			deltasToExport.addAll(deltaList.getRemainingDeltas());
		}
		int fileCounter = 1;
		for (Delta d : deltasToExport) {
			String deltaFilename = this.host.getCurrentModelFilename().replaceAll("\\.uml$", "_delta" + fileCounter + ".xml");
			File deltaFile = new File(outputFolder.getAbsolutePath() + File.separator + deltaFilename);
			if (ModelExporter.writeDeltaToFile(deltaFile, d, this.host.getAnalyzedModel())) {
				this.host.addResultMessage(new AnalysisResultMessage(
						StatusType.INFO, "Exported successful (" + d.getNumberOfUsedChanges() + " Changes) Delta " + deltaFilename + "."));
				
			} else {					
				this.host.addResultMessage(new AnalysisResultMessage(
						StatusType.ERROR, "Something went wrong while exporting Delta " + deltaFilename + "."));					
			}
			String modifiedModelFilename = this.host.getCurrentModelFilename().replaceAll("\\.uml$", "_modified" + fileCounter + ".uml");
			System.out.println(modifiedModelFilename);
			Model modifiedModel = deltaModifiers.get(d).getModifiedModel();
			System.out.println(modifiedModel.eResource().getURI().toString());
			deltaModifiers.get(d);
			UMLModifier.saveModel(modifiedModel, outputFolder, modifiedModelFilename, true);
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Exported modified model " + modifiedModelFilename + "."));
			createReport(ModelExporter.generateXMLOutput(d, this.host.getAnalyzedModel()));
			fileCounter++;
		}
		return true;
	}
	

	/** Creating the Report for this Check.
	 * @param deltaToExport The Delta which is exported.
	 */
	private void createReport(final ExportDelta deltaToExport) {
		this.host.appendLineToReport("It contains " + deltaToExport.getUsedChangesID().size() + " change(s)");
		for (String changeID : deltaToExport.getUsedChangesID()) {
			this.host.appendLineToReport("- " + changeID);
		}
		this.host.appendLineToReport("Applying the Delta has evolved the model as follows:");
		
		for (ExportDeltaElement deltaEle : deltaToExport.getContent()) {
			if (deltaEle instanceof ExportAddElement) {
				this.host.appendLineToReport(((ExportAddElement) deltaEle).toString());
			} else if (deltaEle instanceof ExportCopyElement) {
				this.host.appendLineToReport(((ExportCopyElement) deltaEle).toString());
			} else if (deltaEle instanceof ExportDelElement) {
				this.host.appendLineToReport(((ExportDelElement) deltaEle).toString());
			} else if (deltaEle instanceof ExportEditElement) {
				this.host.appendLineToReport(((ExportEditElement) deltaEle).toString());
			} else if (deltaEle instanceof ExportSubstElement) {
				this.host.appendLineToReport(((ExportSubstElement) deltaEle).toString());
			} else {
				this.host.appendLineToReport("Unsupported DeltaElement");
			}
		}
	}


	@Override
	public String getCheckID() {
		return CHECK_ID;
	}

	@Override
	public String getName() {
		return CHECK_NAME;
	}
}
