package carisma.check.umlchange.efficiency;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.FloatParameter;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.IntegerParameter;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CarismaCheck;
import carisma.check.smartcard.evolution.lockedstatuscheck.LockedStatusEvolutionDeltaOnlyCheck;
import carisma.evolution.DeltaFactoryCheck;
import carisma.evolution.uml2.UMLModifierCheck;
import carisma.evolution.uml2.io.ModelExporterCheck;
import carisma.evolution.uml2.umlchange.UMLchangeParserCheck;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.check.smartcard.lockedstatus.Check;



/**
 * Tests the efficiency of the Evolution-aware Locked Status test.
 * @author Klaus Rudack
 *
 */
public class UMLchangeEfficiencyTestLockedStatus implements CarismaCheck {

	/**
	 * host for reports.
	 */
	private AnalysisHost host;
	
	/**
	 * path to the "tmp" folder as string.
	 */
	private String pathToTmpFolder = System.getProperty("java.io.tmpdir") + System.getProperty("file.separator") + "carisma";
	
	/**
	 * path to the "tmp" folder as {@link File}.
	 */
	private File folderFile = new File(pathToTmpFolder);
	
	/**
	 * integer to save the amount of models for the nonEvolution check.
	 */
	private int modelAmount = 0;
	
	/**
	 * key of the percentage parameter.
	 */
	private String percentage = "carisma.check.umlchange.efficiency.changepercent";
	
	/**
	 * key of the paths parameter.
	 */
	private String paths = "carisma.check.umlchange.efficiency.paths";
	
	/**
	 * variable to save additional outputs.
	 */
	private String messages = "";
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		this.host = host;
		TestingHost changeHost = new TestingHost(false);
		fillExportParameters(parameters);
		initHost(parameters, changeHost);
		folderFile.mkdir();
		String buffer = "";
		if (host != null) {
			this.host = host;
		} else {
			this.host = new TestingHost(true);
		}
		UMLchangeParserCheck parser = new UMLchangeParserCheck();
		long time = System.currentTimeMillis();
		parser.perform(parameters, changeHost);
		time = System.currentTimeMillis() - time;
		parser = null;
		buffer += "UMLchangeParser last " + time + " millisecs\n";
		DeltaFactoryCheck dfc = new DeltaFactoryCheck();
		time = System.currentTimeMillis();
		dfc.perform(parameters, changeHost);
		time = System.currentTimeMillis() - time;
		dfc = null;
		buffer += "DeltaFactoryCheck last " + time + " millisecs\n";
		UMLModifierCheck umlmc = new UMLModifierCheck();
		time = System.currentTimeMillis();
		umlmc.perform(parameters, changeHost);
		time = System.currentTimeMillis() - time;
		umlmc = null;
		buffer += "UMLModifier last " + time + " millisecs\n";
		LockedStatusEvolutionDeltaOnlyCheck lset = new LockedStatusEvolutionDeltaOnlyCheck(false);
		time = System.currentTimeMillis();
		lset.perform(parameters, changeHost); //Evolution aware test
		time = System.currentTimeMillis() - time;
		buffer += "Evolution aware check last " + time + " millisecs\n";
		ModelExporterCheck mec = new ModelExporterCheck();
		time = System.currentTimeMillis();
		mec.perform(parameters, changeHost);
		time = System.currentTimeMillis() - time;
		buffer += "ModelExporter last " + time + " millisecs\n";
		mec = null;
//		changeHost.delete(); //TODO mit changes = 2 und max deltas = true kommt eine exception
		changeHost = null;
		time = nonEvolutionCheck();
		buffer += "Non-Evolution check last " + time + " millisecs\n";
		buffer += modelAmount + " models have been analyzed";
		host.appendLineToReport(messages);
		host.appendLineToReport(buffer);
		return true;
	}
	
	/**
	 * performs the nonEvolution checks.
	 * this checks loads all the UML files in the "tmp" directory and performs a locked-status check for each,
	 * also counting the time needed for the checks
	 * @return the time needed for the checks in millisecs
	 */
	private long nonEvolutionCheck() {
		long completeTime = 0;
		for (File f : folderFile.listFiles()) {
			if (f.getAbsolutePath().endsWith(".uml")) {
				long time;
				modelAmount++;
				Check check  = new Check();  //locked-status check
				Resource res = loadModel(f.getAbsolutePath());
				TestingHost h = new TestingHost(false);
				h.setAnalyzedModel(res);
				time = System.currentTimeMillis();
				check.perform(null, h);
				time = System.currentTimeMillis() - time;
				completeTime += time;
				try {
					res.delete(null);
				} catch (IOException e) {
					messages += "Could not load model " + f.getAbsolutePath() + "\n!"; //kann eigentlich nicht passieren
					e.printStackTrace();
				}
				h.delete();
				h = null;
				check = null;
			}
			f.delete();
		}
		folderFile.delete();
		return completeTime;
	}
	
	/**
	 * fills the parameter map for the modelexporter.
	 * @param parameters parameters of the check.
	 */
	private void fillExportParameters(final Map<String, CheckParameter> parameters) {
		Boolean maxDelta = true;
		String maxDeltaString = "carisma.check.umlchange.efficiency.maxdelta";
		String folder = "carisma.check.modelexporter.outputfolder";
		String  notAll = "carisma.check.modelexporter.onlyMaxSuccessfulDeltas";
		CheckParameter maxDeltaParameter = parameters.get(maxDeltaString);
		if (maxDeltaParameter != null) {
			if (maxDeltaParameter instanceof BooleanParameter) {
				maxDelta = ((BooleanParameter) maxDeltaParameter).getValue();
			} else {
				messages += "Wrong parameter type for the maxDeltas!\n";
			}
		} else {
			messages += "Missing value for maxDeltas!\n";
		}
		FolderParameter fp = new FolderParameter(null, folderFile);
		BooleanParameter  bp = new BooleanParameter(null, maxDelta);
		parameters.put(folder, fp);
		parameters.put(notAll, bp);
	}
	
	
	
	/**
	 * method to load a model from an UML file.
	 * @param filePath the path to the UML file
	 * @return {@link Resource} object of the model
	 */
	public final Resource loadModel(final String filePath) {
		UML2ModelLoader ml = new UML2ModelLoader();
		File file = new File(filePath);
		Resource returnValue = null;
		try {
			returnValue = ml.load(file);
		} catch (IOException e) {
			host.appendLineToReport("Fail to open " + file.getAbsolutePath());
		}
		return returnValue;
	}
	
	/**
	 * inits the  given host with a model according to the given parameters.
	 * default values: 100 paths and 10 percentage of paths with UMLChange stereotypes.
	 * @param parameters {@link CheckParameter} map with parameters for the model in it.
	 * @param h {@link TestingHost} to initialize
	 */
	private void initHost(final Map<String, CheckParameter> parameters, final TestingHost h) {
		CheckParameter percentageParameter = parameters.get(percentage);
		CheckParameter pathsParameter = parameters.get(paths);
		float percentValue = 10;
		int pathValue = 100;
		Resource rs = null;
		if (percentageParameter != null) {
			if (percentageParameter instanceof FloatParameter) {
				percentValue = ((FloatParameter) percentageParameter).getValue();
			} else {
				messages += "Wrong parameter type for the percentages of paths with UMLchange stereotypes!\n";
			}
		} else {
			messages += "Missing value for the percentages of paths with UMLchange stereotypes!\n";
		}
		if (pathsParameter != null) {
			if (pathsParameter instanceof IntegerParameter) {
				pathValue = ((IntegerParameter) pathsParameter).getValue();
			} else {
				messages += "Wrong parameter type for the ammounts of paths!\n";
			}
		} else {
			messages += "Missing value for the ammounts of paths!\n";
		}
		LockedStatusModelCreater lsmc = new LockedStatusModelCreater();
		long  time = System.currentTimeMillis();
		rs = lsmc.getNewModel("TestModel", pathValue, percentValue);
		time = System.currentTimeMillis() - time;
		host.appendLineToReport("Modelcreation last " + time + " millisecs");
		h.setAnalyzedModel(rs);
	}

	

}