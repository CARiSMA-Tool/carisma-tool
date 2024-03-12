package carisma.check.umlchange.efficiency;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;

import carisma.check.smartcard.evolution.lockedstatuscheck.LockedStatusEvolutionDeltaOnlyCheck;
import carisma.check.smartcard.lockedstatus.LockedStatusCheck;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.FloatParameter;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.IntegerParameter;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.evolution.DeltaFactoryCheck;
import carisma.evolution.uml2.UMLModifierCheck;
import carisma.evolution.uml2.io.ModelExporterCheck;
import carisma.evolution.uml2.umlchange.UMLchangeParserCheck;

import java.util.logging.Logger;

/**
 * Tests the efficiency of the Evolution-aware Locked Status test.
 * @author Klaus Rudack
 *
 */
public class UMLchangeEfficiencyTestLockedStatus implements CarismaCheck {

	private static final Logger logger = Logger.getLogger(UMLchangeEfficiencyTestLockedStatus.class.getName());
	
	/**
	 * host for reports.
	 */
	private AnalysisHost analysisHost;
	
	/**
	 * path to the "tmp" folder as string.
	 */
	private String pathToTmpFolder = System.getProperty("java.io.tmpdir") + System.getProperty("file.separator") + "carisma";
	
	/**
	 * path to the "tmp" folder as {@link File}.
	 */
	private File folderFile = new File(this.pathToTmpFolder);
	
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

	private ResourceSet rs = new ResourceSetImpl();
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		this.analysisHost = host;
		TestingHost changeHost = new TestingHost(false);
		fillExportParameters(parameters);
		initHost(parameters, changeHost);
		this.folderFile.mkdir();
		String buffer = "";
		if (host != null) {
			this.analysisHost = host;
		} else {
			this.analysisHost = new TestingHost(true);
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
		LockedStatusEvolutionDeltaOnlyCheck lset = new LockedStatusEvolutionDeltaOnlyCheck(Boolean.FALSE);
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
		buffer += this.modelAmount + " models have been analyzed";
		this.analysisHost.appendLineToReport(this.messages);
		this.analysisHost.appendLineToReport(buffer);
		return true;
	}
	
	/**
	 * performs the nonEvolution checks.
	 * this checks loads all the UML files in the "tmp" directory and performs a locked-status check for each,
	 * also counting the time needed for the checks
	 * @return the time needed for the checks in millisecs
	 * @throws IOException 
	 */
	private long nonEvolutionCheck() {
		long completeTime = 0;
		for (File f : this.folderFile.listFiles()) {
			if (f.getAbsolutePath().endsWith(".uml")) {
				long time;
				this.modelAmount++;
				LockedStatusCheck lockedStatusCheck  = new LockedStatusCheck();  //locked-status check
				Resource res;
				try {
					res = loadModel(f.getAbsolutePath());
					this.messages += "Could not load model " + f.getAbsolutePath() + "\n!"; //kann eigentlich nicht passieren
				} catch (IOException e1) {
					logger.warning("Error message: " + e1.getMessage());
					continue;
				}
				TestingHost h = new TestingHost(false);
				h.setAnalyzedModel(res);
				time = System.currentTimeMillis();
				lockedStatusCheck.perform(null, h);
				time = System.currentTimeMillis() - time;
				completeTime += time;
				try {
					res.delete(null);
				} catch (IOException e) {
					this.messages += "Could not load model " + f.getAbsolutePath() + "\n!"; //kann eigentlich nicht passieren
					logger.warning("Error message: " + e.getMessage());
				}
				h.delete();
				h = null;
				lockedStatusCheck = null;
			}
			f.delete();
		}
		this.folderFile.delete();
		return completeTime;
	}
	
	/**
	 * fills the parameter map for the modelexporter.
	 * @param parameters parameters of the check.
	 */
	private void fillExportParameters(final Map<String, CheckParameter> parameters) {
		boolean maxDelta = true;
		String maxDeltaString = "carisma.check.umlchange.efficiency.maxdelta";
		String folder = "carisma.check.modelexporter.outputfolder";
		String  notAll = "carisma.check.modelexporter.onlyMaxSuccessfulDeltas";
		CheckParameter maxDeltaParameter = parameters.get(maxDeltaString);
		if (maxDeltaParameter != null) {
			if (maxDeltaParameter instanceof BooleanParameter) {
				maxDelta = ((BooleanParameter) maxDeltaParameter).getValue();
			} else {
				this.messages += "Wrong parameter type for the maxDeltas!\n";
			}
		} else {
			this.messages += "Missing value for maxDeltas!\n";
		}
		FolderParameter fp = new FolderParameter(null, this.folderFile);
		BooleanParameter  bp = new BooleanParameter(null, maxDelta);
		parameters.put(folder, fp);
		parameters.put(notAll, bp);
	}	
	
	/**
	 * method to load a model from an UML file.
	 * @param filePath the path to the UML file
	 * @return 
	 * @return {@link Resource} object of the model
	 */
	private Resource loadModel(final String filepath) throws IOException {
		File testmodelfile = new File(filepath);
		assertTrue(testmodelfile.exists());
		Resource modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		modelres.load(Collections.EMPTY_MAP);
		return modelres;
	}
	
	/**
	 * inits the  given host with a model according to the given parameters.
	 * default values: 100 paths and 10 percentage of paths with UMLChange stereotypes.
	 * @param parameters {@link CheckParameter} map with parameters for the model in it.
	 * @param h {@link TestingHost} to initialize
	 */
	private void initHost(final Map<String, CheckParameter> parameters, final TestingHost h) {
		CheckParameter percentageParameter = parameters.get(this.percentage);
		CheckParameter pathsParameter = parameters.get(this.paths);
		float percentValue = 10;
		int pathValue = 100;
		Resource resource = null;
		if (percentageParameter != null) {
			if (percentageParameter instanceof FloatParameter) {
				percentValue = ((FloatParameter) percentageParameter).getValue();
			} else {
				this.messages += "Wrong parameter type for the percentages of paths with UMLchange stereotypes!\n";
			}
		} else {
			this.messages += "Missing value for the percentages of paths with UMLchange stereotypes!\n";
		}
		if (pathsParameter != null) {
			if (pathsParameter instanceof IntegerParameter) {
				pathValue = ((IntegerParameter) pathsParameter).getValue();
			} else {
				this.messages += "Wrong parameter type for the ammounts of paths!\n";
			}
		} else {
			this.messages += "Missing value for the ammounts of paths!\n";
		}
		LockedStatusModelCreater lsmc = new LockedStatusModelCreater();
		long  time = System.currentTimeMillis();
		resource = lsmc.getNewModel("TestModel", pathValue, percentValue);
		time = System.currentTimeMillis() - time;
		this.analysisHost.appendLineToReport("Modelcreation last " + time + " millisecs");
		h.setAnalyzedModel(resource);
	}

	

}