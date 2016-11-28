package carisma.check.activity2petrinet;


import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Package;

import carisma.check.activity2petrinet.petriNet.Export;
import carisma.check.activity2petrinet.petriNet.PetriNet;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;


/**
 * This plugin converts an uml2 activity diagram to a petri net.
 * @author Kubi Mensah
 */
public class Activity2PetrinetCheck implements CarismaCheckWithID {
	
	public static final String CHECK_ID = "carisma.check.activity2petrinet";
	public static final String PARAM_PAGE = "carisma.check.acivity2petrinet.page";
	public static final String PARAM_DEST_FILE = "carisma.check.activity2petrinet.destFile";
	public static final String CHECK_NAME = "CARiSMA Activity to Petri Net Converter";
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		boolean withPage = true;
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Test Activity To Petri Net Converter"));
		
		String exportFileName = System.getProperty("user.dir") + "/petriTest.pnml";
		
		try {
			OutputFileParameter file = (OutputFileParameter) parameters.get(Activity2PetrinetCheck.PARAM_DEST_FILE);
			exportFileName = file.getValue().getAbsolutePath();
		} catch (Exception e) {
			host.appendLineToReport("Error with file parameter. Using default file path!");
		}
		
	
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.appendToReport("No Model can be read");
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Model) {
			Package model = (Package) currentModel.getContents().get(0);
			if (parameters.containsKey(Activity2PetrinetCheck.PARAM_PAGE)) {
				if (parameters.get(Activity2PetrinetCheck.PARAM_PAGE) instanceof BooleanParameter) {
					withPage = ((BooleanParameter) parameters.get(Activity2PetrinetCheck.PARAM_PAGE)).getValue();
				}
			}
			host.appendLineToReport("Starting the Conversion...");
			PetriNet petriNet = new Convert((Model) model, host).convert();
			host.appendLineToReport("Done!");
			Export export = new Export(petriNet);
			host.appendLineToReport("Exporting the converted petri net to :" + exportFileName + "...");
    		export.exportToPNML(exportFileName, withPage);
    		host.appendLineToReport("Done!");
			return true;
		}
			
		return false;
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
