package carisma.ui.console;

import java.io.File;

import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;

import carisma.core.Carisma;
import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;


public class ConsoleApplication implements IApplication {

	public static final String PARAM_ANALYSIS = "--analysis";
	public static final String PARAM_WORKDIR = "--workdir";
	private String analysisFilename = null;
	private String analysisWorkdir = null;
	
	@Override
	public Object start(IApplicationContext context) throws Exception {
		String[] args = (String[]) context.getArguments().get(IApplicationContext.APPLICATION_ARGS);
		int i = 0;
		while (i < args.length) {
			String arg = args[i];
			//System.out.println("..."+arg);
			if (PARAM_ANALYSIS.equalsIgnoreCase(arg)) {
				analysisFilename = args[i+1];
				i++;
			}
			if (PARAM_WORKDIR.equalsIgnoreCase(arg)) {
				analysisWorkdir = args[i+1];
				i++;
			}
			i++;
		}
		if (analysisFilename == null) {
			System.out.println("No analysis file specified! ("+PARAM_ANALYSIS+" [*.adf file])");
			return -1;
		}
		if (analysisWorkdir == null) {
			System.out.println("No working directory specified! ("+PARAM_WORKDIR+" [workdir])");
			return -1;
		}
		System.out.println("Starting CARiSMA with working directory '"+analysisWorkdir+"'...");
		
		if (analysisFilename.startsWith(analysisWorkdir)) {
		}

		analysisFilename = analysisWorkdir+"/"+analysisFilename;

		System.out.println("analysis file = "+analysisFilename);
		
		
		
		Carisma.getInstance().getModelManager().setWorkingDirectory(new File(analysisWorkdir));

		Analysis analysis = AnalysisUtil.readAnalysis(analysisFilename);
		Carisma.getInstance().runAnalysis(analysis, new ConsoleUIConnector());
		return IApplication.EXIT_OK;
	}

	@Override
	public void stop() {
		// TODO Auto-generated method stub

	}

}
