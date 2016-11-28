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
				this.analysisFilename = args[i+1];
				i++;
			}
			if (PARAM_WORKDIR.equalsIgnoreCase(arg)) {
				this.analysisWorkdir = args[i+1];
				i++;
			}
			i++;
		}
		if (this.analysisFilename == null) {
			System.out.println("No analysis file specified! ("+PARAM_ANALYSIS+" [*.adf file])");
			return Integer.valueOf(-1);
		}
		if (this.analysisWorkdir == null) {
			System.out.println("No working directory specified! ("+PARAM_WORKDIR+" [workdir])");
			return Integer.valueOf(-1);
		}
		System.out.println("Starting CARiSMA with working directory '"+this.analysisWorkdir+"'...");
		
		if (this.analysisFilename.startsWith(this.analysisWorkdir)) {
			//TODO: what happens here?
		}

		this.analysisFilename = this.analysisWorkdir+"/"+this.analysisFilename;

		System.out.println("analysis file = "+this.analysisFilename);
		
		
		
		Carisma.getInstance().getModelManager().setWorkingDirectory(new File(this.analysisWorkdir));

		Analysis analysis = AnalysisUtil.readAnalysis(this.analysisFilename);
		Carisma.getInstance().runAnalysis(analysis, new ConsoleUIConnector());
		return IApplication.EXIT_OK;
	}

	@Override
	public void stop() {
		// TODO Auto-generated method stub

	}

}
