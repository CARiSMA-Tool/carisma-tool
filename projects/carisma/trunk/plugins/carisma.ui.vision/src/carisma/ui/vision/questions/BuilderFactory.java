package carisma.ui.vision.questions;

import java.util.ArrayList;
import java.util.List;

import carisma.core.analysis.CheckReference;
import carisma.core.analysis.result.AnalysisResult;

public class BuilderFactory {
	
	public List<Builder> getBuilder(AnalysisResult analysisResult){
		List<Builder> b = new ArrayList<Builder>();
		//a list for the checks
		List<CheckReference> checks =  analysisResult.getAnalysis().getChecks();
		for( int i = 0;  i < checks.size(); i++ ){
			String checkID = checks.get(i).getCheckID();
			switch (checkID){
			case "carisma.check.staticcheck.securelinks":  
				Builder b1 = new SecureLinksBuilder(analysisResult);
				b.add(b1);
				break;
			}
		}
		return b;

	}
}
