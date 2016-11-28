package carisma.ui.vision.questions;

import java.util.ArrayList;
import java.util.List;

import carisma.check.staticcheck.securelinks.SecureLinksCheck;
import carisma.core.analysis.CheckReference;
import carisma.core.analysis.result.AnalysisResult;

public class BuilderFactory {
	
	public static List<Builder> getBuilder(AnalysisResult analysisResult){
		List<Builder> b = new ArrayList<Builder>();
		//a list for the checks
		List<CheckReference> checks =  analysisResult.getAnalysis().getChecks();
		for( int i = 0;  i < checks.size(); i++ ){
			CheckReference checkReference = checks.get(i);
			if(checkReference.isEnabled()){
				String checkID = checkReference.getCheckID();
				switch (checkID){
				case SecureLinksCheck.CHECK_ID:  
					Builder b1 = new SecureLinksBuilder(analysisResult);
					b.add(b1);
					break;
				default:
					break;
				}
			}
		}
		return b;

	}
}
