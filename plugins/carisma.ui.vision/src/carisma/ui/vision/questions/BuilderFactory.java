package carisma.ui.vision.questions;

import java.util.ArrayList;
import java.util.List;

import carisma.check.staticcheck.securelinks.SecureLinksCheck;
import carisma.core.analysis.CheckReference;
import carisma.core.analysis.result.AnalysisResult;
import carisma.core.analysis.result.CheckResult;

public class BuilderFactory {
	
	public static List<Builder> getBuilder(AnalysisResult analysisResult){
		List<Builder> b = new ArrayList<>();
		//a list for the checks
		List<CheckResult> checks = analysisResult.getCheckResults();
		for( int i = 0;  i < checks.size(); i++ ){
			String checkName = checks.get(i).getName();
			switch(checkName){
				case SecureLinksCheck.CHECK_NAME:
					Builder b1 = new SecureLinksBuilder(analysisResult);
					b.add(b1);
					break;
				default:
					break;
			}
		}
		return b;

	}
}
