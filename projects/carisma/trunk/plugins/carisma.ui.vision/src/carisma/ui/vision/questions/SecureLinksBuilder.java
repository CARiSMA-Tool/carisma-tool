package carisma.ui.vision.questions;

import java.util.ArrayList;
import java.util.List;

import carisma.check.staticcheck.securelinks.SecureLinksCheck;
import carisma.core.analysis.result.AnalysisResult;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.CheckResult;


public class SecureLinksBuilder implements Builder {
	
	public static String DEFAULT = "any person containing the gerneral public";
	public static String INSIDER ="the employees of the public administration";
	AnalysisResult analysisResult;
	
	public SecureLinksBuilder(AnalysisResult analysisResult) {
		super();
		this.analysisResult = analysisResult;
	}

	@Override
	public List<Question> generateQuestion() {
		List<Question> questions = new ArrayList<Question>();
		List<String> messages = new ArrayList<String>() ;
		//a list for the checks
		List<CheckResult> checks = this.analysisResult.getCheckResults();
		for( int i = 0;  i < checks.size(); i++ ){
			String checkName = checks.get(i).getName();
			if (checkName.equals(SecureLinksCheck.CHECK_NAME)){
				//List<CheckResult> checkResults = analysisResult.getCheckResults();
				//for(int l = 0; l < checkResults.size(); l++){
					if(!(checks.get(i).isSuccessful())){
						//a list for the check results for secureLinks check
						List<AnalysisResultMessage> results = checks.get(i).getResults();
						for( int k = 0;  k < results.size(); k++ ){
							String message = results.get(k).getText();
							messages.add(message);
						}
					} else {
						System.out.println("The check result is successful! No questions possible!");
						return questions;
					}
				
			}
			String attacker;
			String ability;
			String commPath;
			//check if the message is valid
			for( int j = 0;  j < messages.size(); j++ ){
				String message = messages.get(j);
				String[] parts = message.split(" ");
				if(parts[1].equals("attacker")){ 
					if(parts[2].equals("can")){
						if(parts[4].equals("at")){ 
							//defining attacker, ability and commPath
							attacker = parts[0]; 
							ability = parts[3];
							commPath = parts[5]+" "+parts[6];
							commPath = commPath.substring(0,commPath.length()-1);
						}else throw new IllegalArgumentException("Invalid analysisResult message!");
					}else throw new IllegalArgumentException("Invalid analysisResult message!");
				}else throw new IllegalArgumentException("Invalid analysisResult message!");
				
				//defining outputAttacker
				String outputAttacker;
				if(attacker.equals("default")) outputAttacker = DEFAULT;
				else{ if(attacker.equals("insider")) outputAttacker = INSIDER;
				else throw new IllegalArgumentException("Invalid attacker!"); }
				//defining text
				String s = "Would you allow "
						+ outputAttacker
						+ " to "
						+ ability
						+ " your transmitted personal data at "
						+ commPath
						+ "?";
				//generate question with these attributes
				Question question = new Question();
				question.setAttacker( attacker );
				question.setAbility( ability );
				question.setCommPath( commPath );
				question.setText(s);
				questions.add(question);
				}
		}
			//return a list of question
			return questions;
		}

}
