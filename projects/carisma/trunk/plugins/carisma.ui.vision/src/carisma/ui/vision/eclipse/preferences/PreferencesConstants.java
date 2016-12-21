package carisma.ui.vision.eclipse.preferences;

public enum PreferencesConstants {

	dbaddress("dbaddress"),
	dbport("dbport"),
	dbApiPath("dbApiPath"),
	dbuser("dbuser"),
	dbpasswd("dbpassword"),
	vision_collection("dbCollection"),
	sts_document("stsDocument"),
	pla_document("plaDocument"),
	pla_field("plaField"),
	carisma_document("CARISMAdocument"),
	carisma_field("CARISMAfield"), 
	question_document("CarismaQuestionsdocument"), 
	question_field("CarismaQuestionsfield");
	
	private final String string;

	private PreferencesConstants(String s){
		this.string = s;
	}
	
	@Override
	public String toString() {
		return this.string;
	}
}
