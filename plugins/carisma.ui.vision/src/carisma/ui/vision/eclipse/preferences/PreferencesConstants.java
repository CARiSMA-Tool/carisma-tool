package carisma.ui.vision.eclipse.preferences;

public enum PreferencesConstants {

	dbaddress("dbaddress"),
	dbport("dbport"),
	dbApiPath("dbApiPath"),
	dbTimeout("dbTimeout"),
	dbuser("dbuser"),
	dbpasswd("dbpassword"),
	vision_collection("dbCollection"),
	sts_document("stsDocument"),
	pla_document("plaDocument"),
	pla_field("plaCarismaField"), 
	carisma_document("CARISMAdocument"),
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
