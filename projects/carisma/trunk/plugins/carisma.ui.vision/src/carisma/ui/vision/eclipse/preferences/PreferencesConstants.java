package carisma.ui.vision.eclipse.preferences;

public enum PreferencesConstants {

	dbaddress("dbaddress"),
	dbport("dbport"),
	dbApiPath("dbApiPath"),
	dbuser("dbuser"),
	dbpasswd("dbpassword"),
	vision_collection("dbCollection"),
	sts_document("stsDocument"),
	sts_field("stsField"),
	pla_document("plaDocument"),
	pla_field("plaCarismaField"),
	carisma_collection("carismaCollection"),
	carisma_document("carismaDocument"),
	carisma_field("carismaField"), 
	question_document("questionDocument"), 
	question_field("questionField");
	
	private final String string;

	private PreferencesConstants(String s){
		this.string = s;
	}
	
	@Override
	public String toString() {
		return this.string;
	}
}
