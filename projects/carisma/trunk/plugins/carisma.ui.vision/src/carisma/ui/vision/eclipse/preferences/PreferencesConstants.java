package carisma.ui.vision.eclipse.preferences;

public enum PreferencesConstants {

	dbaddress("dbaddress"),
	dbport("dbport"),
	dbApiPath("dbApiPath"),
	dbuser("user"),
	dbpasswd("password"),
	sts_collection("stsCollection"),
	sts_document("stsDocument"),
	sts_field("stsField"),
	pla_collection("plaCollection"),
	pla_document("plaDocument"),
	pla_field("plaField"),
	carisma_collection("CARISMAcollection"),
	carisma_document("CARISMAdocument"),
	carisma_field("CARISMAfield"), 
	question_collection(""), 
	question_document(""), 
	question_field("");
	
	private final String string;

	private PreferencesConstants(String s){
		this.string = s;
	}
	
	@Override
	public String toString() {
		return this.string;
	}
}
