package carisma.profile.uconcreation.odrl.core.internal.classes.asset;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;

public class Asset extends ODRLClass {
	String uid;
	
	
	
	String uidName;
	
	
	
	public String getUid() {
		return uid;
	}




	public void setUid(String uid) {
		this.uid = uid;
	}




	public String toString() {
		String returnString = this.getClass().getName();
		returnString += System.lineSeparator()+ uid.indent(stringIndent);
		return returnString;
	}
}
