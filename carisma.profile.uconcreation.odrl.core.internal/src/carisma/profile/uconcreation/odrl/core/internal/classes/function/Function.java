package carisma.profile.uconcreation.odrl.core.internal.classes.function;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.party.Party;

public interface Function extends ODRLClass {

	public Party getParty();

	public void setParty(Party party);
	
	
}
