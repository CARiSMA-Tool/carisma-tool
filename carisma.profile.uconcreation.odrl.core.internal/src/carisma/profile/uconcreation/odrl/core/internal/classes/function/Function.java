package carisma.profile.uconcreation.odrl.core.internal.classes.function;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.party.Party;

public abstract class Function extends ODRLClass {
	Party party;
	
	String partyName;

	public Party getParty() {
		return party;
	}

	public void setParty(Party party) {
		this.party = party;
	}
	
	
}
