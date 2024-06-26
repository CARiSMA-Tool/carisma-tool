package carisma.profile.uconcreation.odrl.core.internal.classes.relation;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.asset.Asset;

public interface Relation extends ODRLClass {

	public Asset getAsset();

	public void setAsset(Asset asset);
	
	
}
