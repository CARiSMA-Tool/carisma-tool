package carisma.profile.uconcreation.odrl.core.internal.classes.relation;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.asset.Asset;

public abstract class Relation extends ODRLClass {
	Asset asset;
	
	String assetName;

	public Asset getAsset() {
		return asset;
	}

	public void setAsset(Asset asset) {
		this.asset = asset;
	}
	
	
}
