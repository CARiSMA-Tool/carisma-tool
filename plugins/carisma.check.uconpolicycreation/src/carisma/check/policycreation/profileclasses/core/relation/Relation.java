package carisma.check.policycreation.profileclasses.core.relation;

import java.util.Map;
import java.util.Set;

import carisma.check.policycreation.profileclasses.ODRLClass;
import carisma.check.policycreation.profileclasses.core.asset.Asset;

public abstract class Relation extends ODRLClass {
	Asset asset;
	

	public Asset getAsset() {
		return asset;
	}

	public void setAsset(Asset asset) {
		this.asset = asset;
	}
	
	//Handled in Rule
//	@Override
//	public void fill(EObject currentEObject, Element activityElement) {
//		super.fill(currentEObject, activityElement);
//		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getAssetRelation_Asset());
//		if (attributeValue instanceof EObject newEObj) {
//			Object attributeValueOdrl = handler.addElement(newEObj, this, activityElement);
//			if (attributeValueOdrl instanceof AssetImpl asset) {
//				this.setAsset(asset);
//			}
//		}
//	}
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		return handler.createMap(asset, circlePreventionSet);
	}
}
