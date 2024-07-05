package carisma.check.policycreation.profileimpl.core.relation;

import org.eclipse.emf.ecore.EObject;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.asset.AssetImpl;

public abstract class RelationImpl extends ODRLClassImpl {
	AssetImpl asset;
	
	String assetName;

	public AssetImpl getAsset() {
		return asset;
	}

	public void setAsset(AssetImpl asset) {
		this.asset = asset;
	}
	
	
	@Override
	public void fill(EObject currentEObject, EObject activityElement, UMLModelConverter handler) {
		super.fill(currentEObject, activityElement, handler);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getAssetRelation_Asset());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, activityElement);
			if (attributeValueOdrl instanceof AssetImpl asset) {
				this.setAsset(asset);
			}
		}
	}
}
