package carisma.check.policycreation.profileimpl.core.relation;

import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.asset.AssetImpl;

public abstract class RelationImpl extends ODRLClassImpl {
	AssetImpl asset;
	

	public AssetImpl getAsset() {
		return asset;
	}

	public void setAsset(AssetImpl asset) {
		this.asset = asset;
	}
	
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getAssetRelation_Asset());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, activityElement);
			if (attributeValueOdrl instanceof AssetImpl asset) {
				this.setAsset(asset);
			}
		}
	}
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		return handler.createMap(asset, circlePreventionSet);
	}
}
