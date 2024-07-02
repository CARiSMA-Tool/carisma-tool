package carisma.check.policycreation.profileimpl.core.policy;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.ActivityNode;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.conflict.ConflictStrategyImpl;
import carisma.check.policycreation.profileimpl.core.rule.DutyImpl;
import carisma.check.policycreation.profileimpl.core.rule.PermissionImpl;
import carisma.check.policycreation.profileimpl.core.rule.ProhibitionImpl;

public class PolicyImpl extends ODRLClassImpl{
	String uid;
	ConflictStrategyImpl conflictStrategy;
	List<String> profiles;
	List<String> inheritsFrom;
	List<PermissionImpl> permission = new LinkedList<PermissionImpl>();
	List<ProhibitionImpl> prohibition = new LinkedList<ProhibitionImpl>();
	List<DutyImpl> obligation = new LinkedList<DutyImpl>();
	
	
	public String getUid() {
		return uid;
	}
	public void setUid(String uid) {
		this.uid = uid;
	}
	
	public ConflictStrategyImpl getConflictStrategy() {
		return conflictStrategy;
	}
	public void setConflictStrategy(ConflictStrategyImpl conflictStrategy) {
		this.conflictStrategy = conflictStrategy;
	}
	
	public List<String> getProfiles() {
		return profiles;
	}
	public void setProfiles(List<String> profiles) {
		this.profiles = profiles;
	}
	
	public List<String> getInheritsFrom() {
		return inheritsFrom;
	}
	public void setInheritsFrom(List<String> inheritsFrom) {
		this.inheritsFrom = inheritsFrom;
	}
	
	public List<PermissionImpl> getPermission() {
		return permission;
	}
	public void setPermission(List<PermissionImpl> permission) {
		this.permission = permission;
	}
	public void addPermission(PermissionImpl permission) {
		this.permission.add(permission);
	}

	public List<ProhibitionImpl> getProhibition() {
		return prohibition;
	}
	public void setProhibition(List<ProhibitionImpl> prohibition) {
		this.prohibition = prohibition;
	}
	public void addProhibition(ProhibitionImpl prohibition) {
		this.prohibition.add(prohibition);
	}

	public List<DutyImpl> getObligation() {
		return obligation;
	}
	public void setObligation(List<DutyImpl> obligation) {
		this.obligation = obligation;
	}
	public void addObligation(DutyImpl obligation) {
		this.obligation.add(obligation);
	}
	
	
	@Override
	public void fill(EObject currentEObject, EObject activityElement, UMLModelConverter handler) {
		super.fill(currentEObject, activityElement, handler);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_ConflictStrategy());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, activityElement);
			if (attributeValueOdrl instanceof ConflictStrategyImpl conflictValue) {
				this.setConflictStrategy(conflictValue);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_InheritsFrom());
		if (attributeValue instanceof List list) { //TODO String List attribute
			List<String> attributeValueOdrl = handler.addElement(list, this, activityElement, String.class);
			if (attributeValueOdrl!=null) {
				this.setInheritsFrom(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_Profiles());
		if (attributeValue instanceof List list) { //TODO String List attribute
			List<String> attributeValueOdrl = handler.addElement(list, this, activityElement, String.class);
			if (attributeValueOdrl!=null) {
				this.setProfiles(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_Uid());
		if (attributeValue instanceof String string) {
			this.setUid(string);
		}
		//Activity Diagram: Get contained rules from the contained actions
		if (UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_Base_Activity()) instanceof Activity baseActivity) {
			for (ActivityNode node : baseActivity.getNodes()) {
				if (node instanceof org.eclipse.uml2.uml.Action action) {
					for (EObject stereoAppl : action.getStereotypeApplications()) {
						Object newObject = handler.addElement(stereoAppl, this, action);
						if (newObject instanceof PermissionImpl permission) {
							this.addPermission(permission);
						} else if (newObject instanceof ProhibitionImpl prohibition) {
							this.addProhibition(prohibition);
						} else if (newObject instanceof DutyImpl obligation) {
							this.addObligation(obligation);
						}
					}
				}
			}
		}
	}

	@Override
	public void fillMap(Map<String, Object> map, Set<ODRLClassImpl> circlePreventionSet) {
		super.fillMap(map, circlePreventionSet);
		String uidResult = handler.createMap(uid, circlePreventionSet);
		if (uidResult != null) {
			Object termKey = handler.getTermMap().get(this.getClass().getDeclaredField("uid"));
			if (termKey instanceof String termKeyString) {
				map.put(termKeyString,uidResult);
			}
			
		}
		
	}
	
	

}
