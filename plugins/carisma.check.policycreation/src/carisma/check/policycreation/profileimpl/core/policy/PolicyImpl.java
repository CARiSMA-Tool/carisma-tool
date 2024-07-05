package carisma.check.policycreation.profileimpl.core.policy;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

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
	List<String> profiles = new LinkedList<String>();
	List<String> inheritsFrom = new LinkedList<String>();
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
				this.getInheritsFrom().addAll(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_Profiles());
		if (attributeValue instanceof List list) { //TODO String List attribute
			List<String> attributeValueOdrl = handler.addElement(list, this, activityElement, String.class);
			if (attributeValueOdrl!=null) {
				this.getProfiles().addAll(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_Uid());
		if (attributeValue instanceof String string) {
			this.setUid(string);
		}
		//Activity Diagram: Get contained rules from the contained actions
		if (UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_Base_Activity()) instanceof Activity baseActivity) {
			for (ActivityNode node : new HashSet<>(baseActivity.getNodes())) { //TODO check for alternative solution. Currently converted to set as the list contains every node twice
				System.out.println("BaseActivity nodes of length " + baseActivity.getNodes().size() + baseActivity.getNodes().toString());
				if (node instanceof org.eclipse.uml2.uml.Action action) {
					for (EObject stereoAppl : new HashSet<>(action.getStereotypeApplications())) {
						//TODO: check for already created stereotypes, add to References?
						Object newObject = handler.addElement(stereoAppl, this, action);
						if (newObject instanceof PermissionImpl permissionImpl) {
							this.addPermission(permissionImpl);
						} else if (newObject instanceof ProhibitionImpl prohibitionImpl) {
							this.addProhibition(prohibitionImpl);
						} else if (newObject instanceof DutyImpl obligationImpl) {
							this.addObligation(obligationImpl);
						}
					}
				}
			}
		}
	}
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		map.put(getTypeKeyword(), gatClassTerm());
		return null;
	}
	
	//TODO: remove
//	private void fillMapUid(Map<String, Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
//		String uidResult = handler.createMap(uid, circlePreventionSet);
//		if (uidResult != null) {
//			Object termKey = handler.getTermMap().get(PolicyImpl.class.getDeclaredField("uid"));
//			if (termKey instanceof String termKeyString) {
//				map.put(termKeyString,uidResult);
//			}			
//		}
//	}
//	private void fillMapConflictStrategy(Map<String, Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
//		Object conflictStrategyResult = handler.createMap(conflictStrategy, circlePreventionSet);
//		if (conflictStrategyResult != null) {
//			Object termKey = handler.getTermMap().get(PolicyImpl.class.getDeclaredField("conflictStrategy"));
//			if (termKey instanceof String termKeyString) {
//				map.put(termKeyString,conflictStrategyResult);
//			}			
//		}
//	}
//	private void fillMapProfiles(Map<String, Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
//		List<Object> profilesResult = handler.createMap(profiles, circlePreventionSet);
//		if (profilesResult != null && !profilesResult.isEmpty()) {
//			Object termKey = handler.getTermMap().get(PolicyImpl.class.getDeclaredField("profiles"));
//			if (termKey instanceof String termKeyString) {
//				map.put(termKeyString, profilesResult);
//			}
//		}
//	}
//	private void fillMapInheritsFrom(Map<String, Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
//		List<Object> inheritsFromResult = handler.createMap(inheritsFrom, circlePreventionSet);
//		if (inheritsFromResult != null && !inheritsFromResult.isEmpty()) {
//			Object termKey = handler.getTermMap().get(PolicyImpl.class.getDeclaredField("inheritsFrom"));
//			if (termKey instanceof String termKeyString) {
//				map.put(termKeyString, inheritsFromResult);
//			}
//		}	
//	}
//	private void fillMapPermission(Map<String, Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
//		List<Object> permissionResult = handler.createMap(inheritsFrom, circlePreventionSet);
//		if (permissionResult != null && !permissionResult.isEmpty()) {
//			Object termKey = handler.getTermMap().get(PolicyImpl.class.getDeclaredField("permission"));
//			if (termKey instanceof String termKeyString) {
//				map.put(termKeyString, permissionResult);
//			}
//		}	
//	}
//	private void fillMapProhibition(Map<String, Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
//		List<Object> prohibitionResult = handler.createMap(inheritsFrom, circlePreventionSet);
//		if (prohibitionResult != null && !prohibitionResult.isEmpty()) {
//			Object termKey = handler.getTermMap().get(PolicyImpl.class.getDeclaredField("prohibition"));
//			if (termKey instanceof String termKeyString) {
//				map.put(termKeyString, prohibitionResult);
//			}
//		}	
//	}

}
