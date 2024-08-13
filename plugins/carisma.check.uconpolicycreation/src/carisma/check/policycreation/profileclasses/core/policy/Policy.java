package carisma.check.policycreation.profileclasses.core.policy;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.Element;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileclasses.ODRLClass;
import carisma.check.policycreation.profileclasses.core.conflict.ConflictStrategy;
import carisma.check.policycreation.profileclasses.core.rule.Duty;
import carisma.check.policycreation.profileclasses.core.rule.Permission;
import carisma.check.policycreation.profileclasses.core.rule.Prohibition;

public class Policy extends ODRLClass{
	String uid;
	ConflictStrategy conflictStrategy;
	List<String> profiles = new LinkedList<String>();
	List<String> inheritsFrom = new LinkedList<String>();
	List<Permission> permission = new LinkedList<Permission>();
	List<Prohibition> prohibition = new LinkedList<Prohibition>();
	List<Duty> obligation = new LinkedList<Duty>();
	
	
	
	public String getUid() {
		return uid;
	}
	public void setUid(String uid) {
		this.uid = uid;
	}
	
	public ConflictStrategy getConflictStrategy() {
		return conflictStrategy;
	}
	public void setConflictStrategy(ConflictStrategy conflictStrategy) {
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
	
	public List<Permission> getPermission() {
		return permission;
	}
	public void setPermission(List<Permission> permission) {
		this.permission = permission;
	}
	public void addPermission(Permission permission) {
		this.permission.add(permission);
	}

	public List<Prohibition> getProhibition() {
		return prohibition;
	}
	public void setProhibition(List<Prohibition> prohibition) {
		this.prohibition = prohibition;
	}
	public void addProhibition(Prohibition prohibition) {
		this.prohibition.add(prohibition);
	}

	public List<Duty> getObligation() {
		return obligation;
	}
	public void setObligation(List<Duty> obligation) {
		this.obligation = obligation;
	}
	public void addObligation(Duty obligation) {
		this.obligation.add(obligation);
	}
	
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_ConflictStrategy());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, containingUmlElement);
			if (attributeValueOdrl instanceof ConflictStrategy conflictValue) {
				this.setConflictStrategy(conflictValue);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_InheritsFrom());
		if (attributeValue instanceof List list) { //TODO String List attribute
			List<String> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, String.class);
			if (attributeValueOdrl!=null) {
				this.getInheritsFrom().addAll(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_Profiles());
		if (attributeValue instanceof List list) { //TODO String List attribute
			List<String> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, String.class);
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
						Object newObject = handler.addElement(stereoAppl, this, containingUmlElement);//TODO No explicit passing of different baseElement for the other Element, as that's nor always practical
						if (newObject instanceof Permission permissionImpl) {
							this.addPermission(permissionImpl);
						} else if (newObject instanceof Prohibition prohibitionImpl) {
							this.addProhibition(prohibitionImpl);
						} else if (newObject instanceof Duty obligationImpl) {
							this.addObligation(obligationImpl);
						}
					}
				}
			}
		}
	}
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		map.put(gatTypeKeyword(), gatClassTerm());
		String profileKey = "profile";
		Object profileValue = null;
		if (profiles.size()==1) {
			profileValue=  handler.createMap(profiles.get(0), circlePreventionSet);
		} else if (profiles.size()>1) {
			profileValue = handler.createMap(profiles, circlePreventionSet);
		}
		if (profileValue != null && (!(profileValue instanceof List valueList) || !valueList.isEmpty())) {
			map.put(profileKey, profileValue);
		}
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
