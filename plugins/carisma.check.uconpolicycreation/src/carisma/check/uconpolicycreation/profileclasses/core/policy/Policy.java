package carisma.check.uconpolicycreation.profileclasses.core.policy;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.Element;

import carisma.check.uconpolicycreation.UMLModelConverter;
import carisma.check.uconpolicycreation.profileclasses.ODRLClass;
import carisma.check.uconpolicycreation.profileclasses.core.conflict.ConflictStrategy;
import carisma.check.uconpolicycreation.profileclasses.core.rule.Duty;
import carisma.check.uconpolicycreation.profileclasses.core.rule.Permission;
import carisma.check.uconpolicycreation.profileclasses.core.rule.Prohibition;
import carisma.check.uconpolicycreation.profileclasses.core.rule.Rule;

public class Policy extends ODRLClass{
	/**
	 * The identifier which this policy can be referred by.
	 */
	String uid;
	/**
	 * How conflicting rules are resolved.
	 */
	ConflictStrategy conflictStrategy;
	/**
	 * ODRL-Profiles that this policy conforms to.
	 */
	List<String> profiles = new LinkedList<>();
	/**
	 * ODRL-Profiles that this policy inherits from.
	 */
	List<String> inheritsFrom = new LinkedList<>();
	/**
	 * {@link Permission}s directly contained in this policy.
	 */
	List<Permission> permission = new LinkedList<>();
	/**
	 * {@link Prohibition}s directly contained in this policy.
	 */
	List<Prohibition> prohibition = new LinkedList<>();
	/**
	 * {@link Duty}s directly contained in this policy.
	 */
	List<Duty> obligation = new LinkedList<>();
	
	
	
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
		if (attributeValue instanceof List list) {
			List<String> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, String.class);
			if (attributeValueOdrl!=null) {
				this.getInheritsFrom().addAll(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getODRLPolicy_Profiles());
		if (attributeValue instanceof List list) {
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
			for (ActivityNode node : new HashSet<>(baseActivity.getNodes())) { //TODO check for alternative solution. Currently converted to Set as the list contains every node twice
				if (node instanceof org.eclipse.uml2.uml.Action action) {
					processStereotypes://Label to jump out if a rule being processes is already contained elsewhere
					for (EObject stereoAppl : new HashSet<>(action.getStereotypeApplications())) {
						Object newObject = handler.addElement(stereoAppl, this, containingUmlElement);
						if (newObject instanceof Rule rule) {
							for (ODRLClass referringElement : rule.getReferredBy()) {
								if (referringElement instanceof Rule) {//Rules referred by other elements should not be directly contained in the policy. Needs to be covered both at the Policy and the Rules because the processing order is not fixed.
									rule.removeReferredBy(this);
									continue processStereotypes;
								}
							}
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
	}
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		//handler.addToTopLevelMapElements(map); //part of something not implemented
		map.put(getTypeKeyword(), getClassTerm());
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
		map.put("@context", handler.getContextMapValue());
		return null;
	}
	
	@Override
	public void setHandler(UMLModelConverter handler) {
		super.setHandler(handler);
		handler.setPolicyRoot(this);
	}
}
