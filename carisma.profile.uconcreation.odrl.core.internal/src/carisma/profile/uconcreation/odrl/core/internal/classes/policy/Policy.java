package carisma.profile.uconcreation.odrl.core.internal.classes.policy;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.ConflictStrategy;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Duty;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Permission;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Prohibition;

public class Policy extends ODRLClass{
	private static Set<String> ownAttributes = new TreeSet<>() ;
	String uid;
	ConflictStrategy conflictStrategy;
	List<String> profiles;
	List<String> inheritsFrom;
	List<Permission> permission;
	List<Prohibition> prohibition;
	List<Duty> obligation;
	
	
	String uidName;
	String conflictStrategyName;
	String profilesName;
	String inheritsFromName;
	String permissionName;
	String prohibitionName;
	String obligationName;
	
	
	
	
	
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







	public List<Prohibition> getProhibition() {
		return prohibition;
	}







	public void setProhibition(List<Prohibition> prohibition) {
		this.prohibition = prohibition;
	}







	public List<Duty> getObligation() {
		return obligation;
	}







	public void setObligation(List<Duty> obligation) {
		this.obligation = obligation;
	}







	static {
		ownAttributes.add("uid");
	}
	
	
	
	
	

	
	@Override
	public Set<String> getOdrlAttributes() {
		Set<String> attributes = super.getOdrlAttributes();
		attributes.add("uid");//alternative zu ownAttributes
		return attributes;
	}
}
