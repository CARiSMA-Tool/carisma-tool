package carisma.profile.uconcreation.odrl.core.internal.classes.policy;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.ConflictStrategy;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Duty;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Permission;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Prohibition;

public interface Policy extends ODRLClass{

	
	public String getUid();







	public void setUid(String uid);







	public ConflictStrategy getConflictStrategy();







	public void setConflictStrategy(ConflictStrategy conflictStrategy);







	public List<String> getProfiles();







	public void setProfiles(List<String> profiles);






	public List<String> getInheritsFrom();



	public void setInheritsFrom(List<String> inheritsFrom);





	public List<Permission> getPermission();





	public void setPermission(List<Permission> permission);
	public void addPermission(Permission permission);





	public List<Prohibition> getProhibition();


	public void setProhibition(List<Prohibition> prohibition);
	
	public void addProhibition(Prohibition prohibition);







	public List<Duty> getObligation();






	public void setObligation(List<Duty> obligation);
	
	public void addObligation(Duty obligation);

}
