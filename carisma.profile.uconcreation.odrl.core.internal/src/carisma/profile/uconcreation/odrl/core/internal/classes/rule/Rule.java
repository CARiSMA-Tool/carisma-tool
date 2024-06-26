package carisma.profile.uconcreation.odrl.core.internal.classes.rule;

import java.util.LinkedList;
import java.util.List;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.action.Action;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Function;
import carisma.profile.uconcreation.odrl.core.internal.classes.relation.Relation;

public interface Rule extends ODRLClass {
	
	public String getUid();
	public void setUid(String uid);
	
	public List<Function> getInvolvedParties();
	public void setInvolvedParties(List<Function> involvedParties);
	public void addInvolvedParties(Function involvedParty);
	
	public List<Relation> getInvolvedAssets();
	public void setInvolvedAssets(List<Relation> involvedAssets);
	public void addInvolvedAssets(Relation involvedAsset);
	
	public Action getAction();
	public void setAction(Action action);
	public ConstraintInterface getConstraint();
	public void setConstraint(ConstraintInterface constraint);
	
	
}
