package carisma.profile.uconcreation.odrl.core.internal.classes.rule;

import java.util.List;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.action.Action;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Function;
import carisma.profile.uconcreation.odrl.core.internal.classes.relation.Relation;

public abstract class Rule extends ODRLClass {
	String uid;
	List<Function> involvedParties;
	List<Relation> involvedAssets;
	Action action;
	ConstraintInterface constraint;
	
	String uidName;
	String involvedPartiesName;
	String involvedAssetsName;
	String actionName;
	
	
	public String getUid() {
		return uid;
	}
	public void setUid(String uid) {
		this.uid = uid;
	}
	public List<Function> getInvolvedParties() {
		return involvedParties;
	}
	public void setInvolvedParties(List<Function> involvedParties) {
		this.involvedParties = involvedParties;
	}
	public List<Relation> getInvolvedAssets() {
		return involvedAssets;
	}
	public void setInvolvedAssets(List<Relation> involvedAssets) {
		this.involvedAssets = involvedAssets;
	}
	public Action getAction() {
		return action;
	}
	public void setAction(Action action) {
		this.action = action;
	}
	public ConstraintInterface getConstraint() {
		return constraint;
	}
	public void setConstraint(ConstraintInterface constraint) {
		this.constraint = constraint;
	}
	
	
}
