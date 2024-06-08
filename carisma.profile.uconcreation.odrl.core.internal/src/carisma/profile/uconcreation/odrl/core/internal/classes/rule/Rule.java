package carisma.profile.uconcreation.odrl.core.internal.classes.rule;

import java.util.Set;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.action.Action;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Function;
import carisma.profile.uconcreation.odrl.core.internal.classes.relation.Relation;

public abstract class Rule extends ODRLClass {
	String uid;
	Set<Function> involvedParties;
	Set<Relation> involvedAssets;
	Action action;
	
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
	public Set<Function> getInvolvedParties() {
		return involvedParties;
	}
	public void setInvolvedParties(Set<Function> involvedParties) {
		this.involvedParties = involvedParties;
	}
	public Set<Relation> getInvolvedAssets() {
		return involvedAssets;
	}
	public void setInvolvedAssets(Set<Relation> involvedAssets) {
		this.involvedAssets = involvedAssets;
	}
	public Action getAction() {
		return action;
	}
	public void setAction(Action action) {
		this.action = action;
	}
	
	
}
