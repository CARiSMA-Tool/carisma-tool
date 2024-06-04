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
}
