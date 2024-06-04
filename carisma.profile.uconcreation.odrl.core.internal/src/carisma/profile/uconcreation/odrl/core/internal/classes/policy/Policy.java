package carisma.profile.uconcreation.odrl.core.internal.classes.policy;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.ConflictStrategy;

public class Policy extends ODRLClass{
	private static Set<String> ownAttributes = new TreeSet<>() ;
	public String uid;
	public ConflictStrategy conflictStrategy;
	public List<String> profiles;
	public List<String> inheritsFrom;
	
	
	String uidName;
	String conflictStrategyName;
	String profilesName;
	String inheritsFromName;
	
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
