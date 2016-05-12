package carisma.check.createhelpdocument;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.uml2.uml.Dependency;

public class helper {

	
	public static Collection<Dependency> makeCollection(Set<Dependency> a) {
	    Collection<Dependency> coll = new HashSet<Dependency>();
	    coll.addAll(a);
	    return coll;
	}}
