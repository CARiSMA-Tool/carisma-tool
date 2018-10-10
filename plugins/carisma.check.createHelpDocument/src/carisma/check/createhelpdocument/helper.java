package carisma.check.createhelpdocument;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.uml2.uml.Dependency;

import carisma.core.analysis.AnalysisHost;

public class helper {

	public static Collection<Dependency> makeCollection(Set<Dependency> a) {
		Collection<Dependency> coll = new HashSet<>();
		coll.addAll(a);
		return coll;
	}

	

	public static void roleClassMapper(List<String> classes, HashSet<String> roles, AnalysisHost host) {

		for (String r : roles) {
			for (String c : classes) {
				if (r.equalsIgnoreCase(c)) {
					host.appendToReport("| Role: " + r + " is mapped to Class: " + c + ". |\n");
				}
			}
		}
	}

	public static void documentClassMapper(List<String> classes, HashSet<String> documents, AnalysisHost host) {
		// TODO Auto-generated method stub
		for (String d : documents) {
			for (String c : classes) {
				if (d.equalsIgnoreCase(c)) {
					host.appendToReport("| Document: " + d + " is mapped to Class: " + c + ". |\n");
				}
			}
		}
	}
	
	

	public static void roleNodeNameMapper(HashSet<String> roles, HashSet<String> nodeNames, AnalysisHost host) {
		// TODO Auto-generated method stub
		for (String r : roles) {
			for (String n : nodeNames) {
				if (r.equals(n)) {
					host.appendToReport(
							"| Role: " + r + " is mapped to Node: " + n + ". |\n");
				}
			}
		}
	}
}
