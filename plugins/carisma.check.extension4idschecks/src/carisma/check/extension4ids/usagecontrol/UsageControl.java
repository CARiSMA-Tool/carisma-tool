package carisma.check.extension4ids.usagecontrol;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.eclipse.emf.common.util.EList;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.Type;

import carisma.check.staticcheck.securelinks.utils.AnalysisMessage;
import carisma.check.staticcheck.securelinks.utils.OutputTarget;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.StatusType;
import carisma.modeltype.uml2.UMLDeploymentHelper;

/** Analyzes a deployment diagram for usage control violation
 * @author Sanjeev Sun Shakya
 *
 */
public class UsageControl {
	
	private static final String DATA_USAGE_CONTROL = "ignore usage control";
	
	private List<AnalysisMessage> errorMessages = null;
	private AnalysisHost analysisHost;

	public UsageControl(AnalysisHost host) {
		if (host != null) {
			this.analysisHost = host;
		} else {
			this.analysisHost = new DummyHost(true);
		}
		this.errorMessages = new ArrayList<>();
	}

	public List<AnalysisMessage> getErrorMessages() {
		return Collections.unmodifiableList(this.errorMessages);
	}
	
	public int checkUsageControl(final Package pkg) {
		this.errorMessages.clear();
		for (Dependency dependency : UsageControlHelper.getAllRelevantDependencies(pkg)) {
			System.out.println("We have dependencies here:"+ dependency.getClass().getName());
			if (UsageControlHelper.hasUsageControlRequirements(dependency)) {
				printProcessedDependency(dependency);

				for (CommunicationPath commPath : UMLDeploymentHelper.getCommunicationPaths(dependency)) {
					printProcessedCommunicationPath(commPath);

					this.errorMessages.addAll(compliesWithRequirements(commPath, dependency));
				}
				Map<Node, Node> unconnectedNodes = UMLDeploymentHelper.getUnconnectedNodes(dependency);
				for (Entry<Node, Node> entry : unconnectedNodes.entrySet()) {
					Node targetNode = entry.getValue();
					this.errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH,
							UsageControlHelper.nodesNotConnected(entry.getKey(), targetNode, dependency)));
				}
			}
		}
		this.analysisHost.appendLineToReport("\n------------------------------------------------------------------------------------");
		this.analysisHost.appendLineToReport("The usage control analysis detected "+errorMessages.size()+" errors.");
		this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------");
		this.analysisHost.appendLineToReport("------------------------------------------------------------------------------------\n");
		return this.errorMessages.size();
	}
	
	private void printProcessedCommunicationPath(CommunicationPath commPath) {
		StringBuilder depString = new StringBuilder("\n\t - Processing communication path '");
		depString.append(commPath.getQualifiedName());
		EList<Type> ends = commPath.getEndTypes();
		if (ends.size() >= 2) {
			depString.append("' between '");
			int i = 0;
			for (; i < ends.size() - 1; i++) {
				depString.append(ends.get(i).getQualifiedName());
				depString.append("', '");
			}
			depString.append("' and '");
			depString.append(ends.get(i).getQualifiedName());
		}
		depString.append("'");
		this.analysisHost.appendLineToReport(depString.toString());
	}

	private void printProcessedDependency(Dependency dependency) {
		StringBuilder depString = new StringBuilder("\nProcessing all relevant communication paths for dependency '");
		depString.append(dependency.getQualifiedName());
		depString.append("' between '");
		EList<NamedElement> clients = dependency.getClients();
		int i = 0;
		for (; i < clients.size() - 1; i++) {
			depString.append(clients.get(i).getQualifiedName());
			depString.append("', '");
		}
		depString.append(clients.get(i).getQualifiedName());
		depString.append("' and '");
		EList<NamedElement> suppliers = dependency.getSuppliers();
		int j = 0;
		for (; j < suppliers.size() - 1; j++) {
			depString.append(suppliers.get(j).getQualifiedName());
			depString.append("', '");
		}
		depString.append(suppliers.get(j).getQualifiedName());
		depString.append("':");
		this.analysisHost.appendLineToReport(depString.toString());
	}
	
	public static List<AnalysisMessage> compliesWithRequirement(final CommunicationPath aLink,
			final Stereotype stRequirement) {
		List<AnalysisMessage> errors = new ArrayList<>();
		String attacker = getAttacker(aLink);
		Set<String> threats = getThreats();
		if (stRequirement.getName().equals("usagecontrol")) {
            threats.add(DATA_USAGE_CONTROL);
        }
		System.out.println("Threats: "+ threats);
		List<String> violations = getViolations(stRequirement, threats, aLink);
		if (!violations.isEmpty()) {
			errors.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH,
					UsageControlHelper.usageControlViolated(attacker, aLink, violations)));
		}
		return errors;
	}
	
	/**
     * Returns the violations of the attacker given the set of threats and the requirement to fulfill.
     * 
     * @param stRequirement - security requirement
     * @param threats - set of threats (induced by a linktype)
     * @return - true, if requirement is met
     */
    public static List<String> getViolations(final Stereotype stRequirement, final Set<String> threats, final CommunicationPath aLink) {
        List<String> violations = new ArrayList<>();
        if (!UsageControlHelper.areBothConnectors(UMLDeploymentHelper.getNodes(aLink))) {
            violations.add(DATA_USAGE_CONTROL);
        }
        return violations;
    }
    
    /**
	 * Checks if the link (or rather the linktype) complies with the
	 * requirements imposed by the dependency.
	 * 
	 * @param aLink
	 *            - the link in question
	 * @param aDep
	 *            - the dependency with requirements
	 * @return - true if the link complies with requirements
	 */
	public static List<AnalysisMessage> compliesWithRequirements(final CommunicationPath aLink, final Dependency aDep) {
		List<AnalysisMessage> errors = new ArrayList<>();
		for (Stereotype stRequirement : getRequirements(aDep)) {
			errors.addAll(compliesWithRequirement(aLink, stRequirement));
		}
		return errors;
	}
	
	/**
	 * Returns the usage control requirements the dependency has.
	 * 
	 * @param aDep
	 *            - the dependency to search
	 * @return - the set of requirements
	 */
	public static Set<Stereotype> getRequirements(final Dependency aDep) {
		HashSet<Stereotype> requirements = new HashSet<>();
		for (Stereotype stereo : aDep.getAppliedStereotypes()) {
			if (UsageControlHelper.isUsageControlRequirement(stereo)) {
				requirements.add(stereo);
			}
		}
		return requirements;
	}
	
	/**
	 * Returns the attacker of the model defined by the usage control stereotype.
	 * 
	 * @param model
	 *            - the model to search
	 * @return - the attacker type
	 */
	public static String getAttacker(final Element element) {
		if (element == null) {
			throw new IllegalArgumentException("Tried to get attacker from a null object.");
		}
		String attacker = "Malicious participant";
		
		return attacker;
	}
	
	public static Set<String> getThreats() {
		HashSet<String> threats = new HashSet<>();
		threats.add(DATA_USAGE_CONTROL);
		return threats;
	}


}
