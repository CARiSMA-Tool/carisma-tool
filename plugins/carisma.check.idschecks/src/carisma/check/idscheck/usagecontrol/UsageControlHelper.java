package carisma.check.idscheck.usagecontrol;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;

import carisma.check.staticcheck.securelinks.Messages;
import carisma.check.staticcheck.securelinks.SecureLinks;
import carisma.check.staticcheck.securelinks.utils.AnalysisMessage;
import carisma.check.staticcheck.securelinks.utils.OutputTarget;
import carisma.core.analysis.result.StatusType;
import carisma.core.util.EObjectUtil;
import carisma.modeltype.uml2.UMLDeploymentHelper;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.extension4ids.Extension4IDS;
import carisma.profile.umlsec.extension4ids.Extension4IDSUtil;

/** Helper class for usage control check
 * @author Sanjeev Sun Shakya
 *
 */
public class UsageControlHelper {
	private static final String DATA_USAGE_CONTROL = "ignore usage control";
	
	/**
     * Checks if any artifact deployed on the node has the <<IDSconnector>> stereotype.
     * 
     * @param node - the node to check
     * @return true if any artifact deployed on the node has the <<IDSconnector>> stereotype
     */
	public static boolean hasConnectorArtifact(Node node) {
        for (Element element : UMLDeploymentHelper.getDeployedArtifacts(node)) {
            if (element instanceof Artifact && Extension4IDSUtil.hasStereotype(element, Extension4IDS.IDSCONNECTOR)) {
            	System.out.println("Is truee");
                return true;
            }
        }
        return false;
    }
	
	/**
     * Checks if data usage control should be applied based on the presence of the <<IDS connector>> stereotype on artifacts.
     * 
     * @param nodes - the nodes to check
     * @return true if data usage control should be applied
     */
    public static boolean areBothConnectors(List<Node> nodes) {
        int count = 0;
        for (Node node : nodes) {
            if (hasConnectorArtifact(node)) {
                count++;
            }
        }
        return count == 2;
    }
    
    /**
     * Checks if the link (or rather the linktype) complies with the stereotype
     * requirement.
     * 
     * @param aLink - the link in question
     * @param stRequirement - the requirement stereotype
     * @return - true if the link complies with the requirement
     */
 
    
    public static Set<Dependency> getAllRelevantDependencies(final Package pkg) {
		Set<Dependency> relevantDependencies = new HashSet<>();
		
		relevantDependencies.addAll(UMLHelper.getAllElementsOfType(pkg, Dependency.class));
		return relevantDependencies;
	}
    
    public static String usageControlViolated(
			final String attacker,
			final CommunicationPath commPath,
			final List<String> violations) {
		StringBuffer bfr = new StringBuffer();
		for (String ability : violations) {
			bfr.append(ability);
			bfr.append("/");
		}
		bfr.deleteCharAt(bfr.lastIndexOf("/"));
		return attacker
				+ " can "
				+ bfr.toString()
				+ " at "
				+ EObjectUtil.getTypeAndName(commPath)
				+ ".";
	}
    
    public static String nodesNotConnected(final Node sourceNode, final Node targetNode, final Dependency dep) {
		return "Even though "
				+ EObjectUtil.getTypeAndName(dep)
				+ " has requirements, there is no CommunicationPath between "
				+ EObjectUtil.getTypeAndName(sourceNode)
				+ " and "
				+ EObjectUtil.getTypeAndName(targetNode)
				+ ".";
	}
    
    /**
	 * Checks if the stereotype is a usage control
	 * requirement used on dependencies. 
	 * @param stereotype - the stereotype to check
	 * @return - true if stereotype is requirement
	 */
	public static boolean 
	isUsageControlRequirement(final Stereotype stereotype) {
		System.out.println("profile name: "+ stereotype.getProfile().getName());
		if (!stereotype.getProfile().getName().contains("ids")) {
			return false;
		}
		return isUsageControlRequirement(stereotype.getName());
	}
	
	/**
	 * Checks if the extension4ids profile contains a stereotype of the given name.
	 * @param stName - name of stereotype to look for
	 * @return - true if extension4ids contains stereotype
	 */
	public static boolean isUsageControlRequirement(final String stName) {
		List<String> nameParts = Arrays.asList(stName.split("::"));
		String stereoName = nameParts.get(nameParts.size() - 1);
		if (stereoName.equalsIgnoreCase("usagecontrol")) {
			return true;
		}				
		return false;
	}
	
	/**
	 * Checks if the dependency has Usage control
	 * requirements at all.
	 * @param aDep - the dependency to check
	 * @return - true if it has any
	 */
	public static boolean hasUsageControlRequirements(final Dependency aDep) {
		for (Stereotype stereo : aDep.getAppliedStereotypes()) {
			if (isUsageControlRequirement(stereo)) {
				return true;
			}
		}
		return false;
	}
    
    
}
