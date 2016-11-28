package carisma.evolution;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

/**
 * Utility class that eases developing evolution checks.
 * @author Klaus Rudack
 *
 */
public final class EvolutionUtility {

	/**
	 * default constructor.
	 */
	private EvolutionUtility() {
	}
	
	/**
	 * Gets the ID of a non Evolution check to a given Evolution check.
	 * @param evolutionID - name of the evolution check
	 * @return - ID of the non Evolution check, null if there is no extensionPoint carisma.evolution.NonEvolution
	 * or no element if the given ID is at this extensionPoint
	 */
	public static String getNonEvolutionCheck(final String evolutionID) {
		String extensionPointId = EvolutionActivator.EXTENSION_POINT_EVOLUTION_CHECK_MAPPING;
		IExtensionRegistry extensionRegestry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = extensionRegestry.getExtensionPoint(extensionPointId);
		if (extensionPoint == null) {
			return null;
		}
		for (IConfigurationElement configElement : extensionPoint.getConfigurationElements()) {
			String evoID = configElement.getAttribute("EvolutionCheckID");
			if (evoID != null) {
				if (evoID.equals(evolutionID)) {
					return configElement.getAttribute("NonEvolutionCheckID");
				}
			}
		}
		return null;
	}
	
	/**
	 * Gets the ID of an Evolution check to a given non-Evolution check.
	 * @param nonEvolutionCheck - name of the non-evolution check
	 * @return -ID of the Evolution check, null if there is no extensionPoint carisma.evolution.NonEvolution
	 * or no element if the given ID is at this extensionPoint
	 */
	public static String getEvolutionCheck(final String nonEvolutionCheck) {
		String extensionPointId = EvolutionActivator.EXTENSION_POINT_EVOLUTION_CHECK_MAPPING;
		IExtensionRegistry extensionRegestry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = extensionRegestry.getExtensionPoint(extensionPointId);
		if (extensionPoint == null) {
			return null;
		}
		for (IConfigurationElement configElement : extensionPoint.getConfigurationElements()) {
			String nonEvoId = configElement.getAttribute("NonEvolutionCheckID");
			if (nonEvoId != null) {
				if (nonEvoId.equals(nonEvolutionCheck)) {
					return configElement.getAttribute("EvolutionCheckID");
				}
			}
		}
		return null;
	}
	
}
