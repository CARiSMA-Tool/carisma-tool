package carisma.evolution.uml2;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;

import carisma.evolution.Delta;



public class ModifierMap {
	
	private Map<Delta,UMLModifier> wrappedMap = null;
	
	private Resource modelResource = null;

	public ModifierMap(final Resource usedModelResource) {
		wrappedMap = new HashMap<Delta, UMLModifier>();
		modelResource = usedModelResource;
	}
	
	public UMLModifier get(final Delta delta) {
		UMLModifier aModifier = null;
		if (wrappedMap.containsKey(delta)) {
			System.out.println("Modifier von dem Delta schon da...");
			aModifier = wrappedMap.get(delta);
		} else {
			System.out.println("Modifier noch nicht da, mache einen...");
			aModifier = new UMLModifier(modelResource, delta);
			wrappedMap.put(delta, aModifier);
		}
		return aModifier;
	}
}
