package carisma.evolution.uml2;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;

import carisma.evolution.Delta;



public class ModifierMap {
	
	private Map<Delta,UMLModifier> wrappedMap = null;
	
	private Resource modelResource = null;

	public ModifierMap(final Resource usedModelResource) {
		this.wrappedMap = new HashMap<>();
		this.modelResource = usedModelResource;
	}
	
	public UMLModifier get(final Delta delta) {
		UMLModifier aModifier = null;
		if (this.wrappedMap.containsKey(delta)) {
			System.out.println("Modifier von dem Delta schon da...");
			aModifier = this.wrappedMap.get(delta);
		} else {
			System.out.println("Modifier noch nicht da, mache einen...");
			aModifier = new UMLModifier(this.modelResource, delta);
			this.wrappedMap.put(delta, aModifier);
		}
		return aModifier;
	}
}
