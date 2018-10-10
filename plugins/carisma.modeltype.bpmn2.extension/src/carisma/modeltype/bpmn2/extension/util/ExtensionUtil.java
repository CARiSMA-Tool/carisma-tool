package carisma.modeltype.bpmn2.extension.util;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;

public final class ExtensionUtil {
	
	/** Hide default Constructor.
	 */
	private ExtensionUtil() {
		
	}

	/**
	 * Standard EMF Resource loader.
	 * @param file The EMF model
	 * @return Returns the loaded model
	 * @throws IOException If error occurred during loading resource
	 */
	public static Resource loadResource(final File file) throws IOException {
		URI uri = URI.createFileURI(file.getAbsolutePath());
		ResourceSet resourceSet = new ResourceSetImpl();
		Resource resource = resourceSet.getResource(uri, true);
		resource.load(new HashMap<String, Object>());
		return resource;
	}
	
}
