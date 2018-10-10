package carisma.check.oclcheck.util;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;

import carisma.core.models.ModelLoader;

@Deprecated
public class UniversalModelLoader implements ModelLoader {

	@Override
	public Resource load(File file) throws IOException {
		URI uri = URI.createFileURI(file.getAbsolutePath());
		ResourceSet resourceSet = new ResourceSetImpl();
		Resource resource = resourceSet.getResource(uri, true);
		resource.load(new HashMap<String, Object>());
		return resource;
	}

}
