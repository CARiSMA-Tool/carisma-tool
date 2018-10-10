package carisma.regulatory.ruleallocator;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.Resource.Factory.Registry;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;

import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.DatamodelFactory;
import carisma.regulatory.ruleallocator.datamodel.DatamodelPackage;
import carisma.regulatory.ruleallocator.datamodel.ModelElementType;

/**
 * Provides methods to load, save and update the {@link carisma.regulatory.ruleallocator.datamodel}
 * @author jkowald
 *
 */
public class DatamodelManager {
	
	/**
	 * An ResourceSet instance to hold the resource object.
	 */
	private ResourceSet resourceSet;
	
	/**
	 * A package wide resource instance to handle loading and saving of the container.
	 */
	private Resource resource;
	
	/**
	 * A package wide container instance.
	 */
	private Container container;
	
	/**
	 * The path to the actual loaded Datamodel file.
	 */
	private String filepath;
	
	/**
	 * Instance of the DatamodelFactory
	 */
	private DatamodelFactory datamodelFactory;
	
	/**
	 * Constructor.
	 */
	public DatamodelManager() {
		datamodelFactory = DatamodelFactory.eINSTANCE;
		Registry registry = Registry.INSTANCE;
	    Map<String, Object> map = registry.getExtensionToFactoryMap();
	    map.put("xmi", new XMIResourceFactoryImpl());
	    map.put("temp", new XMIResourceFactoryImpl());
	    resourceSet = new ResourceSetImpl();
	    resource = null;
	    container = datamodelFactory.createContainer();
	    filepath = "";
	    DatamodelPackage datamodelPackage = DatamodelPackage.eINSTANCE;
	}
	
	/**
	 * Loads a container from the given Datamodel file.
	 * @param path The path to the file
	 * @return The success of the method
	 */
	public boolean loadFile(String path) {
		if (!path.isEmpty() && (new File(path)).exists()) {
			filepath = path;
			resource = resourceSet.getResource(URI.createURI("file:///" + path), true);
			if (resource.getContents().size() > 0) {
				container = (Container) resource.getContents().get(0);
			}
			return true;
		} 
		return false;
	}
	
	/**
	 * Saves the container / resource object to the given file path.
	 */
	public void saveFile() {
		if (resourceSet != null && resource != null) {
			try {
				resource.save(null);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * Returns the actual container object.
	 * @return The actual container object
	 */
	public Container getContainer() {
		return container;
	}
	
	/**
	 * Returns the actual resource object.
	 * @return The actual resource object
	 */
	public Resource getResource() {
		return resource;
	}
	
	/**
	 * Returns the path to the actual loaded file.
	 * @return The path to the actual loaded file
	 */
	public String getFilepath() {
		return filepath;
	}
	
	/**
	 * Returns true if a resource object exist, which is connected to a Datamodel file.
	 * @return True if a resource object exist, which is connected to a Datamodel file, false otherwise
	 */
	public boolean isContainerFromFile() {
		return (resource != null); 
	}
	
	/**
	 * Creates a new temp file for the Yaoqiang editor.
	 * @param path The path of the new temp file
	 * @return An instance of File which is related to the new temp file
	 */
	public File createYaoqiangTempfile(String path) {
		File tempFile = new File(path);
		try {
			tempFile.createNewFile();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return tempFile;
	}
	
	/**
	 * This method reads the Yaoqiang temporary file and returns the chosen BPMNElement.
	 * @param path The path to the temporary file.
	 * @return The BPMNElement, which is selected in the Yaoqiang Editor
	 */
	public BPMNElement readYaoqiangTempfile(String path) {
		Resource tempResource = resourceSet.createResource(URI.createFileURI(path));
		
		BPMNElement chosenBPMNElement = null;
		try {
			tempResource.load(null);
		} catch (IOException e) {
			e.printStackTrace();
		}

		if (tempResource.getContents().size() > 0) {
			Container tempContainer = (Container) tempResource.getContents().get(0);
			ModelElementType chosenModelElementType = null;
			for (Object tempModelElementTypeObject : tempContainer.getContainsModelType()) {
				chosenModelElementType = (ModelElementType) tempModelElementTypeObject;
			}
			for (Object tempBPMNElementObject : tempContainer.getContainsBPMNElement()) {
				chosenBPMNElement = (BPMNElement) tempBPMNElementObject;
			}
			chosenBPMNElement.setType(chosenModelElementType);
		}
		return chosenBPMNElement;
	}
	
	/**
	 * Deletes an old Yaoqiang temporary file.
	 * @param path The path to the temp file which will be deleted
	 */
	public void deleteYaoqiangTempfile(String path) {
		File tempFile = new File(path);
		if (tempFile.exists()) {
			tempFile.delete();
		}
	}
}
