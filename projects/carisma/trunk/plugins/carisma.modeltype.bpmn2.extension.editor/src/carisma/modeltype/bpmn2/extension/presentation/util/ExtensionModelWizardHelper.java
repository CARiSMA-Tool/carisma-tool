package carisma.modeltype.bpmn2.extension.presentation.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import org.eclipse.bpmn2.DocumentRoot;
import org.eclipse.bpmn2.Lane;
import org.eclipse.bpmn2.Task;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;

import carisma.modeltype.bpmn2.BPMN2Helper;
import carisma.modeltype.bpmn2.extension.ExtensionFactory;
import carisma.modeltype.bpmn2.extension.ExtensionRoot;


/**
 * Helper class for the ExtensionModelWizard.
 * All non GUI methods are implemented here.
 * @author Marcel Michel
 */
public class ExtensionModelWizardHelper {

	/**
	 * Loads a bpmn2 model and inserts its extensible components to the extension model.
	 * @param rootObject The rootObject of the extensionModel
	 * @param bpmn2File The bpmn2 model represented as file object
	 * @throws IOException If bpmn2 model could not be read
	 */
	public static void addExtensibleObjectsToModel(EObject rootObject, File bpmn2File) throws IOException {
		ResourceSet rs = new ResourceSetImpl();
		Resource bpmn2model = rs.createResource(URI.createURI(bpmn2File.getPath()));
		try(FileInputStream inputStream = new FileInputStream(bpmn2File)){
			bpmn2model.load(inputStream, Collections.EMPTY_MAP);
		}
		DocumentRoot root =  (DocumentRoot) bpmn2model.getContents().get(0);
		
		List<Task> taskList = BPMN2Helper.getAllElementsOfType(root, Task.class);
		List<Lane> laneList = BPMN2Helper.getAllElementsOfType(root, Lane.class);
		
		ExtensionRoot rootElement = (ExtensionRoot) rootObject;
		
		for (Task t : taskList) {
			carisma.modeltype.bpmn2.extension.Task extTask = 
					ExtensionFactory.eINSTANCE.createTask();
			extTask.setName(t.getName());
			rootElement.getTask().add(extTask);
		}
		
		for (Lane l : laneList) {
			carisma.modeltype.bpmn2.extension.Lane extLane = 
					ExtensionFactory.eINSTANCE.createLane();
			extLane.setName(l.getName());
			rootElement.getLane().add(extLane);
		}
	}
}
