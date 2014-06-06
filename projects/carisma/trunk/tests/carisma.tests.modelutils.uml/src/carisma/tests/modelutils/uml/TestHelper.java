package carisma.tests.modelutils.uml;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;

public class TestHelper {
	
	private static UML2ModelLoader ml = null;
	
	/**
	 * Loads model when unit testing.
	 * @param modelFolderPath
	 * @param testmodelname
	 * @return
	 */
	public static final Model loadModel(final String modelFolderPath, final String testmodelname) {
		File testmodelfile = new File(modelFolderPath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		if (ml == null) {
			ml = new UML2ModelLoader();
		}
		Resource modelres = null;
		try {
			modelres = ml.load(testmodelfile);
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
		assertNotNull(modelres);
		Model theModel = (Model) modelres.getContents().get(0);
		assertNotNull(theModel);
		return theModel;
	}
	/**
	 * Safely unloads model in unit testing.
	 * @param theModel
	 * @return
	 */
	public static final boolean unloadModel(final Model theModel) {
		Resource theResource = theModel.eResource();
		assertNotNull(theResource);
		theResource.unload();
		assertFalse(theResource.isLoaded());
		return true;
	}
	/**
	 * Returns an adequately qualified element of a given type.
	 * Unit testing fails if element not found
	 * @param pkg
	 * @param adequatelyQualifiedName
	 * @param type
	 * @return - the element you asked for
	 */
	public static <T extends NamedElement> T checkedGetElement(final Package pkg, final String adequatelyQualifiedName, final java.lang.Class<T> type) {
		T targetElement = null;
		try {
			targetElement = UMLHelper.getElementOfNameAndType(pkg, adequatelyQualifiedName, type);
		} catch (ModelElementNotFoundException e) {
			fail(e.getMessage());
		}
		assertNotNull(targetElement);
		return targetElement;
	}
	
}
