package carisma.processanalysis.tests;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.eclipse.emf.ecore.resource.Resource;
import org.junit.Test;

import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.processanalysis.loader.misc.UML2ActivityImporter;

/**
 * JUnit tests for the processanalysis model.
 * @author Klaus Rudack
 *
 */
public class ModelTest {
	
	/**
	 * path to the model-folder.
	 */
	private String testmodeldir = "";
	
	/**
	 * name of the model.
	 */
	private String testmodelfilename = "";
	
	/**
	 * modelfile.
	 */
	private File testmodelfile = null;
	
	/**
	 * UML2ModelLoader.
	 */
	private UML2ModelLoader ml = null;
	
	/**
	 * model-resource.
	 */
	private Resource modelres = null;

	/**
	 * loads a model specified by the given string.
	 * @param testmodelname - name of the model to load
	 */
	public final void loadModel(final String testmodelname) {
		testmodeldir = System.getProperty("testmodeldir");
		testmodelfilename = testmodelname;
		testmodelfile = new File(testmodeldir + File.separator
				+ testmodelfilename);
		assertTrue(testmodelfile.exists());

		if (ml == null) {
			ml = new UML2ModelLoader();
		}

		try {
			modelres = ml.load(testmodelfile);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	@Test
	public final void test() {
		loadModel("activityModel.uml");
		assertNotNull(modelres);
		
		UML2ActivityImporter importer = new UML2ActivityImporter(modelres);
		assertNotNull(importer);
		
		//ProcessDescription dataModel = null;

		//UML2ActivityImporter ist nun ein CarismaCheck und hat die Methode doImport nicht mehr.
		//TODO: Neue Testmethode schreiben
		//try {
		//	dataModel = importer.doImport();
		//} catch (ImportException e) {
		//	e.printStackTrace();
		//}
		
		//assertNotNull(dataModel);

		// Zum Test alle Entitaeten einmal ausgeben lassen
		//for (ProcessEntity entity : dataModel.getEntities()) {
		//	assertNotNull(entity);
		//	for (Text text : entity.getTexts()) {
		//		System.out.println(text);
		//		assertNotNull(text);
		//	}
		//}

		// for (Action a : UMLHelper.getAllElementsOfType(model, Action.class))
		// {
		// System.out.println("Action: " + a.getName());
		// System.out.println("Label: " + a.getLabel());
		// System.out.println("EAnnotations: ");
		//
		// for (Comment c : a.getOwnedComments()) {
		// System.out.println(c.getBody());
		// }
		// }
		//
		// for (Comment c : UMLHelper.getAllElementsOfType(model,
		// Comment.class)) {
		// System.out.println("Comment-Body: " + c.getBody());
		//
		// for (Element ae : c.getAnnotatedElements()) {
		// if (ae instanceof Action) {
		// Action annoAction = (Action) ae;
		// System.out.println("Annotated Action Name: "
		// + annoAction.getName());
		// }
		// }
		// }
	}

}
