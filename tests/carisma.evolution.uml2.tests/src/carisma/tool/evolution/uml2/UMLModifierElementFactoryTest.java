package carisma.tool.evolution.uml2;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Collections;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.UMLFactory;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.uml2.UMLModifierElementFactory;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;


/**
 * JUnit test to test UMLModifierElementFactory.
 * @author Klaus Rudack  edited by bberghoff
 *
 */
public class UMLModifierElementFactoryTest {

	
	/**
	 * UMLFactory to create UML-Elements.
	 */
	private final UMLFactory umlfactory = UMLFactory.eINSTANCE;
	

	/**
	 * a UML2ModelLoader.
	 */
	private ResourceSet rs = new ResourceSetImpl();
	
	/**
	 * the model-resource.
	 */
	private Resource modelres = null;
	
	
	/**
	 * loads the given model.
	 * @param testmodelname - the model to load
	 * @throws IOException 
	 */
	private void loadModel(final String testmodelname) throws IOException {
		String testmodeldir = "resources/models/modifier";
		File testmodelfile = new File(testmodeldir + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
	}
	
	/**
	 * this test tests the method insertContainmentRelationship(...) in the UMLModifierElementFactory.
	 * @throws IOException 
	 */
	@Test
	@Ignore
	public final void test() throws IOException {
		loadModel("ActivityTest.uml");
		try {
			Model originalModel = (Model) this.modelres.getContents().get(0);
			assertNotNull(originalModel);
			Element a = UMLHelper.getElementByName(originalModel, "ActivityTestActivity");
			assertNotNull(a);
			Element o = this.umlfactory.createOpaqueAction();
			((NamedElement) o).setName(o.getClass().getName());
			if (o.getOwner() != null) {
				Logger.log(LogLevel.DEBUG, o.getOwner().toString());
			} else {
				Logger.log(LogLevel.DEBUG, "null");
			}
			assertEquals(2, ((Activity) a).getNodes().size());

		    UMLModifierElementFactory.insertContainmentRelationship(a, o);
		    
			assertEquals(3, ((Activity) a).getNodes().size());
			Logger.log(LogLevel.DEBUG, o.getOwner().toString());
			while (a.getOwner() != null) {
				a = a.getOwner();
				Logger.log(LogLevel.DEBUG, a.toString());
			}
			Logger.log(LogLevel.DEBUG, "================");
			for (Element e : originalModel.allOwnedElements()) {
				Logger.log(LogLevel.DEBUG, e.toString());
			}
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}		
	}
	
	@After
	public void unloadModel(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}
}
