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
import org.eclipse.uml2.uml.Association;
import org.eclipse.uml2.uml.AssociationClass;
import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;


public class DeletingRelationsTest {
	
	private ResourceSet rs = new ResourceSetImpl();
	
	private Resource modelres = null;
	
	/**
	 * loads the given model.
	 * @param testmodelname - the model to load
	 * @throws IOException 
	 */
	private void loadModel(final String testmodelname) throws IOException {
		String testmodeldir = "resources/models/modifier";
		File testmodelfile = new File(testmodeldir + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
	}

	@Test
	public void testDeleteRelationsByDestroying() throws IOException {
		loadModel("nary.uml");
		try {
			assertNotNull(this.modelres);
			Model theModel = (Model) this.modelres.getContents().get(0);
			assertNotNull(theModel);
			Class class0 = (Class) UMLHelper.getElementByName(theModel, "Class0");
			assertNotNull(class0);
			Class class1 = (Class) UMLHelper.getElementByName(theModel, "Class1");
			assertNotNull(class1);
			Association cl01 = (Association) UMLHelper.getElementByName(theModel, "class0_class1_0");
			assertNotNull(cl01);
			AssociationClass ac01 = (AssociationClass) UMLHelper.getElementByName(theModel, "AssociationClass0");
			assertNotNull(ac01);
			class0.destroy();
			assertTrue(class0.getRelationships().isEmpty());
			assertTrue(ac01.getRelatedElements().isEmpty());
			Class class3 = (Class) UMLHelper.getElementByName(theModel, "Class3");
			assertNotNull(class3);
			Class class4 = (Class) UMLHelper.getElementByName(theModel, "Class4");
			assertNotNull(class4);
			Dependency dep0 = (Dependency) UMLHelper.getElementByName(theModel, "Dependency0");
			assertNotNull(dep0);
			assertEquals(1, dep0.getClients().size());
			assertEquals(2, dep0.getSuppliers().size());
			assertTrue(class3.getRelationships().contains(dep0));
			class3.getSourceDirectedRelationships().contains(dep0);
			class4.destroy();
			assertTrue(class4.getRelationships().isEmpty());
			assertEquals(1, dep0.getClients().size());
			assertEquals(1, dep0.getSuppliers().size());
			assertTrue(class3.getRelationships().contains(dep0));
			class3.getSourceDirectedRelationships().contains(dep0);
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
