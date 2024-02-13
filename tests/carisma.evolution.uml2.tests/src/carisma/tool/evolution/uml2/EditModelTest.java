package carisma.tool.evolution.uml2;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.ControlFlow;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.OpaqueAction;
import org.eclipse.uml2.uml.Package;
import org.junit.After;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.uml2.UMLModifier;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;

/**
 * this JUnit test contains tests according to the stereotype <<edit>> for the UMLModifier.
 * @author Klaus Rudack
 *
 */
public class EditModelTest {

    /**
     * a UML2ModelLoader.
     */
    private ResourceSet rs = new ResourceSetImpl();
    
    /**
     * the model-resource.
     */
    private Resource modelres = null;
    
    /**
     * Name of an Element.
     */
    private static final String NEW_NAME = "NewName";
    
    /**
     * Error Message.
     */
    private static final String ERROR_NEW_NAME_NOT_FOUND = "Element '" + NEW_NAME + "' of type 'OpaqueAction' not found in ";  /**
    
     * Error Message.
     */
    private static final String ERROR_TARGET1_NOT_FOUND = "Element 'Target1' of type 'OpaqueAction' not found in ";  
    
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
     * this test generates editElements and tests if a model will be edited correctly.
     * @throws IOException 
     */
    @Test
    public final void testEditsForModel() throws IOException {
        loadModel("testCreateEditsForModel.uml");
        String controlFlow = "ControlFlow2";
        try {
            Model theOldModel = (Model) this.modelres.getContents().get(0);
            UMLModifier um;
            Model theNewModel;
            
            ControlFlow oldControlFlow2 = 
                    UMLHelper.getElementOfNameAndType(theOldModel, controlFlow, ControlFlow.class);
            assertNotNull(oldControlFlow2);
            EditElement editControlFlow2 = new EditElement(oldControlFlow2);
            editControlFlow2.addKeyValuePair("target", "Target2");
            editControlFlow2.addKeyValuePair("visibility", "private");
            List<DeltaElement> deltaContent = new ArrayList<>();
            deltaContent.add(editControlFlow2);
            OpaqueAction target1InOldModel = 
                    UMLHelper.getElementOfNameAndType(theOldModel, "Target1", OpaqueAction.class);
            EditElement editTarget1 = new EditElement(target1InOldModel);
            editTarget1.addKeyValuePair("name", NEW_NAME);
            deltaContent.add(editTarget1);
            Delta d = new Delta(deltaContent);
            um = new UMLModifier(this.modelres, d);
            theNewModel = um.getModifiedModel();
            ControlFlow newControlFlow2 =
                    UMLHelper.getElementOfNameAndType(theNewModel, "ControlFlow2", ControlFlow.class);
            assertNotNull(newControlFlow2);
            assertNotSame(oldControlFlow2, newControlFlow2);
            assertNotSame(oldControlFlow2.getVisibility(), newControlFlow2.getVisibility());
            if (wrappGetElementOfNameAndType(theOldModel, NEW_NAME, OpaqueAction.class) != null) {
                fail(ERROR_NEW_NAME_NOT_FOUND + theOldModel.getName());
            }
            
            if (wrappGetElementOfNameAndType(theNewModel, "Target1", OpaqueAction.class) != null) {
              fail(ERROR_TARGET1_NOT_FOUND + theNewModel.getName());
           }
            try {
                if (UMLHelper.getElementOfNameAndType(theNewModel, NEW_NAME, OpaqueAction.class) == null) {
                    fail(ERROR_NEW_NAME_NOT_FOUND + theNewModel.getName());  
                }
            } catch (ModelElementNotFoundException e) {
                fail(ERROR_NEW_NAME_NOT_FOUND + theNewModel.getName());
            }
        } catch (ModelElementNotFoundException e) {
            Logger.log(LogLevel.ERROR, "", e);
            fail(e.getMessage());       
        }
    }
    
    /** Little helper Method which catches the original Exception and returns null instead.
     * This method is only called on expected null return value. 
     * 
     * @param pkg the package to search
     * @param name the name of the element to look for
     * @param type the class of the element to look for
     * @param <T> generic return Type.
     * @return the found Object or null.
     */
    private static <T extends NamedElement> T wrappGetElementOfNameAndType(final Package pkg, final String name, final Class<T> type) {
        try {
           return UMLHelper.getElementOfNameAndType(pkg, name, type);
        } catch (ModelElementNotFoundException e) {
            return null;
        }
    }
    
    
    /**
     * tests what happens with wrong edits.
     * @throws IOException 
     */
    @Test
    public final void testIncorrectEdits() throws IOException {
        loadModel("testCreateEditsForModel.uml");
        String controlFlow = "ControlFlow2";
        try {
            Model theOldModel = (Model) this.modelres.getContents().get(0);
            UMLModifier um;
            Model theNewModel;
            ControlFlow target = (ControlFlow) UMLHelper.getElementByName(theOldModel, controlFlow);
            assertNotNull(target);
            assertEquals("public", target.getVisibility().getLiteral());
            ControlFlow newTarget = null;
            try {
                newTarget = UMLHelper.getElementOfNameAndType(theOldModel, "newCF", ControlFlow.class);
            } catch (ModelElementNotFoundException e) {
                assertNull(newTarget);              
            }
            assertNull(newTarget);
            EditElement editElement1 = new EditElement(target);
            editElement1.addKeyValuePair("name", "newCF");
            editElement1.addKeyValuePair("visibility", "private");
            List<DeltaElement> delList = new ArrayList<>();
            delList.add(editElement1);
            Delta d = new Delta(delList);
            um = new UMLModifier(this.modelres, d);
            theNewModel = um.getModifiedModel();
            target = null;
            try {
                target = (ControlFlow) UMLHelper.getElementByName(theNewModel, controlFlow);
            } catch (ModelElementNotFoundException e) {
                assertNull(target);             
            }
            assertNull(target);
            newTarget = (ControlFlow) UMLHelper.getElementByName(theNewModel, "newCF");
            assertEquals("private", newTarget.getVisibility().getLiteral());
            assertNotNull(newTarget);
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
