package carisma.evolution.emfdelta;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.List;

import org.eclipse.emf.compare.diff.metamodel.DiffElement;
import org.eclipse.emf.compare.diff.metamodel.DiffModel;
import org.eclipse.emf.compare.diff.metamodel.DifferenceKind;
import org.eclipse.emf.compare.diff.metamodel.ModelElementChangeLeftTarget;
import org.eclipse.emf.compare.util.ModelUtils;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;

import carisma.evolution.Change;

public final class TestHelper {
	
	/**
	 * This is a shortcut to use EMFDelta in other tests.
	 * @param reffilename A string which contains the name of the reference model file in 'resources/models/'
	 * @param chafilename A string which contains the name of the changed model file in 'resources/models/'
	 * @return A list of Change objects created by EMFDelta
	 */
	public static List<Change> quickEMFDelta(String reffilename, String chafilename) {
		File reffile = null;
		File chafile = null;
		EObject chamodel = null;
		try {		
			String modeldir = "resources/models";
			File dirtest = new File(modeldir);
			assertTrue(dirtest.exists());
			assertTrue(dirtest.isDirectory());
			
			reffile = new File(modeldir + File.separator + reffilename);
			chafile = new File(modeldir + File.separator + chafilename);
			
			// Modeldatein prüfen
			if (!reffile.canRead()) {
				fail("Cant read reference model");
			}
			if (!chafile.canRead()) {
				fail("Cant read changed model");
			}
			ResourceSet resourceSet2 = new ResourceSetImpl();
			chamodel = ModelUtils.load(chafile, resourceSet2);	
		} catch (Exception e) {
			fail("Loading try block failed.");
		}
		EMFDelta emfDeltaInst = new EMFDelta();
		assertTrue(emfDeltaInst != null);
		emfDeltaInst.createDiffModel(reffile, chamodel);
		List<Change> changeList	= emfDeltaInst.generateDeltaDescriptions();
		assertTrue(changeList != null);
		return changeList;
	}
	
	public static DiffModel quickDiffModel(String reffilename, String chafilename) {
		File reffile = null;
		File chafile = null;
		EObject chamodel = null;
		try {		
			String modeldir = "resources/models";
			File dirtest = new File(modeldir);
			assertTrue(dirtest.exists());
			assertTrue(dirtest.isDirectory());
			
			reffile = new File(modeldir + File.separator + reffilename);
			chafile = new File(modeldir + File.separator + chafilename);
			
			// Modeldatein prüfen
			if (!reffile.canRead()) {
				fail("Cant read reference model");
			}
			if (!chafile.canRead()) {
				fail("Cant read changed model");
			}
			ResourceSet resourceSet2 = new ResourceSetImpl();
			chamodel = ModelUtils.load(chafile, resourceSet2);	
		} catch (Exception e) {
			fail("Loading try block failed.");
		}
		EMFDelta emfDeltaInst = new EMFDelta();
		assertTrue(emfDeltaInst != null);
		DiffModel diffmodel = emfDeltaInst.createDiffModel(reffile, chamodel);
		assertTrue(diffmodel != null);
		assertTrue(diffmodel.getDifferences().size() > 0);
		return diffmodel;
	}
	
	public static EObject quickSecrecyStereotypeFromComparison(String reffilename, String chafilename) {
		DiffModel diffmodel	= quickDiffModel(reffilename, chafilename);
		for (DiffElement de : diffmodel.getDifferences()) {
			assertTrue(de != null);
			if (de.getKind().equals(DifferenceKind.ADDITION)) {
				ModelElementChangeLeftTarget mECLT 	= (ModelElementChangeLeftTarget) de;
				assertTrue(mECLT != null);
				EObject element						= mECLT.getLeftElement();
				assertTrue(element != null);
				if (hasAttributeSecrecy(element)) {
					return element;
				}
			}
		}
		fail("Reached end of list of difference without finding the secrecy stereotype element");
		return null;
	}
				
	private static boolean hasAttributeSecrecy(EObject element) {
		for (EAttribute eA : element.eClass().getEAllAttributes()) {
			if (eA.getName().equalsIgnoreCase("secrecy")) {
				return true;		
			}
		}
		return false;
	}
}
