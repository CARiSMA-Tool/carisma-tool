package carisma.profile.umlchange;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.IOException;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Profile;
import org.eclipse.uml2.uml.Stereotype;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

public class ProfileEnumConformanceTest {

	@SuppressWarnings("static-method")
	@Test
	public void test() {
		Profile profile = null;
		URI profileUri = URI.createURI(UMLChangeActivator.UML_FILE);
		ResourceSet resourceSet = new ResourceSetImpl();
		Resource profileResource = resourceSet.getResource(profileUri, true);
		try {
			profileResource.load(null);
			for (EObject anObject : profileResource.getContents()) {
				if (anObject instanceof Profile) {
					profile = (Profile) anObject;
					break;
				}
			}
			assertNotNull("Profile not found!", profile);
			
			for (Stereotype stereo : profile.getOwnedStereotypes()) {
				if (UMLchange.getValue(stereo.getName()) == null) {
					fail("Profile has non-implemented stereotype " + stereo.getName() + ".");							
				}
			}
			for (UMLchange stereo : UMLchange.values()) {
				if (profile.getOwnedStereotype(stereo.toString()) == null) {
					fail("Enumeration has non-profile stereotype " + stereo.toString() + ".");												
				}
			}
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, "Couldn't find profile UMLChange.", e);
			fail();
		}
	}
}

