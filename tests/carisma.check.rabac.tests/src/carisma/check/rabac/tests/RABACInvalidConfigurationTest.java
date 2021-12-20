package carisma.check.rabac.tests;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;

import org.eclipse.emf.common.EMFPlugin;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.junit.Before;
import org.junit.Test;

import carisma.check.rabac.RABACCheck;
import carisma.check.rabac.RABACTest;
import carisma.check.rabac.TestHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckParameterDescriptor;

public class RABACInvalidConfigurationTest implements RABACTest {

	private final static String filepath = "resources" + File.separator + "models";

	private Resource model = null;
	private StringBuilder report;

	TestHost host = new TestHost(this);
	HashMap<String, CheckParameter> config = new HashMap<>();
	
	@Before
	public void setUp() throws FileNotFoundException, IOException {
		// required when a stand-alone EMF environment is used
		if (!EMFPlugin.IS_ECLIPSE_RUNNING) {
			fail("Execute as plugin test");
		}

		this.report = new StringBuilder();
		
		ResourceSet rs = new ResourceSetImpl();
		File file = new File(new File(RABACInvalidConfigurationTest.filepath), "valid.uml");
		this.model = rs.createResource(URI.createFileURI(file.toString()));
		try(FileInputStream inputStream = new FileInputStream(file)){
			this.model.load(inputStream, Collections.EMPTY_MAP);
		}
	}

	@Test
	public void testInvalidRABACConfiguration() {
		this.config.put(RABACCheck.PARAM_CONFIGURATION, new InputFileParameter(new CheckParameterDescriptor(null, null,
				null, null, false, null), new File("resources" + File.separator + "missing")));

		assertFalse(new RABACCheck().perform(this.config, this.host));

		this.config.put(RABACCheck.PARAM_CONFIGURATION, new InputFileParameter(new CheckParameterDescriptor(null, null,
				null, null, false, null), new File("resources" + File.separator + "rabac_configuration-invalid.xml")));

		assertFalse(new RABACCheck().perform(this.config, this.host));
	}

	@Override
	public StringBuilder getReport() {
		return this.report;
	}

	@Override
	public Resource getModel() {
		return this.model;
	}

}