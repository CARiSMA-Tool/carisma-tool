package carisma.check.rabac.tests;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;

import org.eclipse.emf.common.EMFPlugin;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.junit.Before;
import org.junit.Test;

import carisma.check.rabac.RABACCheck;
import carisma.check.rabac.RABACTest;
import carisma.check.rabac.TestHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckParameterDescriptor;

public class RABACValidModelsTest implements RABACTest {

	public Resource model = null;
	public StringBuilder report;

	HashMap<String, CheckParameter> config = new HashMap<>();
	private final String filepath = "resources" + File.separator + "models";

	@Before
	public void setUp() {
		// required when a stand-alone EMF environment is used
		if (!EMFPlugin.IS_ECLIPSE_RUNNING) {
			fail("Execute as plugin test");
		}

		this.report = new StringBuilder();
	}

	@Test
	public void testValidRABACModel() throws IOException {
		final File file = new File(new File(this.filepath), "valid.uml");
		this.model = new ResourceSetImpl().createResource(URI.createFileURI(file.toString()));
		try(FileInputStream stream = new FileInputStream(file)){
			this.model.load(stream, Collections.emptyMap());
			this.config.put(RABACCheck.PARAM_CONFIGURATION, new InputFileParameter(new CheckParameterDescriptor(null, null,
					null, null, false, null), new File("resources" + File.separator + "rabac_configuration-valid.xml")));

			final RABACCheck rabacCheck = new RABACCheck();
			final TestHost host = new TestHost(this);
			assertTrue(rabacCheck.perform(this.config, host));
			assertTrue(this.report.toString().contains("No errors have been detected."));
		}
	}

	@Override
	public Resource getModel() {
		return this.model;
	}

	@Override
	public StringBuilder getReport() {
		return this.report;
	}

}