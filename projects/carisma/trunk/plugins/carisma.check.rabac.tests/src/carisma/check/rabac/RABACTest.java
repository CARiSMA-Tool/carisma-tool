package carisma.check.rabac;

import java.io.File;
import java.util.HashMap;

import junit.framework.TestCase;

import org.eclipse.emf.common.EMFPlugin;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.URIConverter;
import org.eclipse.uml2.uml.UMLPackage;
import org.eclipse.uml2.uml.resource.UMLResource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.UserAbortedAnalysisException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckParameterDescriptor;
import carisma.tests.modelutils.uml.TestHelper;

public class RABACTest extends TestCase {

	private class TestHost implements AnalysisHost {
		@Override
		public void addResultMessage(AnalysisResultMessage detail) {
			System.out.println(detail.getText());
		}

		@Override
		public void appendToReport(String text) {
		}

		@Override
		public void appendLineToReport(String text) {
			report.append(text);
		}

		@Override
		public Resource getAnalyzedModel() {
			return model;
		}

		@Override
		public String getCurrentModelFilename() {
			return "";
		}

		@Override
		public void putToRegister(String registerName, Object data) throws RegisterInUseException {
		}

		@Override
		public boolean isRegisterInUse(String registerName) {
			return false;
		}

		@Override
		public Object getFromRegister(String registerName) throws RegisterNotInUseException {
			return null;
		}

		@Override
		public Object removeFromRegister(String registerName) throws RegisterNotInUseException {
			return null;
		}

		@Override
		public void displayError(String message) {
		}

		@Override
		public File getFileToBeWritten(File file) throws UserAbortedAnalysisException {
			return file;
		}
	}

	private Resource model = null;
	private StringBuilder report;

	TestHost host = new TestHost();
	HashMap<String, CheckParameter> config = new HashMap<String, CheckParameter>();
	private String filepath = "resources" + File.separator + "models";

	@Override
	protected void setUp() {
		// required when a stand-alone EMF environment is used
		if (!EMFPlugin.IS_ECLIPSE_RUNNING) {
			Resource.Factory.Registry.INSTANCE.getExtensionToFactoryMap().put(UMLResource.FILE_EXTENSION,
					UMLResource.Factory.INSTANCE);
			UMLPackage.eINSTANCE.getClass();
			URIConverter.URI_MAP.put(URI.createURI("platform:/plugin/carisma.profile.umlsec.rabac/profile/"), URI
					.createFileURI(new File("../carisma.profile.umlsec.rabac/profile/").getAbsolutePath())
					.appendSegment(""));
		}

		report = new StringBuilder();
	}

	public void testInvalidRABACModels() {
		model = TestHelper.loadModel(filepath, "invalid1.uml").eResource();
		config.put("carisma.check.rabac.configuration", new InputFileParameter(new CheckParameterDescriptor(null, null,
				null, null, false, null), new File("resources" + File.separator + "rabac_configuration-valid.xml")));

		assertFalse(new RABACCheck().perform(config, host));

		model = TestHelper.loadModel(filepath, "invalid2.uml").eResource();

		assertFalse(new RABACCheck().perform(config, host));

		model = TestHelper.loadModel(filepath, "invalid3.uml").eResource();

		assertFalse(new RABACCheck().perform(config, host));
	}

	public void testInvalidRABACConfiguration() {
		model = TestHelper.loadModel(filepath, "valid.uml").eResource();
		config.put("carisma.check.rabac.configuration", new InputFileParameter(new CheckParameterDescriptor(null, null,
				null, null, false, null), new File("resources" + File.separator + "missing")));

		assertFalse(new RABACCheck().perform(config, host));

		config.put("carisma.check.rabac.configuration", new InputFileParameter(new CheckParameterDescriptor(null, null,
				null, null, false, null), new File("resources" + File.separator + "rabac_configuration-invalid.xml")));

		assertFalse(new RABACCheck().perform(config, host));
	}

	public void testValidRABACModel() {
		model = TestHelper.loadModel(filepath, "valid.uml").eResource();
		config.put("carisma.check.rabac.configuration", new InputFileParameter(new CheckParameterDescriptor(null, null,
				null, null, false, null), new File("resources" + File.separator + "rabac_configuration-valid.xml")));

		assertTrue(new RABACCheck().perform(config, host));
		assertTrue(report.toString().contains("Operation"));
	}

}