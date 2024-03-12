package carisma.check.rabac.tests;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.eclipse.emf.common.EMFPlugin;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import carisma.check.rabac.RABACCheck;
import carisma.check.rabac.RABACTest;
import carisma.check.rabac.TestHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckParameterDescriptor;

import java.util.logging.Logger;


@RunWith(Parameterized.class)
public class RABACInvalidModelsTest implements RABACTest {
	
	private static final Logger logger = Logger.getLogger(RABACInvalidModelsTest.class.getName());
	
	private final static String filepath = "resources" + File.separator + "models";

	private Resource model = null;
	private StringBuilder report;

	private HashMap<String, CheckParameter> config = new HashMap<>();

	public RABACInvalidModelsTest(Resource testModel) {
		this.model = testModel;
	}
	
	@Before
	public void setUp() {
		// required when a stand-alone EMF environment is used
		if (!EMFPlugin.IS_ECLIPSE_RUNNING) {
			fail("Execute as plugin test");
		}

		this.report = new StringBuilder();
	}
	
	@Parameters
	public static Collection<Resource> collectParameters(){
		ResourceSet rs = new ResourceSetImpl();
		
		List<Resource> resources = new ArrayList<>();
		
		File folder = new File(RABACInvalidModelsTest.filepath);
		String[] list = folder.list(new FilenameFilter() {
			
			@Override
			public boolean accept(File dir, String name) {
				return name.endsWith(".uml") && name.contains("invalid");
			}
		});
		for(String file : list){
			Resource r = rs.createResource(URI.createFileURI(file));
			try (FileInputStream inputStream = new FileInputStream(new File(folder, file))){
				r.load(inputStream, Collections.EMPTY_MAP);
				resources.add(r);
			} catch (Exception e) {
				logger.warning("Error message: " + e.getMessage());
			} 
		}
		return resources;
	}

	@Test
	public void testInvalidRABACModels() {
		this.config.put(RABACCheck.PARAM_CONFIGURATION, new InputFileParameter(new CheckParameterDescriptor(null, null,
				null, null, false, null), new File("resources" + File.separator + "rabac_configuration-valid.xml")));

		assertFalse(new RABACCheck().perform(this.config, new TestHost(this)));
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