package carisma.regulatory.ruleallocator.tests;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.Resource.Factory.Registry;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.junit.Before;
import org.junit.Test;

import carisma.regulatory.ruleallocator.DatamodelManager;
import carisma.regulatory.ruleallocator.datamodel.Allocation;
import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.DatamodelFactory;
import carisma.regulatory.ruleallocator.datamodel.RuleElement;
import carisma.regulatory.ruleallocator.datamodel.RuleElementType;
import carisma.regulatory.ruleallocator.datamodel.Situation;

public class DatamodelManagerTest {
	
	/**
	 * Instance of the DatamodelManager, which is the base of the test.
	 */
	private DatamodelManager datamodelManager;
	
	/**
	 * Path to the test data.
	 */
	private String testDataPath = "resources" + File.separator;
	
	/**
	 * A temporary RuleElementType object which is created in the tests.
	 */
	private RuleElementType testRuleElementType;
	
	/**
	 * A temporary RuleElement object which is created in the tests.
	 */
	private RuleElement testRuleElement;
	
	/**
	 * A temporary BPMNElement object which is created in the tests.
	 */
	private BPMNElement testBPMNElement;
	
	/**
	 * A temporary Situation object which is created in the tests.
	 */
	private Situation testSituation;
	
	/**
	 * A temporary Allocation object which is created in the tests.
	 */
	private Allocation testAllocation;
	
	@Before
	/**
	 * Instantiates the DatamodelManager.
	 */
	public void construct() {
		datamodelManager = new DatamodelManager();
	}
	
	@Test
	/**
	 * Tests, if the constructor of the DatamodelManager has set the variables right.
	 */
	public void ConstructorTest() {
		// This method also tests getContainer()
		assertTrue(datamodelManager.getContainer() instanceof Container);
		assertTrue(datamodelManager.getFilepath().isEmpty());
		assertTrue(datamodelManager.getResource() == null);
		Registry registry = Registry.INSTANCE;
		Map<String, Object> map = registry.getExtensionToFactoryMap();
		assertTrue(map.containsKey("xmi"));
		assertTrue(map.containsKey("temp"));
		Object valueXmi = map.get("xmi");
		assertTrue(valueXmi instanceof XMIResourceFactoryImpl);
		Object valueTemp = map.get("temp");
		assertTrue(valueTemp instanceof XMIResourceFactoryImpl);
	}
	
	@Test
	/**
	 * Tests primary the loadFile method and secondarly the getResource
	 * and getFilepath methods.
	 */
	public void loadFileTest() {
		String testFilePath = testDataPath + "exampleRules.xmi";
		File testFile = new File(testFilePath);
		assertTrue(testFile.exists());
		testFilePath = testFile.getAbsolutePath();
		assertTrue(datamodelManager.loadFile(testFilePath));
		assertTrue(datamodelManager.getResource() instanceof Resource);
		assertTrue(datamodelManager.getFilepath().equalsIgnoreCase(testFilePath));
		Container container = datamodelManager.getContainer();
		assertTrue(container.getContainsAllocation().size() == 1);
		assertTrue(container.getContainsBPMNElement().size() == 1);
		assertTrue(container.getContainsModelType().size() == 25);
		assertTrue(container.getContainsRuleElement().size() == 4);
		assertTrue(container.getContainsRuleElementAssociation().size() == 0);
		assertTrue(container.getContainsRuleElementAssociationType().size() == 0);
		assertTrue(container.getContainsRuleElementType().size() == 5);
		assertTrue(container.getContainsSituation().size() == 1);
	}
	
	@Test
	/**
	 * Tests primary the saveFile method and secondarly the loadFile,
	 * and getContainer methods.
	 */
	public void saveFileTest() {
		// This method also tests getContainer(), loadFile()
		
		// Duplicate the test xmi file
		String testFilePath = testDataPath + "exampleRules.xmi";
		File testFile = new File(testFilePath);
		assertTrue(testFile.exists());
		File testFileDuplicate = new File(testDataPath + "exampleRulesDuplicate.xmi");
		assertFalse(testFileDuplicate.exists());
		try {
			FileUtils.copyFile(testFile, testFileDuplicate);
		} catch (IOException ioe) {
			fail(ioe.getMessage());
		}
		assertTrue(testFileDuplicate.exists());
		
		// Load the duplicate
		String testFileDuplicatePath = testFileDuplicate.getAbsolutePath();
		assertTrue(datamodelManager.loadFile(testFileDuplicatePath));
		Container container = datamodelManager.getContainer();
		
		// Create some new test content
		assertTrue(createTestElements(container));
		
		// Save the new test content
		datamodelManager.saveFile();
		
		datamodelManager = new DatamodelManager();
		
		// Load the updated duplicated test xmi file
		assertTrue(datamodelManager.loadFile(testFileDuplicatePath));
		
		// Check the content
		container = datamodelManager.getContainer();
		assertTrue(container.getContainsAllocation().size() == 2);
		assertTrue(container.getContainsBPMNElement().size() == 2);
		assertTrue(container.getContainsModelType().size() == 25);
		assertTrue(container.getContainsRuleElement().size() == 5);
		assertTrue(container.getContainsRuleElementAssociation().size() == 0);
		assertTrue(container.getContainsRuleElementAssociationType().size() == 0);
		assertTrue(container.getContainsRuleElementType().size() == 6);
		assertTrue(container.getContainsSituation().size() == 2);
				
		testFileDuplicate.delete();
	}

	@Test
	/**
	 * Tests the isContainerFromFile method.
	 */
	public void isContainerFromFileTest() {
		assertFalse(datamodelManager.isContainerFromFile());
		String testFilePath = testDataPath + "exampleRules.xmi";
		File testFile = new File(testFilePath);
		assertTrue(testFile.exists());
		testFilePath = testFile.getAbsolutePath();
		assertTrue(datamodelManager.loadFile(testFilePath));
		assertTrue(datamodelManager.isContainerFromFile());
	}
	
	@Test
	/**
	 * Tests the createYaoqiangTempfile and deleteYaoqiangTempfile
	 * methods.
	 */
	public void testCreateAndDeleteYaoqiangTempfile() {
		String tempFilePath = testDataPath + "yaoqiangTest.temp";
		File yaoqiangTestFile = new File(tempFilePath);
		assertFalse(yaoqiangTestFile.exists());
		datamodelManager.createYaoqiangTempfile(tempFilePath);
		assertTrue(yaoqiangTestFile.exists());
		datamodelManager.deleteYaoqiangTempfile(tempFilePath);
		assertFalse(yaoqiangTestFile.exists());
	}
	
	@Test
	/**
	 * Tests the readYaoqiangTempfile method.
	 */
	public void testReadYaoqiangTempfile() {
		String tempFilePath = testDataPath + "exampleYaoqiangTempfile.temp";
		File yaoqiangTestFile = new File(tempFilePath);
		tempFilePath = yaoqiangTestFile.getAbsolutePath();
		BPMNElement yaoqiangBPMNElement = datamodelManager.readYaoqiangTempfile(tempFilePath);
		assertNotNull(yaoqiangBPMNElement);
		assertTrue(yaoqiangBPMNElement.getName().equals("Authorize Payment"));
		assertTrue(yaoqiangBPMNElement.getID().equals("_7"));
	}
	
	@SuppressWarnings("unchecked")
	/**
	 * Helping method to create some test elements for the xmi file.
	 * @param container The container which is loaded from the xmi file
	 * @return A boolean which indicates the success of the creation
	 */
	private boolean createTestElements(Container container) {
		try {
			DatamodelFactory datamodelFactory = DatamodelFactory.eINSTANCE;
			
			// Add a RuleElementType
			testRuleElementType = datamodelFactory.createRuleElementType();
			testRuleElementType.setName("newTestRuleElementType");
			container.getContainsRuleElementType().add(testRuleElementType);
			
			// Add a RuleElement
			testRuleElement = datamodelFactory.createRuleElement();
			testRuleElement.setName("newTestRuleElement");
			testRuleElement.setType(testRuleElementType);
			container.getContainsRuleElement().add(testRuleElement);
			
			// Add a BPMNElement
			testBPMNElement = datamodelFactory.createBPMNElement();
			testBPMNElement.setName("newTestBPMNElement");
			testBPMNElement.setID("_1909");
			container.getContainsBPMNElement().add(testBPMNElement);
			
			// Add a Situation
			testSituation = datamodelFactory.createSituation();
			testSituation.setName("newTestSituation");
			testSituation.getHas().add(testRuleElement);
			container.getContainsSituation().add(testSituation);
			
			// Add a Allocation
			testAllocation = datamodelFactory.createAllocation();
			testAllocation.setBpmnElement(testBPMNElement);
			testAllocation.setRuleElement(testRuleElement);
			container.getContainsAllocation().add(testAllocation);
			
			return true;
		} catch (Exception e) {
			fail(e.getMessage());
		}
		return false;
	}
}
