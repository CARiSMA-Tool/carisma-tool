package carisma.regulatory.ruleallocator.tests;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import org.junit.Before;
import org.junit.Test;

import carisma.regulatory.ruleallocator.AllocationController;
import carisma.regulatory.ruleallocator.DatamodelManager;
import carisma.regulatory.ruleallocator.datamodel.Allocation;
import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.RuleElement;

/**
 * All AllocationController tests.
 * @author jkowald
 *
 */
public class AllocationControllerTest {
	
	/**
	 * Local path to the test data.
	 */
	private String testDataPath = "resources";
	
	/**
	 * Instance of the DatamodelManager to load the files for the AllocationController.
	 */
	private DatamodelManager datamodelManager;
	
	/**
	 * Instance of the AllocationController which will be tested.
	 */
	private AllocationController allocationController;
	
	@Before
	/**
	 * Instantiates the DatamodelManager and the AllocationController and
	 * sets the absolute path to the test data.
	 */
	public void construct() {
		datamodelManager = new DatamodelManager();
		allocationController = new AllocationController(datamodelManager);
		File absolutPathFile = new File(testDataPath);
		testDataPath = absolutPathFile.getAbsolutePath() + File.separator;
	}
	
	@Test
	/**
	 * Tests the allocate and the removeAllocation methods of the
	 * AllocationController.
	 */
	public void testAllocateAndRemoveAllocation() {
		datamodelManager.loadFile(testDataPath + "exampleRules.xmi");
		Container container = datamodelManager.getContainer();
		assertNotNull(container);
		assertTrue(container.getContainsAllocation().size() == 1);
		
		BPMNElement bpmnElement = getBPMNElementWithName("Worker");
		RuleElement ruleElementNotFitting = getRuleElementWithName("Initiate Payment");
		Allocation allocationNull = allocationController.allocate(ruleElementNotFitting, bpmnElement);
		assertNull(allocationNull);
		assertTrue(container.getContainsAllocation().size() == 1);
		
		RuleElement ruleElementFitting = getRuleElementWithName("Manager");
		Allocation allocation = allocationController.allocate(ruleElementFitting, bpmnElement);
		assertNotNull(allocation);
		assertTrue(container.getContainsAllocation().size() == 2);
		
		allocationController.removeAllocation(allocation);
		assertTrue(container.getContainsAllocation().size() == 1);
	}
	
	/**
	 * Helping method to get a BPMNElement with a specific name.
	 * @param name The name, the method is searching for
	 * @return The BPMNElement which owns the specific name or null if none is found
	 */
	private BPMNElement getBPMNElementWithName(String name) {
		Container container = datamodelManager.getContainer();
		for (Object bpmnElementObject : container.getContainsBPMNElement()) {
			BPMNElement bpmnElement = (BPMNElement) bpmnElementObject;
			if (bpmnElement.getName().equalsIgnoreCase(name)) {
				return bpmnElement;
			}
		}
		return null;
	}
	
	/**
	 * Helping method to get a RuleElement with a specific name.
	 * @param name The name, the method is searching for
	 * @return The RuleElement which owns the specific name or null if none is found
	 */
	private RuleElement getRuleElementWithName(String name) {
		Container container = datamodelManager.getContainer();
		for (Object ruleElementObject : container.getContainsRuleElement()) {
			RuleElement ruleElement = (RuleElement) ruleElementObject;
			if (ruleElement.getName().equalsIgnoreCase(name)) {
				return ruleElement;
			}
		}
		return null;
	}
}