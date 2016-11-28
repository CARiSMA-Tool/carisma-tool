/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/

package carisma.modeltype.bpmn2;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import carisma.modeltype.bpmn2.yaoqiang.YaoqiangHelper;

/**
 * Test class.
 * 
 * @author Marcel Michel
 * 
 */
public class BPMN2HelperTest {

	/**
	 * The relative filepath to the model directory.
	 */
	private String filepath = "resources/models/";

	/**
	 * Tests the isisYaoqiangModel method.
	 */
	@Test
	public void isYaoqiangModelTest() {
		assertFalse(YaoqiangHelper.isYaoqiangModel(this.filepath + "ThisFileDoesNotExist"));
		
		assertTrue(YaoqiangHelper.isYaoqiangModel(this.filepath + "yaoqiang.bpmn"));
		assertFalse(YaoqiangHelper.isYaoqiangModel(this.filepath + "nonYaoqiang.bpmn"));
	}
	
	/**
	 * Tests the yaoqiang2emfModel method.
	 * The transformed model does not longer contain the 
	 * targetElement and sourceElement tags (redundant information)
	 */
	@Test
	public void yaoqiang2emfModelTest() {
		String output = YaoqiangHelper.yaoqiang2emfModel(this.filepath + "yaoqiang.bpmn");
		assertFalse(output.contains("targetElement"));
		assertFalse(output.contains("sourceElement"));
		
		output = YaoqiangHelper.yaoqiang2emfModel(this.filepath + "ThisFileDoesNotExist");
		assertTrue(output.equals(""));
	}
}
