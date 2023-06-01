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
package carisma.check.smartcard.authorizedstatus;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Constraint;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Region;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.StateMachine;
import org.eclipse.uml2.uml.Transition;
import org.junit.Test;

import carisma.check.smartcard.TestHelper;
import carisma.check.smartcard.utils.AnalysisMessage;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.UMLStateMachineHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


public class AuthorizedStatusCheckTest {

	private static final String filepath = "resources/models/";
		
	@SuppressWarnings("static-method")
	@Test
	public void testAuthorizedStatus() {
		Model model = TestHelper.loadModel(AuthorizedStatusCheckTest.filepath, "testStateMachine.uml");
		assertNotNull(model.getAppliedProfile("UMLsec"));
		Package p1 = (Package) model.getMember("testStateMachine");
		assertNotNull(p1);
		Class cl = (Class) p1.getMember("Class1");
		assertNotNull(cl);
		StateMachine sm = (StateMachine) cl.getMember("StateMachine");
		assertNotNull(sm);
		Region r = sm.getRegion("Region");
		assertNotNull(r);
		State s = (State) r.getMember("State1");
		assertNotNull(s);
		assertTrue(UMLsecUtil.hasStereotype(s, UMLsec.AUTHORIZED_STATUS));
		StereotypeApplication stapp = UMLHelper.getStereotypeApplication(s, "authorized-status");
		assertNotNull(stapp);
		TaggedValue permissionTag = stapp.getTaggedValue("permission");
		assertNotNull(permissionTag);
		String permissionValue = (String) permissionTag.getValue();
		assertEquals("Dudel",permissionValue);
		assertEquals(1, s.getIncomings().size());
		Transition t = s.getIncoming("Uebergang");
		assertNotNull(t);
		Constraint c = t.getGuard();
		assertNotNull(c);
		OpaqueExpression guard = UMLStateMachineHelper.getGuard(c);
		assertNotNull(guard);
		for (String body : guard.getBodies()) {
			assertTrue(body.contains(permissionValue));
			assertEquals(c, guard.getOwner());
		}
		assertEquals(t, c.getOwner());
		assertEquals(s, t.getTarget());
		AuthorizedStatus check = new AuthorizedStatus();
		assertEquals(1,check.checkAllAuthorizedStates(model));
		permissionTag.setValue("Rums Rummel Dudel");
		assertEquals(0, check.checkAllAuthorizedStates(model));
		t.setGuard(null);
		assertEquals(1, check.checkAllAuthorizedStates(model));
		for (AnalysisMessage error : check.getErrorMessages()) {
			Logger.log(LogLevel.ERROR, error.getMessage());
		}
		permissionTag.setValue("");
		assertEquals(0, check.checkAllAuthorizedStates(model));
	}

}
