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
/**
* This package owns a class to analyze a state machine with respect to authorized-status.
*  it seeks for every state in the statemachine and proofs if there is a guard on every incoming transition
*  an if it owns the permission string of the stereotype authorized-status
*/
package carisma.check.smartcard.authorizedstatus;
