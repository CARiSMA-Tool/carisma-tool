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
* This package owns classes to get all possible paths through a given statemachine.
*  it proofs any state what kind of state it is and has specified methods for each possible state
*  like states, composite states, or pseudostates like history states or parallelisation and calculates
*  all possible next states in that way
*  it wont work for historys in parallelisation correctly yet
*/
package carisma.modeltype.uml2.statemachine;
