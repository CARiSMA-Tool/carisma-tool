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
* This package owns a class to analyze an activity-diagram  with respect to fair exchange.
*  it seeks through any possible path backwards, will analyze a path as not correct if it finds the start element
*  before it finds a stop element
*/
package carisma.check.staticcheck.fairexchange;
