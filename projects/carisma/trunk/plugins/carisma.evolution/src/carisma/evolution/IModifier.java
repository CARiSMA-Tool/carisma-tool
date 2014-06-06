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
package carisma.evolution;

import org.eclipse.emf.ecore.resource.Resource;
/**
 * A modifier is able to modify a model according to
 * a set of DeltaElements. Once initialized with a model,
 * it also provides the ability to copy a delta to represent
 * the model elements from the model copy.
 * @author Daniel Warzecha
 *
 */
public interface IModifier {
	
	Resource edit(Resource oldModel, Delta delta);
	
	Delta copyDelta(final Delta oldDelta);
}

