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
package carisma.core.models;

import java.io.File;
import java.io.IOException;

import org.eclipse.emf.ecore.resource.Resource;

/**
 * Describes a class for loading models.
 * @author wenzel
 *
 */
@Deprecated
public interface ModelLoader {
	
	Resource load(File file) throws IOException;
	
}
