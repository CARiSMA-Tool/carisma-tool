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
package carisma.modeltype.owl2.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

/**
 * 
 * @author Marcel Michel
 */
public class ClassNameMapper {

	private final EPackage ePackage;
	
	private Map<String, EClass> mapContext;
    
    public ClassNameMapper(EPackage ePackage) {
    	this.ePackage = ePackage;
    	initHashMap();
    }

	private void initHashMap() {
		Map<String, EClass> map = new HashMap<String, EClass>();
        for (EObject obj : ePackage.eContents()) {
        	if (obj instanceof EClass) {
        		map.put(((EClass) obj).getName().toLowerCase(Locale.ENGLISH), (EClass) obj);
        	}
        }
        mapContext = Collections.unmodifiableMap(map);
	}
	
	public EClass getEClassByName(String name) {
		if (mapContext.containsKey(name.toLowerCase(Locale.ENGLISH))) {
			return mapContext.get(name.toLowerCase(Locale.ENGLISH));
		} else {
			return null;
		}
	}
}
