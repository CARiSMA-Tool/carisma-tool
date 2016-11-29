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
package carisma.check.bpmn2.ocl;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.eclipse.bpmn2.Bpmn2Package;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;

import carisma.modeltype.bpmn2.extended.ExtendedPackage;
import carisma.modeltype.bpmn2.extension.ExtensionPackage;
import carisma.ocl.library.OclLibrary;


/**
 * This class contains some reusable methods and
 * stored variables.
 * @author m.michel
 */
public class OclHelper {

	/**
	 * Maps string to context class.
	 * Important for OCL context expression
	 */
	private Map<String, EClass> mapContext;
    {
        Map<String, EClass> bpmnMap = new HashMap<>();
        for (EObject obj : Bpmn2Package.eINSTANCE.eContents()) {
        	if (obj instanceof EClass) {
        		bpmnMap.put(((EClass) obj).getName().toLowerCase(Locale.ENGLISH), (EClass) obj);
        	}
        }
        this.mapContext = Collections.unmodifiableMap(bpmnMap);
    }
    
    /**
	 * Maps string to extended context class.
	 * Important for extended OCL context expression
	 */
	private final Map<String, EClass> mapExtendedContext;
    {
        Map<String, EClass> bpmnMap = new HashMap<>();
        bpmnMap.put("lane", ExtendedPackage.eINSTANCE.getExtendedLane());
        bpmnMap.put("task", ExtendedPackage.eINSTANCE.getExtendedTask());
        bpmnMap.put("process", ExtendedPackage.eINSTANCE.getExtendedProcess());
        bpmnMap.put("selection", ExtensionPackage.eINSTANCE.getSelection());
        bpmnMap.put("taskset", ExtensionPackage.eINSTANCE.getTaskSet());
        this.mapExtendedContext = Collections.unmodifiableMap(bpmnMap);
    }
	
	/**
	 * Loads an OCL-Library.
	 * 
	 * @param file The OCL-Library File
	 * @return If successful the method returns an OCL-Library otherwise null 
	 * @throws IOException If the OCL-Library could not be loaded
	 */
	public final static OclLibrary getOclLibrary(final File file) throws IOException {
		URI uri = URI.createFileURI(file.getAbsolutePath());
		ResourceSet resourceSet = new ResourceSetImpl();
		Resource resource = resourceSet.getResource(uri, true);
		resource.load(new HashMap<String, Object>());
		
		EObject content = resource.getContents().get(0);
		if (content instanceof OclLibrary) { 
			return (OclLibrary) resource.getContents().get(0);
		}
		return null;
	}
	
	/**
	 * Maps the context string to a bpmn2 instance. 
	 * 
	 * @param context The context class represented as string
	 * @return returns the context class as instance.
	 * 	If the context string could not be resolved the method returns null
	 */
	public final EClass getContextClass(final String context) {
		if (this.mapContext.containsKey(context.toLowerCase(Locale.ENGLISH))) {
			return this.mapContext.get(context.toLowerCase(Locale.ENGLISH));
		}
		return null;
	}
	
	/**
	 * Maps the context string to a bpmn2extended instance. 
	 * 
	 * @param context The context class represented as string
	 * @return returns the context class as instance.
	 * 	If the context string could not be resolved the method returns null
	 */
	public final EClass getExtendedContextClass(final String context) {
		if (this.mapExtendedContext.containsKey(context.toLowerCase(Locale.ENGLISH))) {
			return this.mapExtendedContext.get(context.toLowerCase(Locale.ENGLISH));
		} else if (this.mapContext.containsKey(context.toLowerCase(Locale.ENGLISH))) {
			return this.mapContext.get(context.toLowerCase(Locale.ENGLISH));
		} else {
			return null;
		}
	}
}
