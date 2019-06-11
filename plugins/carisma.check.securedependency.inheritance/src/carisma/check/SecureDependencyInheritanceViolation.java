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
package carisma.check;

import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Property;

public class SecureDependencyInheritanceViolation {
    
	public Dependency getDependency() {
        return this.dependency;
    }

    public Classifier getClient() {
        return this.client;
    }

    public Classifier getSupplier() {
        return this.supplier;
    }

    public String getDescription() {
        return this.description;
    }
    public Classifier getClassifier() {
    	return this.classifier;
    }
    public Operation getOverriding_operation() {
    	return this.overriding_operation;
    }
    public Property getAttribute() {
    	return this.overriding_attribute;
    }

    private Dependency dependency;
	private  Classifier client;
	private Classifier supplier;
	private String description;
	private Classifier classifier; 
	private Operation overriding_operation;
	private Property overriding_attribute;
	
	
	public SecureDependencyInheritanceViolation(String description, Dependency dependency, Classifier client, Classifier supplier) {
		super();
		this.dependency = dependency;
		this.client = client;
		this.supplier = supplier;
		this.description = description;
	}
	public SecureDependencyInheritanceViolation(String description, Classifier classifier, Operation overriding_operation) {
		super();
		this.description = description;
		this.classifier = classifier;
		this.overriding_operation = overriding_operation;
	}
	public SecureDependencyInheritanceViolation(String description, Classifier classifier, Property overriding_attribute) {
		super();
		this.description = description;
		this.classifier = classifier;
		this.overriding_attribute = overriding_attribute;
	}
}