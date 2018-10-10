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
package carisma.check.staticcheck.securedependency;

import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.Dependency;

public class SecureDependencyViolation {
    
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

    private Dependency dependency;
	private  Classifier client;
	private Classifier supplier;
	private String description;
	
	public SecureDependencyViolation(String description, Dependency dependency, Classifier client, Classifier supplier) {
		super();
		this.dependency = dependency;
		this.client = client;
		this.supplier = supplier;
		this.description = description;
	}
}