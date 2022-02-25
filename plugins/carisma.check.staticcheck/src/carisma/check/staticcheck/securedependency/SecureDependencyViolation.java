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

import java.util.Collection;

import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Element;

public class SecureDependencyViolation {

	private final Dependency dependency;
	private final Classifier client;
	private final Classifier supplier;
	private final Element violating;
	private final String description;
	private final Collection<String> signatures;
	private final String securityLevel;

	public SecureDependencyViolation(final String description, final Dependency dependency, final Classifier client, final Classifier supplier,
			final Collection<String> signature, final String property, final Element violating) {
		this.dependency = dependency;
		this.client = client;
		this.supplier = supplier;
		this.description = description;
		this.signatures = signature;
		this.securityLevel = property;
		this.violating = violating;
	}

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


	public Element getViolatingElement() {
		return this.violating;
	}

	public Collection<String> getReleventSignatures() {
		return this.signatures;
	}

	public String getSecurityLevel() {
		return this.securityLevel;
	}
}