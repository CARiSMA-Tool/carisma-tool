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
package carisma.core.checks;

import java.util.ArrayList;
import java.util.List;

/**
 * Description of an analysis check.
 * @author wenzel
 *
 */
public class CheckDescriptor {

	private String id;
	private String name;
	private String publisher;
	private String targetModelType;
	private String implementingClass;
	private String description;
	private String magicKeys;
	private List<CheckParameterDescriptor> parameters;
	private List<String> preconditions;
	private List<String> postconditions;

	public CheckDescriptor(String id, String name, String publisher,
			String targetModelType, String implementingClass, String description, String magicKeys) {
		super();
		this.id = id;
		this.name = name;
		this.publisher = publisher;
		this.targetModelType = targetModelType;
		if (targetModelType == null || targetModelType.trim().equals("")) {
			this.targetModelType = "*";
		}
		this.implementingClass = implementingClass;
		this.description = description;
		this.magicKeys = magicKeys;
		this.parameters = new ArrayList<>();
		this.preconditions = new ArrayList<>();
		this.postconditions = new ArrayList<>();
	}

	public String getCheckDescriptorId() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public String getPublisher() {
		return this.publisher;
	}

	public String getTargetModelType() {
		return this.targetModelType;
	}

	public String getImplementingClass() {
		return this.implementingClass;
	}

	public String getDescription() {
		return this.description;
	}

	public String getMagicKeys() {
		return this.magicKeys;
	}

	public List<CheckParameterDescriptor> getParameters() {
		return this.parameters;
	}

	public List<String> getRequiredKeys() {
		return this.preconditions;
	}

	public List<String> getProvidedKeys() {
		return this.postconditions;
	}

	public CheckParameterDescriptor getParameterByID(String paramID) {
		for (CheckParameterDescriptor cpd : getParameters()) {
			if (cpd.getID().equalsIgnoreCase(paramID)) {
				return cpd;
			}
		}
		return null;
	}
	
	@Override
	public String toString() {
		return this.id;
	}
	
/*	@Override
	public String toString() {
		String params = "";
		for (CheckParameterDescriptor ppd : getParameters()) {
			params += ppd.toString();
		}
		
		return "CheckDescriptor [id=" + id + ", name=" + name + ", publisher="
				+ publisher + ", targetModelType=" + targetModelType
				+ ", implementingClass=" + implementingClass + ", description="
				+ description + ", parameters=" + params + "]";
	} */
}
