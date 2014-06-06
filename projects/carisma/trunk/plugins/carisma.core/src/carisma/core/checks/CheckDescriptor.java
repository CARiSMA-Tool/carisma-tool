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
		this.parameters = new ArrayList<CheckParameterDescriptor>();
		this.preconditions = new ArrayList<String>();
		this.postconditions = new ArrayList<String>();
	}

	public String getCheckDescriptorId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public String getPublisher() {
		return publisher;
	}

	public String getTargetModelType() {
		return targetModelType;
	}

	public String getImplementingClass() {
		return implementingClass;
	}

	public String getDescription() {
		return description;
	}

	public String getMagicKeys() {
		return magicKeys;
	}

	public List<CheckParameterDescriptor> getParameters() {
		return parameters;
	}

	public List<String> getRequiredKeys() {
		return preconditions;
	}

	public List<String> getProvidedKeys() {
		return postconditions;
	}

	public CheckParameterDescriptor getParameterByID(String id) {
		for (CheckParameterDescriptor cpd : getParameters()) {
			if (cpd.getID().equalsIgnoreCase(id)) {
				return cpd;
			}
		}
		return null;
	}
	
	@Override
	public String toString() {
		return id;
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
