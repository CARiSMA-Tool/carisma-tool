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

/**
 * Description of a parameter of an analysis check.
 * @author wenzel
 *
 */
public class CheckParameterDescriptor {

	private String id;
	private String name;
	private String description;
	private ParameterType type;
	private boolean optional;
	private String defaultValue;

    /**
     * Constant value used as basis for hashCode().
     */
    private static final int INITIAL_HASH_VALUE = 17;
    /**
     * Constant value to calculate hashCode().
     */
    private static final int HASH_FACTOR = 37;
	
	public CheckParameterDescriptor(String id, String name, String description, ParameterType type,
			boolean optional, String value) {
		// name, description and defaultValue are optional, but not allowed to be null
		// see Ticket Bug #1519
		super();
		this.id = id;
		if (name != null) {
			this.name = name;
		} else {
			this.name = "";
		}
		if (name != null) {
			this.description = description;
		} else {
			this.description = "";
		}
		this.type = type;
		this.optional = optional;
		if (value != null) {
			this.defaultValue = value;
		} else {
			this.defaultValue = "";
		}
	}
	
	public String getID() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public String getDescription() {
		return this.description;
	}
	
	public ParameterType getType() {
		return this.type;
	}
	
	public boolean isOptional() {
		return this.optional;
	}

	public String getDefaultValue() {
		return this.defaultValue;
	}
	@Override
	public String toString() {
		return "CheckParameterDescriptor [id=" + this.id + ", name=" + this.name + ", type=" + this.type
				+ ", optional=" + this.optional + "]";
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (obj instanceof CheckParameterDescriptor) {
			CheckParameterDescriptor other = (CheckParameterDescriptor) obj;
			return other.getID().equals(getID()) && other.getType().equals(getType());
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		int result = INITIAL_HASH_VALUE;
		result = HASH_FACTOR * result + ((this.id == null) ? 0 : this.id.hashCode());
		result = HASH_FACTOR * result + ((this.name == null) ? 0 : this.name.hashCode());
		result = HASH_FACTOR * result + ((this.description == null) ? 0 : this.description.hashCode());
		result = HASH_FACTOR * result + ((this.type == null) ? 0 : this.type.hashCode());
		result = HASH_FACTOR * result + (((this.optional) ? 0 : 1));
		result = HASH_FACTOR * result + ((this.defaultValue == null) ? 0 : this.defaultValue.hashCode());
		
		return result;
	}
	
}
