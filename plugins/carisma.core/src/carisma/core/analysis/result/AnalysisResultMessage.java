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
package carisma.core.analysis.result;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * @author buerger
 * Represents results of an analysis which has just run.
 */
@XmlRootElement(name = "AnalysisResultMessage")
@XmlType(propOrder = { "status","text","modelElement","additionalInformation"})
public class AnalysisResultMessage{
	private String text;
	private StatusType status;
	private String modelElement;
	private String additionalInformation;
	private CheckResult parent;

	public AnalysisResultMessage(final String text, final StatusType status,
			final String modelElement, final String additionalInformation) {
		this.text = text;
		this.status = status;
		this.modelElement = modelElement;
		this.additionalInformation = additionalInformation;
	}

	public AnalysisResultMessage(final StatusType status, final String text) {
		this(text, status, null, null);
	}

	public AnalysisResultMessage(){

	}


	@XmlElement(name = "text")
	public String getText() {
		return this.text;
	}
	public void setText(final String text) {
		this.text = text;
	}
	@XmlElement(name = "status")
	public StatusType getStatus() {
		return this.status;
	}
	public void setStatus(final StatusType status) {
		this.status = status;
	}
	public String getModelElement() {
		return this.modelElement;
	}
	public void setModelElement(final String modelElement) {
		this.modelElement = modelElement;
	}
	public String getAdditionalInformation() {
		return this.additionalInformation;
	}
	public void setAdditionalInformation(final String additionalInformation) {
		this.additionalInformation = additionalInformation;
	}
	void setParent(final CheckResult parent) {
		this.parent = parent;
	}
	public CheckResult getParent() {
		return this.parent;
	}


}
