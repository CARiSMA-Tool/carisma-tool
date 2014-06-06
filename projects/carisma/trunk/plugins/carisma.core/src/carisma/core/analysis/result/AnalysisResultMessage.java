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


/**
 * @author buerger
 * Represents results of an analysis which has just run.
 */
public class AnalysisResultMessage{
	private String text;
	private StatusType status;
	private String modelElement;
	private String additionalInformation;
	private CheckResult parent;

	public AnalysisResultMessage(String text, StatusType status,
			String modelElement, String additionalInformation) {
		super();
		this.text = text;
		this.status = status;
		this.modelElement = modelElement;
		this.additionalInformation = additionalInformation;
	}

	public AnalysisResultMessage(StatusType status, String text) {
		this(text, status, null, null);
	}
	
	public String getText() {
		return text;
	}
	public void setText(String text) {
		this.text = text;
	}
	public StatusType getStatus() {
		return status;
	}
	public void setStatus(StatusType status) {
		this.status = status;
	}
	public String getModelElement() {
		return modelElement;
	}
	public void setModelElement(String modelElement) {
		this.modelElement = modelElement;
	}
	public String getAdditionalInformation() {
		return additionalInformation;
	}
	public void setAdditionalInformation(String additionalInformation) {
		this.additionalInformation = additionalInformation;
	}
	void setParent(CheckResult parent) {
		this.parent = parent;
	}
	public CheckResult getParent() {
		return parent;
	}


}
