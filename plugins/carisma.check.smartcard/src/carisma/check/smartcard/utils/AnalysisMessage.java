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
package carisma.check.smartcard.utils;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class AnalysisMessage {
	private StatusType type = null;
	private OutputTarget target = null;
	private String message = null;
	
	public AnalysisMessage(
			final StatusType messageType, final OutputTarget messageTarget, final String newMessage) {
		this.type = messageType;
		this.target = messageTarget;
		this.message = newMessage;
	}
	
	public StatusType getType() {
		return this.type;
	}
	
	public OutputTarget getTarget() {
		return this.target;
	}
	
	public String getMessage() {
		return this.message;
	}

	public void print(final AnalysisHost host, final String usedPrefix) {
		String prefix = "";
		if (usedPrefix != null) {
			prefix = usedPrefix;
		}
		if (host != null) {
			if (this.target == OutputTarget.BOTH) {
				host.addResultMessage(new AnalysisResultMessage(this.type, prefix + this.message));
				host.appendLineToReport(prefix + this.message);
				return;
			}
			if (this.target == OutputTarget.DETAIL) {
				host.addResultMessage(new AnalysisResultMessage(this.type, prefix + this.message));
				return;
			}
			if (this.target == OutputTarget.REPORT) {
				host.appendLineToReport(prefix + this.message);
				return;
			}
		}
	}
}
