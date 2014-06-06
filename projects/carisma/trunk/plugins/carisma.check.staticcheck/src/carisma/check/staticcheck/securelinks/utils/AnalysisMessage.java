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
package carisma.check.staticcheck.securelinks.utils;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class AnalysisMessage {
	private StatusType type = null;
	private OutputTarget target = null;
	private String message = null;
	
	public AnalysisMessage(
			final StatusType messageType, final OutputTarget messageTarget, final String newMessage) {
		type = messageType;
		target = messageTarget;
		message = newMessage;
	}
	
	public StatusType getType() {
		return type;
	}
	
	public OutputTarget getTarget() {
		return target;
	}
	
	public String getMessage() {
		return message;
	}

	public void print(final AnalysisHost host) {
		if (host != null) {
			if (target == OutputTarget.BOTH) {
				host.addResultMessage(new AnalysisResultMessage(type, message));
				host.appendLineToReport(message);
				return;
			}
			if (target == OutputTarget.DETAIL) {
				host.addResultMessage(new AnalysisResultMessage(type, message));
				return;
			}
			if (target == OutputTarget.REPORT) {
				host.appendLineToReport(message);
				return;
			}
		}
	}
}
