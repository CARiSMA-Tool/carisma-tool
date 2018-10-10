package carisma.ui.console;

import carisma.core.analysis.UIConnector;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CheckDescriptor;
import carisma.core.checks.CheckParameter;

public class ConsoleUIConnector implements UIConnector {

	@Override
	public void updateView() {
		// TODO nothing to do?
	}

	@Override
	public <T extends CheckParameter> T askParameter(CheckDescriptor checkDescriptor, T checkParameter) {
		System.err.println("WARNING: Asking for parameter!");
		//TODO implement
		return checkParameter;
	}

	@Override
	public int sendMessage(String title, String message, StatusType type, String[] answers, int defaultIndex) {
		System.err.println("WARNING: Sending message!");
		//TODO implement
		return defaultIndex;
	}

}
