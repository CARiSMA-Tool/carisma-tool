package carisma.core.analysis;

import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CheckDescriptor;
import carisma.core.checks.CheckParameter;

/**
 * Interface for UI-Interactions (e.g. Console or Eclipse Instance).
 * Will be used by the AnalyzerObject to interact with the GUI.
 * @author Marcel Michel
 */
public interface UIConnector {

		/**
		 * Called if the ResultView should be updated.
		 */
		void updateView();

		/**
		 * Called for every AskParameter at the beginning of an Analysis.
		 * @param checkDescriptor The description Object of the current Check
		 * @param checkParameter The current AskParameter
		 * @param <T> Generic type for a CheckParameter
		 * @return The UserInput represented as CheckParameter
		 */
		<T extends CheckParameter> T askParameter(CheckDescriptor checkDescriptor, T checkParameter);
		
		/**
		 * Called if the Analyzer sends a Status Message.
		 * @param title The Title of the message
		 * @param message The Message itself
		 * @param type The Type of the message
		 * @param answers The available answers
		 * @param defaultIndex The default Index for the answers
		 * @return The return code of the dialog (See {@link org.eclipse.jface.window.Window.getReturnCode()})
		 */
		int sendMessage(String title, String message, StatusType type, String[] answers, int defaultIndex);
		
}
