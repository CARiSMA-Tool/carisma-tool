package carisma.ui.eclipse;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.FloatParameter;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.IntegerParameter;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.UIConnector;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CheckDescriptor;
import carisma.core.checks.CheckParameter;
import carisma.ui.eclipse.dialogs.parameters.ParameterDialogUtil;

/**
 * Eclipse UI Connector.
 * @author Marcel Michel
 */
public class EclipseUIConnector implements UIConnector {

	/**
	 * Updates the Result View.
	 */
	@Override
	public final void updateView() {
		CarismaGUI.showAnalysisResultsView();
	}

	/**
	 * Opens a dialog for each ask parameter and converts
	 * the user input to a CheckParameter.
	 * @param checkDescriptor The description Object of the current Check
	 * @param checkParameter The current AskParameter
	 * @param <T> Generic type for a CheckParameter
	 * @return The UserInput represented as CheckParameter
	 */
	@Override
	public final <T extends CheckParameter> T askParameter(
			final CheckDescriptor checkDescriptor, final T checkParameter) {
		ParameterDialogUtil parameterDialogUtil = new ParameterDialogUtil(new Shell(), checkDescriptor);
		
		if (checkParameter instanceof BooleanParameter) {
			((BooleanParameter) checkParameter).setValue(parameterDialogUtil.queryBooleanParameter((BooleanParameter) checkParameter));
			return checkParameter;
		} else if (checkParameter instanceof StringParameter) {
			((StringParameter) checkParameter).setValue(parameterDialogUtil.queryStringParameter((StringParameter) checkParameter));
		} else if (checkParameter instanceof IntegerParameter) {
			((IntegerParameter) checkParameter).setValue(parameterDialogUtil.queryIntegerParameter((IntegerParameter) checkParameter));
		} else if (checkParameter instanceof FloatParameter) {
			((FloatParameter) checkParameter).setValue(parameterDialogUtil.queryFloatParameter((FloatParameter) checkParameter));
		} else if (checkParameter instanceof InputFileParameter) {
			((InputFileParameter) checkParameter).setValue(parameterDialogUtil.queryInputFileParameter((InputFileParameter) checkParameter));
		} else if (checkParameter instanceof FolderParameter) {
			((FolderParameter) checkParameter).setValue(parameterDialogUtil.queryFolderParameter((FolderParameter) checkParameter));
		} else if (checkParameter instanceof OutputFileParameter) {
			((OutputFileParameter) checkParameter).setValue(parameterDialogUtil.queryOutputFileParameter((OutputFileParameter) checkParameter));
		}
		return checkParameter;
	}

	/**
	 * Method will open a standard Message Dialog for
	 * Status Update Messages.
	 * @param title The Title of the message
	 * @param message The Message itself
	 * @param type The Type of the message
	 * @param answers The available answers
	 * @param defaultIndex The default Index for the answers
	 * @return The return code of the dialog
	 */
	@Override
	public final int sendMessage(final String title, final String message, final StatusType type,
			final String[] answers, final int defaultIndex) {
		
		Display display = Display.getDefault();
		Shell shell = new Shell(display);
		
		int dialogType = MessageDialog.NONE;
		if (type.equals(StatusType.ERROR)) {
			dialogType = MessageDialog.ERROR;
		} else if (type.equals(StatusType.WARNING)) {
			dialogType = MessageDialog.WARNING;
		} else if (type.equals(StatusType.INFO)) {
			dialogType = MessageDialog.INFORMATION;
		}
		
		MessageDialog mDialog = new MessageDialog(shell,
				title, null, message, dialogType, 
				answers, defaultIndex);
		mDialog.open();
		return mDialog.getReturnCode();
	}

}
