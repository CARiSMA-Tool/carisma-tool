package carisma.ui.vision.popup.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;

import carisma.core.analysis.result.AnalysisResult;
import carisma.core.analysis.result.CheckResult;
import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.popup.actions.PopUpAction;

public class HelpDocumentOutput implements PopUpAction {

	@Override
	public boolean perform(final IMenuManager manager, final AnalysisResult analysisResult) {
		Action action4 = new Action() {
			public void run() {
				super.run();
				CarismaGUI.INSTANCE.startAutomatedAnalysis(analysisResult);
			}
		};
		action4.setEnabled(false);
		for (CheckResult chkR : analysisResult.getCheckResults()) {
			if (chkR.getName().compareTo("Create Help Document for STS mapping") == 0) { // TODO:
																							// Call
																							// Name
																							// Method
																							// for
																							// compare
				action4.setEnabled(true);
			}
		}
		action4.setText("Create automated analysis from help document");
		manager.add(action4);
		
		return true;
	}

}
