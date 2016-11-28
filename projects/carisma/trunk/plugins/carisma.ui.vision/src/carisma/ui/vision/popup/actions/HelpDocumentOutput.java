package carisma.ui.vision.popup.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;

import carisma.core.analysis.result.AnalysisResult;
import carisma.core.analysis.result.CheckResult;
import carisma.ui.popup.actions.PopUpAction;
import carisma.ui.vision.automatedanalysis.AutomatedAnalysis;

public class HelpDocumentOutput implements PopUpAction {

	@Override
	public boolean perform(final IMenuManager manager, final AnalysisResult analysisResult) {
		Action action4 = new Action() {
			@Override
			public void run() {
				super.run();
				AutomatedAnalysis.startAutomatedAnalysis(analysisResult);
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
