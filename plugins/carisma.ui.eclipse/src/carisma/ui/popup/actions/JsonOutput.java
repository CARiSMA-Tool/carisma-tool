package carisma.ui.popup.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;

import carisma.core.analysis.result.AnalysisResult;
import carisma.ui.eclipse.CarismaGUI;

public class JsonOutput implements PopUpAction {

	@Override
	public boolean perform(IMenuManager manager, AnalysisResult analysisResult) {
		Action action = new Action() {
			@Override
			public void run() {
				super.run();
				CarismaGUI.saveJson(analysisResult);
			}
		};
		action.setText("Create JSON report for this analysis");
		manager.add(action);
		return true;
	}

}
