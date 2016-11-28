package carisma.ui.popup.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;

import carisma.core.analysis.result.AnalysisResult;
import carisma.ui.eclipse.CarismaGUI;

public class HtmlOutput implements PopUpAction {

	@Override
	public boolean perform(final IMenuManager manager, final AnalysisResult analysisResult) {
		Action action = new Action() {
			@Override
			public void run() {
				super.run();
				CarismaGUI.openReport(analysisResult);
			}
		};
		action.setText("Create report for selected analysis");
		manager.add(action);
		
		return true;
	}

}
