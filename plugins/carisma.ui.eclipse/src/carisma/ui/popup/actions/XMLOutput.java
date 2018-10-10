package carisma.ui.popup.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;

import carisma.core.analysis.result.AnalysisResult;
import carisma.ui.eclipse.CarismaGUI;

public class XMLOutput implements PopUpAction {

	@Override
	public boolean perform(final IMenuManager manager, final AnalysisResult analysisResult) {
		/*
		 * initializing xml output menu.
		 * 
		 */
		Action action2 = new Action() {
			@Override
			public void run() {
				super.run();
				CarismaGUI.saveXml(analysisResult);
			}
		};
		action2.setText("Create XML-Output for selected analysis");
		manager.add(action2);
		
		return true;
	}

}
