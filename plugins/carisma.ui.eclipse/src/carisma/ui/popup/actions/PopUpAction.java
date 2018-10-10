package carisma.ui.popup.actions;

import org.eclipse.jface.action.IMenuManager;

import carisma.core.analysis.result.AnalysisResult;

public interface PopUpAction {

	
	public boolean perform(final IMenuManager manager, final AnalysisResult analysisResult);
}
