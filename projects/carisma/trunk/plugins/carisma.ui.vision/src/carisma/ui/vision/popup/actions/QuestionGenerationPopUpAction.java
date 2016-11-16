package carisma.ui.vision.popup.actions;


import java.util.List;

import org.eclipse.jface.action.IMenuManager;

import carisma.core.analysis.result.AnalysisResult;

import carisma.ui.popup.actions.PopUpAction;
import carisma.ui.vision.questions.Builder;
import carisma.ui.vision.questions.BuilderFactory;
import carisma.ui.vision.questions.QuestionGenerationAction;


public class QuestionGenerationPopUpAction implements PopUpAction {

	public QuestionGenerationPopUpAction() {
		
	}
	

	@Override
	public boolean perform(IMenuManager manager, AnalysisResult analysisResult) {
		
		QuestionGenerationAction questionGenerationAction = new QuestionGenerationAction(analysisResult);
		
		questionGenerationAction.setText("Generate Question");
		questionGenerationAction.setEnabled(false);
		
		//enable the generate question button if there is a builder
		BuilderFactory builderfactory = new BuilderFactory();
		List<Builder> builder = builderfactory.getBuilder(analysisResult);
		if (builder.size() >= 1){
			questionGenerationAction.setEnabled(true);
		};
		
		manager.add(questionGenerationAction);
		return false;
	}
	
}
