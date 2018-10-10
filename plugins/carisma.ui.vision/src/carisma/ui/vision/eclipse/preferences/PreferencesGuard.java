package carisma.ui.vision.eclipse.preferences;

import java.security.Guard;

import carisma.ui.vision.popup.actions.VisiOnDBOutput;
import carisma.ui.vision.questions.QuestionGenerationAction;
import carisma.vision.dbAccess.dbAccess;


public final class PreferencesGuard implements Guard {

	private static final Class<?>[] allowedClasses = {dbAccess.class, VisiOnDBOutput.class, QuestionGenerationAction.class};
	
	@Override
	public void checkGuard(Object accessingObject) throws SecurityException {
		//TODO: Dummy impl
	}

}
