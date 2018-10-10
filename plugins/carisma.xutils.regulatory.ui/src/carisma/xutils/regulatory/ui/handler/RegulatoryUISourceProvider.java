/*
 * not used for now. Helper Method to change the Perspective.
 * Probably usefully for further development and actually replaced by a
 * visibleWhen -> with(activeWorkbenchWindow.activePerspective) -> 
 * equals(<<Perspective.ID>>) 
 * Notation
 * 
 * DB
 */


//package carisma.xutils.regulatory.ui.handler;
//
//import java.util.HashMap;
//import java.util.Map;
//
//import org.eclipse.ui.AbstractSourceProvider;
//import org.eclipse.ui.ISourceProvider;
//import org.eclipse.ui.ISourceProviderListener;
//import org.eclipse.ui.ISources;
//
//public class RegulatoryUISourceProvider extends AbstractSourceProvider {
//	
//	public final static String uiPerspectiveKey =
//				"carisma.xutils.regulatory.ui.handler.ShowActions";
//	private final static String uiPerspective = "showActions";
//	private final static String otherPerspective = "hideActions";
//	
//	private boolean isUIPerspective = false;
//	private static RegulatoryUISourceProvider instance;
//	
//	public RegulatoryUISourceProvider() {
//		super();
//		instance = this;
//	}
//	
//	@Override
//	public void dispose() {
//		// TODO Auto-generated method stub
//
//	}
//	
//	public static RegulatoryUISourceProvider getInstance() {
//		return instance;
//	}
//
//	@Override
//	public Map<String, String> getCurrentState() {
//		Map<String, String> currentState = new HashMap<String, String>();
//		String state = isUIPerspective ? uiPerspective : otherPerspective;
//		currentState.put(uiPerspectiveKey, state);
//		return currentState;
//	}
//
//	@Override
//	public String[] getProvidedSourceNames() {
//		return new String[] {uiPerspectiveKey};
//	}
//	
//	public void perspectiveChanged(boolean _isUIPerspective) {
//		if (this.isUIPerspective == _isUIPerspective) { return; /* no change */ }
//		this.isUIPerspective = _isUIPerspective;
//		String currentState = isUIPerspective ? uiPerspective : otherPerspective;
//		fireSourceChanged(ISources.WORKBENCH, uiPerspectiveKey, currentState);
//	}
//
//}
