package carisma.regulatory.ruleallocator;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

public class Perspective implements IPerspectiveFactory {
	
	public static String ID = "carisma.regulatory.ruleallocator.perspective";

	@Override
	public void createInitialLayout(IPageLayout layout) {
		layout.setEditorAreaVisible(false);
		layout.setFixed(true);
		
		IFolderLayout folderLayout = layout.createFolder("whole", IPageLayout.LEFT, 0.95f, "");
		
		folderLayout.addView("carisma.regulatory.ruleallocator.ruleallocatorview");
		layout.getViewLayout("carisma.regulatory.ruleallocator.ruleallocatorview").setCloseable(false);
	}
}
