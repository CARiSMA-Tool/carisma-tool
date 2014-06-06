package carisma.xutils.regulatory.ui.first;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

import carisma.xutils.regulatory.ui.model.Constants;

// TODO: Auto-generated Javadoc
/**
 * The Class Perspective.
 */
public class Perspective implements IPerspectiveFactory {
	
	/** The Constant ID. */
	public final static String ID = "carisma.xutils.regulatory.ui.first.Perspective";

    /* (non-Javadoc)
     * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
     */
    public void createInitialLayout(IPageLayout layout) {

        IFolderLayout folder = layout.createFolder(Constants.PATH_FIRST,
                IPageLayout.LEFT, 0.33f, layout.getEditorArea());

        folder.addView(Constants.CLS_REGULATIONS);
        folder.addView(Constants.CLS_CONFIGURATION);

//         layout.addView("carisma.xutils.regulatory.ui.first.Regularien", IPageLayout.LEFT,
//         0.4f,
//         layout.getEditorArea());
//         // IViewLayout.setClosable();
//        
//         layout.addView("carisma.xutils.regulatory.ui.first.Konfiguration", IPageLayout.LEFT,
//         0.4f, layout.getEditorArea());

        layout.setEditorAreaVisible(false);

    }
}
