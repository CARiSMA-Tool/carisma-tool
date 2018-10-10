package carisma.ui.eclipse.editors;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import carisma.core.analysis.CheckReference;
import carisma.core.checks.CheckDescriptor;
import carisma.ui.eclipse.CarismaGUI;

/**
 * LabelProvider for the ListViewer for the list of selected checks.
 */
public class SelectedChecksLabelProvider extends LabelProvider implements ITableLabelProvider {
	@Override
	public final String getColumnText(final Object obj, final int index) {
		if (obj instanceof CheckReference) {
			CheckDescriptor checkDescriptor = CarismaGUI.getCheckRegistry().
					getCheckDescriptor(((CheckReference) obj).getCheckID());
			if (checkDescriptor != null) {
				return checkDescriptor.getName();
			}
		}
		return obj.toString();
	}
	
	@Override
	public final Image getColumnImage(final Object obj, final int index) {
		if (obj instanceof CheckReference) {
			CheckDescriptor checkDescriptor = 
				CarismaGUI.getCheckRegistry().getCheckDescriptor(((CheckReference) obj).getCheckID());
			if (checkDescriptor == null) {
				return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_ERROR_TSK);
			} 
			return PlatformUI.getWorkbench().getSharedImages().getImage(
					ISharedImages.IMG_OBJ_ELEMENT);
		}
		return PlatformUI.getWorkbench().getSharedImages().getImage(
				ISharedImages.IMG_OBJ_FILE);
	}
}
