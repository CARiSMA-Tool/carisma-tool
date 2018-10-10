package carisma.ui.eclipse.editors;

import java.util.List;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TransferData;

import carisma.core.analysis.CheckReference;

/**
 * DropAdapter for the list of selected checks.
 * @author jkowald
 */
public class SelectedChecksDropListener extends ViewerDropAdapter {
	
	/**
	 * Location of drop.
	 */
	private int location;
	
	/**
	 * Target of drop (not the dropped Element).
	 */
	private CheckReference target;

	/**
	 * Corresponding TableViewer.
	 */
	private final TableViewer tableViewer;
		
	/**
	 * The AdfEditor instance belonging to the UI instance containing the TableViewer.
	 */
	private AdfEditorController controller;
	
	/**
	 * Constructor.
	 * @param viewer Corresponding TableViewer
	 * @param controller the corresponding AdfEditorController instance
	 */
	protected SelectedChecksDropListener(final Viewer viewer, final AdfEditorController controller) {
		super(viewer);
		this.tableViewer = (TableViewer) viewer;
		this.controller = controller;
	}
	
	@Override
	public final void drop(final DropTargetEvent event) {
		this.location = this.determineLocation(event);
		this.target = (CheckReference) determineTarget(event);
		super.drop(event);
	}

	@Override
	public final boolean performDrop(final Object data) {
		if (data != null) {
			CheckReference dropItem = (CheckReference) data;
			return dropAtTarget(dropItem);
		}
		return false;
	}

	@Override
	public final boolean validateDrop(
			final Object targetObject, 
			final int operation,
			final TransferData transferType) {
		return true;
	}
	
	/**
	 * Drops the item at the determined target location.
	 * @param dropItem The element to drop.
	 * @return Success of the method
	 */
	private boolean dropAtTarget(final CheckReference dropItem) {
		if (dropItem != null) {
			if (this.target != null && this.target.equals(dropItem)) {
				return false;
			}
			int targetIndex = this.controller.getSelectedChecksList().size() - 1;
			if (this.location >= 1 && this.location <= 3) {
				targetIndex = getTargetIndex();
			}
			this.controller.removeCheck(dropItem);
			if (targetIndex >= 0) {
				int insertIndex;
				switch (this.location) {
				case 1: // Before target
					this.controller.getSelectedChecksList().add(targetIndex, dropItem);
					break;
				case 2: // After target
					insertIndex = targetIndex + 1;
					if (insertIndex > this.controller.getSelectedChecksList().size()) {
						insertIndex = this.controller.getSelectedChecksList().size();
					}
					this.controller.getSelectedChecksList().add(insertIndex, dropItem);
					break;
				case 3: // On target
					this.controller.getSelectedChecksList().add(targetIndex, dropItem);
					break;
				default: // Into nothing
					this.controller.getSelectedChecksList().add(dropItem);
				}
				this.tableViewer.refresh();
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Gets the index of the target element in the list of CheckReferences.
	 * @return The index or -1 if the element was not found
	 */
	private int getTargetIndex() {
		List<CheckReference> checkReferenceList = this.controller.getSelectedChecksList(); 
		if (this.target != null && checkReferenceList != null) {
			return checkReferenceList.indexOf(this.target);
		}
		return -1;
	}
}
