package carisma.ui.eclipse.editors;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;

import carisma.core.analysis.CheckReference;

/**
 * DragSourceListener for the list of selected checks.
 * @author jkowald
 */
public class SelectedChecksDragListener implements DragSourceListener {
	
	/**
	 * Corresponding TableViewer.
	 */
	private final TableViewer tableViewer;
		
	/**
	 * Constructor.
	 * @param tableViewer The corresponding TableViewer.
	 */
	public SelectedChecksDragListener(final TableViewer tableViewer) {
		this.tableViewer = tableViewer;
	}

	@Override
	public final void dragStart(final DragSourceEvent event) { }

	@Override
	public final void dragSetData(final DragSourceEvent event) {
		IStructuredSelection selection = (IStructuredSelection) this.tableViewer.getSelection();
		CheckReference firstElement = (CheckReference) selection.getFirstElement();
		event.data = firstElement;
	}

	@Override
	public final void dragFinished(final DragSourceEvent event) { }

}
