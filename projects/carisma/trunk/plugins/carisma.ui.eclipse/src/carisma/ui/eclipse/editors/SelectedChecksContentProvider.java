package carisma.ui.eclipse.editors;

import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;

import carisma.core.analysis.CheckReference;

/**
 * ContentProvider for the ListViewer for the list of selected checks.
 */
public class SelectedChecksContentProvider extends ArrayContentProvider {
	@Override
	public final Object[] getElements(final Object inputElement) {		
		if (inputElement instanceof List<?>) {	
			@SuppressWarnings("unchecked")
			List<CheckReference> checkReferenceList = (List<CheckReference>) inputElement;
			return checkReferenceList.toArray();
		}	
		return null;
	}
}
