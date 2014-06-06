package carisma.regulatory.ruleallocator;
import java.util.List;

import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Original ContentProvider for lists from BA S.Penner.
 * @author spenner
 */
public class ContentProvider implements IContentProvider, IStructuredContentProvider {
	
	@Override
	public Object[] getElements(Object inputElement) {
		if (inputElement instanceof List)
	         return ((List) inputElement).toArray();
		return null;
	}

	@Override
	public void dispose() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// TODO Auto-generated method stub
		
	}
	
	

}
