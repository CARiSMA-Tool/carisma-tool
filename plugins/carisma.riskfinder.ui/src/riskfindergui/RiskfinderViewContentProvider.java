package riskfindergui;

import java.util.ArrayList;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Implementation of the {@link ITreeContentProvider} for the RiskfinderView.
 * @author Tobias Meier
 *
 */
public class RiskfinderViewContentProvider implements ITreeContentProvider {

	@Override
	public final Object[] getChildren(final Object parentElement) {
		if (parentElement instanceof RiskActivity) { 
			RiskActivity riskact = (RiskActivity) parentElement;
			return riskact.getRelWords().toArray();
		}
		if (parentElement instanceof RelevantWords) {
			RelevantWords words = (RelevantWords) parentElement;
			return words.getPatterns().toArray();
		}
		return new Object[0];
	}

	@Override
	public final Object getParent(final Object element) {
		return null;
	}

	@Override
	public final boolean hasChildren(final Object element) {
		return true;
	}

	@SuppressWarnings("unchecked")
	@Override
	public final Object[] getElements(final Object inputElement) {
		if (inputElement instanceof ArrayList) {
			return ((ArrayList<RiskActivity>) inputElement).toArray();
		}
		return new Object[0];
	}

	@Override
	public void dispose() {
	}

	@Override
	public void inputChanged(final Viewer viewer, final Object oldInput,
			final Object newInput) {
	}
	
}
