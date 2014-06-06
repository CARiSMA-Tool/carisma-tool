package riskfindergui;

import java.util.ArrayList;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import carisma.check.riskfinder.AnalyserResult;


//	TODO Klaus R.: Diese Klasse wird anscheinend nie genutzt, loeschen!!
public class RiskViewContentProvider implements ITreeContentProvider {

	@Override
	public Object[] getChildren(final Object parentElement) {
		if (parentElement instanceof AnalyserResult)
			return ((AnalyserResult) parentElement).toArray();
		else
			return null;
	}

	@Override
	public Object getParent(final Object element) {
		return null;
	}

	@Override
	public boolean hasChildren(final Object element) {
		if (element instanceof AnalyserResult)
			return ((AnalyserResult) element).size() > 0;
		else
			return false;
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object[] getElements(final Object inputElement) {
		return ((ArrayList<RiskActivity>) inputElement).toArray();
	}

	@Override
	public void dispose() {
	}

	@Override
	public void inputChanged(final Viewer viewer, final Object oldInput,
			final Object newInput) {
	}

}
