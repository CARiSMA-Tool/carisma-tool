package riskfindergui;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;

/**
 * The label provider for the pattern column.
 *
 */
public class PatternLabelProvider extends ColumnLabelProvider {
	
	/**
	 * Returns the pattern count.
	 */
	@Override
	public String getText(final Object element) {
		if (element instanceof RelevantWords) {
			return new Integer(((RelevantWords) element).getPatterns().size()).toString();
		}
		return null;
	}
	/**
	 * Returns no image. Patterns have no image in the column.
	 */
	@Override
	public Image getImage(final Object element) {
		return null;
	}

}

