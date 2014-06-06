package riskfindergui;

import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.osgi.framework.Bundle;

/**
 * This class provides the content of the column cells in the Riskfinder view.
 * Warning Signs and Activity names or scored words. 
 *
 */
public class EntityLabelProvider extends ColumnLabelProvider {

	@Override
	public final String getText(final Object element) {
		if (element instanceof RiskActivity) {
				return ((RiskActivity) element).getName()
						+ "(" + ((RiskActivity) element).getScore() + ")";
			}
			if (element instanceof RelevantWords) {
					return ((RelevantWords) element).getTreeSet().toString();
			}
			if (element instanceof RiskPattern) {
				return ((RiskPattern) element).getPatternString();
			}

			return null;

		}
	
	/**
	 * Returns the warning sign icon depending on the given element and its risk score.
	 * @param element - should be a RiskActivity
	 * @return - the appropriate warning sign icon
	 */
	@Override
	public Image getImage(final Object element) {
		if (element instanceof RiskActivity) {
			RiskActivity act = (RiskActivity) element;
			if (act.getScore() >= 1000) {
				return getWarningSignIcon("red");
			}
			if (act.getScore() >= 500) {
				return getWarningSignIcon("orange");
			}
			if (act.getScore() >= 200) {
				return getWarningSignIcon("yellow");
			}
		}
		return null;

	}
	/**
	 * Returns the warning sign icon colored using the given color. 
	 * @param color - the color of the icon (i.e. red, orange or yellow)
	 * @return - the warning sign icon
	 */
	public Image getWarningSignIcon(final String color) {
		Bundle bundle = Platform.getBundle("riskfindergui");
		IPath iconPath = new Path("icons/" + color + ".png");
		URL iconUrl = FileLocator.find(bundle, iconPath, null);
		ImageDescriptor desc = ImageDescriptor.createFromURL(iconUrl);
		return new Image(null, desc.createImage(), 0);
	}

}
