package carisma.ui.eclipse.views;

import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

public class ExtensionPointEvaluator {

	public static Boolean isVisionInstalled() {

		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint("carisma.ui.visionresultsmenu");
		IExtension[] extensions;

		if (point == null) {
			return false;
		} else {
			extensions = point.getExtensions();

			for (IExtension e : extensions) {
				if (e.getUniqueIdentifier().equals("carisma.ui.vision"))
					return true;

			}

			return false;

		}

	}

}
