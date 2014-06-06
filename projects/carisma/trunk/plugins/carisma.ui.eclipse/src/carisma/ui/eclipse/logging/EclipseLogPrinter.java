package carisma.ui.eclipse.logging;

import org.eclipse.core.runtime.IStatus;

import carisma.core.logging.ExternalLogPrinter;
import carisma.core.logging.LogLevel;


public class EclipseLogPrinter implements ExternalLogPrinter {

	@Override
	public boolean print(StackTraceElement ste, LogLevel level, String message) {
		if (level == LogLevel.DEBUG) {
			return false;
		} 
		int status = -1;
		if (level == LogLevel.INFO) {
			status = IStatus.INFO;
		} else if (level == LogLevel.WARNING) {
			status = IStatus.WARNING;
		} else if (level == LogLevel.ERROR) {
			status = IStatus.ERROR;
		}
		String[] x = ste.getClassName().split("\\.");
		String plugin = x[0] + "." + x[1] + "." + x[2] + "." + x[3];
		
		//Carisma.INSTANCE.getLog().log(new Status(status, plugin, message));
		return true;
	}

}
