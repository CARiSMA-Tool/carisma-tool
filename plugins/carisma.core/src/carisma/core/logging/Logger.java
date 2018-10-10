/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.core.logging;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

public final class Logger {
	
	private static ExternalLogPrinter externalLogPrinter = null;
	
	/** Hide the default Constructor.
	 * 
	 */
	private Logger() {
	}
	
	public static ExternalLogPrinter getExternalLogPrinter() {
		return externalLogPrinter;
	}

	public static void setExternalLogPrinter(ExternalLogPrinter externalLogPrinter) {
		Logger.externalLogPrinter = externalLogPrinter;
	}

	/**
	 * Writes a log message.
	 * @param level
	 * @param message
	 */
	public static void log(LogLevel level, String message) {
		log(level, message, 0);
	}
	
	/**
	 * Writes a log message, but the calling method is now determined by the top of the stack trace.  
	 * @param level
	 * @param message
	 * @param additionalStackDepth
	 */
	public static void log(LogLevel level, String message, int additionalStackDepth) {
		StackTraceElement ste = Thread.currentThread().getStackTrace()[3 + additionalStackDepth];
		String out = level.toString() + ste.getMethodName() + "(" + ste.getClassName() + ":" + ste.getLineNumber() + ")\t";
		if (!forwardToExternalPrinter(ste, level, message)) {
			System.out.println(out + reformat(out.length(), message));
		}
	}

	/**
	 * Writes a log message and prints the stack trace of the given exception afterwards.
	 * @param level
	 * @param message
	 */
	public static void log(LogLevel level, String message, Exception exception) {
		log(level, message, exception, 0);
	}
	
	/**
	 * Writes a log message and prints the stack trace of the given exception afterwards, 
	 * but the calling method is now determined by the top of the stack trace.  
	 * @param level
	 * @param message
	 * @param exception
	 * @param additionalStackDepth
	 */
	public static void log(LogLevel level, final String message, Exception exception, int additionalStackDepth) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		exception.printStackTrace(new PrintStream(baos));
		StackTraceElement ste = Thread.currentThread().getStackTrace()[3 + additionalStackDepth];
		String out = level.toString() + ste.getMethodName() + "(" + ste.getClassName() + ":" + ste.getLineNumber() + ")\t";
		String exceptionMessage = message + "\nException Message: " + exception.getMessage() + "\nSee StackTrace for details:\n" + baos.toString();
		if (!forwardToExternalPrinter(ste, level, exceptionMessage)) {
			System.out.println(out + reformat(out.length(), exceptionMessage));
		}
	}

	/**
	 * Formats the message for console output. 
	 * @param depth
	 * @param string
	 * @return
	 */
	private static String reformat(int depth, String string) {
		if (string == null) {
			return "";
		}
		if (!string.contains("\n")) {
			return string;
		}
		StringBuffer sb = new StringBuffer("");
		for (int i = 1; i < depth; i++) {
			sb.append(" ");
		}
		sb.append("\t");
		return string.replace("\n", "\n" + sb.toString());
	}
	
	/**
	 * Forwards the message to the external log printer (if set).
	 * @param ste
	 * @param level
	 * @param message
	 * @return true if the message has been forwarded to the Eclipse log.
	 */
	private static boolean forwardToExternalPrinter(StackTraceElement ste, LogLevel level, String message) {
		if (externalLogPrinter == null) {
			return false;
		}
		return externalLogPrinter.print(ste, level, message);
	}


}
