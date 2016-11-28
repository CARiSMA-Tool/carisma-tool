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
package carisma.core.util;

import java.io.File;
import java.util.Calendar;
import java.util.GregorianCalendar;

public final class Utils {
	
   	/** 
	 * To hide the default constructor.
	 */
	private Utils() {
	}
	
	/**
	 * Creates a YYYYMMDD-HHMMSS string of the current time.
	 * @return
	 */
	public static String getISOTimestamp() {
		GregorianCalendar c = new GregorianCalendar();
		String ret = "";
		ret = ret + c.get(Calendar.YEAR);
		int tmp = c.get(Calendar.MONTH) + 1;
		if (tmp <= 9) {
			ret = ret + "0";
		}
		ret = ret + tmp;
		tmp = c.get(Calendar.DAY_OF_MONTH);
		if (tmp <= 9) {
			ret = ret + "0";
		}
		ret = ret + tmp;
		ret = ret + "-";
		tmp = c.get(Calendar.HOUR_OF_DAY);
		if (tmp <= 9) {
			ret = ret + "0";
		}
		ret = ret + tmp;
		tmp = c.get(Calendar.MINUTE);
		if (tmp <= 9) {
			ret = ret + "0";
		}
		ret = ret + tmp;
		tmp = c.get(Calendar.SECOND);
		if (tmp <= 9) {
			ret = ret + "0";
		}
		ret = ret + tmp;
		
		return ret;
	}
	
	/**
	 * increments the filename if necessary.
	 * example: input file.html, file.html already exists so output will be file_2.html;
	 * if file_2.html also exist, file_3.html ....
	 * if input is file_2.html and it already exist, next try will be file_3.html and not file_2_2.html
	 * @param fileName the filename
	 * @return the new filename
	 */
	public static String incrementFileNameIfNecessary(final String fileName) {
		File file = new File(fileName);
		  if (!file.exists()) {
			return fileName;
		}
		String pathString = "";
		String fileString = fileName;
		String fileEnding = "";
		String newFile;
		if (fileName.lastIndexOf('/') >= 0) {
			pathString = fileName.substring(0, fileName.lastIndexOf('/')) + "/";
			fileString = fileName.substring(fileName.lastIndexOf('/'));
		}
		if (fileString.lastIndexOf('.') >= 0) {
			fileEnding = fileString.substring(fileString.lastIndexOf('.'));
			fileString = fileString.substring(0, fileString.lastIndexOf('.'));
		}
		int count = 2;
		String integer;
		if ((fileString.lastIndexOf('_') >= 0) && (fileString.lastIndexOf('_') < fileString.length() - 1)) {
			integer = fileString.substring(fileString.lastIndexOf('_') + 1);
			try {
				count = Integer.valueOf(integer).intValue();
				fileString = fileString.substring(0, fileString.lastIndexOf('_')) + "_";
			} catch (Exception e) {
				/*This try-catch block is just for checking if the filename ends with an underline-character
				followed by an integer, so there is no need of an handling of the exception.*/
			}
		}
		if (fileString.lastIndexOf('_') < 0) {
			fileString += "_";
		}
		newFile = pathString + fileString + count + fileEnding;
		file = new File(newFile);
		while (file.exists()) {
			count++;
			newFile = pathString + fileString + count + fileEnding;
			file = new File(newFile);
		}
		return newFile;
	}
}
