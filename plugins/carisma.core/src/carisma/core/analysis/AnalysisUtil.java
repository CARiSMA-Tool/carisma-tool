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
package carisma.core.analysis;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;
import com.thoughtworks.xstream.security.AnyTypePermission;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;


/**
 * Utility class for dealing with analyses.
 * @author wenzel
 *
 */
public final class AnalysisUtil {
	
	
	/** Hide default Constructor of an Util class.
	 */
	private AnalysisUtil() { 
	}
	
	
	private static XStream createXStream() {
		XStream xStream = new XStream(new DomDriver());
		xStream.alias("Analysis", Analysis.class);
		xStream.alias("Check", CheckReference.class);
		xStream.alias("StringParameter", StringParameter.class);
		xStream.alias("IntegerParameter", IntegerParameter.class);
		xStream.alias("BooleanParameter", BooleanParameter.class);
		xStream.alias("InputFileParameter", InputFileParameter.class);
		xStream.alias("OutputFileParameter", OutputFileParameter.class);
		xStream.addPermission(AnyTypePermission.ANY);
		return xStream;
	}

	/**
	 * Writes an analysis configuration into a file.
	 * @param analysis
	 * @param filename
	 */
	public static void storeAnalysis(Analysis analysis, String filename) {
		XStream xStream = createXStream();
		try (FileOutputStream fos = new FileOutputStream(filename)){
			xStream.toXML(analysis, fos);
			System.out.println(filename);
		} catch (FileNotFoundException e) {
			AnalysisUtil.logFileNotFoundException(filename, e);
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, "Error on closing \"" + filename, e);
		}
		
	}
	
	private static void logFileNotFoundException(String filename, FileNotFoundException e) {
		File output = new File(filename);
		if (output.isDirectory()) {
			Logger.log(LogLevel.ERROR, "The file: \"" + filename + "\" is a directory. Please specify a correct file", e);
		} else if (!output.exists()) {
			Logger.log(LogLevel.ERROR, "The file: \"" + filename + "\" cannot be created for some reasons", e);
		} else {
			Logger.log(LogLevel.ERROR, "The file: \"" + filename + "\" cannot be opend for some reasons", e);
		}
	}
	
	/**
	 * Reads an analysis configuration from a file.
	 * @param filename
	 * @return
	 */
	public static Analysis readAnalysis(String filename) {
		XStream xStream = createXStream();
		try (FileInputStream fis = new FileInputStream(filename);
			InputStreamReader isr = new InputStreamReader(fis, StandardCharsets.ISO_8859_1);){
			Object object = xStream.fromXML(isr);
			Analysis readAnalysis = (Analysis) object;
			if (readAnalysis.getSelectedEditorId() == null) {
				readAnalysis.setSelectedEditorId("");
			}
			return readAnalysis;
		} catch (FileNotFoundException e) {
			AnalysisUtil.logFileNotFoundException(filename, e);
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, "", e);
		}
		return null;
	}
}
