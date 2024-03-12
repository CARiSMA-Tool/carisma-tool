package carisma.core.io.implementations;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import carisma.core.io.content.Content;

import java.util.logging.Logger;


public class FileIO {

	private static final Logger logger = Logger.getLogger(FileIO.class.getName());
	
	public static Document read(File file) {
		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder dBuilder;
		try {
			dbFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			dbFactory.setFeature("http://xml.org/sax/features/external-general-entities", false);
			dbFactory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
			dbFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
			dbFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
			dBuilder = dbFactory.newDocumentBuilder();
		} catch (ParserConfigurationException e1) {
			logger.warning("Error message: " + e1.getMessage());
			return null;
		}
		Document doc;
		try {
			doc = dBuilder.parse(file);
		} catch (SAXException e) {
			logger.warning("Error message: " + e.getMessage());
			return null;
		} catch (IOException e) {
			logger.warning("Error message: " + e.getMessage());
			return null;
		}
		doc.getDocumentElement().normalize();
		return doc;
	}
	
	//function for saving a Content at the specific location
	public static void write(Content content, File file) {
		try{
			if(!file.exists()){
				file.createNewFile();
			}
			try(BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(file))){
				bufferedWriter.write(content.asString());
			} catch (IOException e) {
				logger.warning("Error message: " + e.getMessage());
			}
		} catch (IOException e) {
			logger.warning("Error message: " + e.getMessage());
		}
	}
}
