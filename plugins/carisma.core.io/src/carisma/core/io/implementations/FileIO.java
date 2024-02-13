package carisma.core.io.implementations;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import carisma.core.io.content.Content;


public class FileIO {

	public static Document read(File file) {
		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder dBuilder;
		try {
			dBuilder = dbFactory.newDocumentBuilder();
		} catch (ParserConfigurationException e1) {
			e1.printStackTrace();
			return null;
		}
		Document doc;
		try {
			doc = dBuilder.parse(file);
		} catch (SAXException e) {
			e.printStackTrace();
			return null;
		} catch (IOException e) {
			e.printStackTrace();
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
				e.printStackTrace();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
