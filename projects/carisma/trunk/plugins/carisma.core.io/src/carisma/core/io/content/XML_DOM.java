package carisma.core.io.content;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.json.JSONException;
import org.json.XML;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class XML_DOM implements Content {

	public static final String ID = "XML_DOM";
	private Document document;
	
	public XML_DOM(BASE64 base64) throws ContentException{
		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder dBuilder;
		try {
			dBuilder = dbFactory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			throw new ContentException(e);
		}
		try {
			
			document = dBuilder.parse(new ByteArrayInputStream(base64.getBytesDecoded()));
		} catch (SAXException e) {
			throw new ContentException(e);
		} catch (IOException e) {
			throw new ContentException(e);
		}
		document.getDocumentElement().normalize();
	}
	
	public XML_DOM(Document document){
		this.document = document;
	}
	
	public XML_DOM(InputStream stream) throws ContentException{
		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder dBuilder;
		try {
			dBuilder = dbFactory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			throw new ContentException(e);
		}
		try {
			document = dBuilder.parse(stream);
		} catch (SAXException e) {
			throw new ContentException(e);
		} catch (IOException e) {
			throw new ContentException(e);
		}
		document.getDocumentElement().normalize();
	}
	
	public XML_DOM(File file) throws ContentException {
		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder dBuilder;
		try {
			dBuilder = dbFactory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			throw new ContentException(e);
		}
		try {
			document = dBuilder.parse(file);
		} catch (SAXException e) {
			throw new ContentException(e);
		} catch (IOException e) {
			throw new ContentException(e);
		}
		document.getDocumentElement().normalize();
	}
	
	public XML_DOM(String xml) throws ContentException {
		InputSource in = new InputSource();
		in.setCharacterStream(new StringReader(xml));
		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder dBuilder;
		try {
			dBuilder = dbFactory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			throw new ContentException(e);
		}
		try {
			document = dBuilder.parse(in);
		} catch (SAXException e) {
			throw new ContentException(e);
		} catch (IOException e) {
			throw new ContentException(e);
		}
		document.getDocumentElement().normalize();
	}

	public XML_DOM(JSON content) throws ContentException {
		try {
			String string = XML.toString(content);
			document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(string);
		} catch (JSONException e) {
			e.printStackTrace();
		} catch (SAXException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		}
	}

	@Override
	public String getFormat() {
		return ID;
	}

	@Override
	public String asString() {
		try {
			DOMSource domSource = new DOMSource(document);
			StringWriter writer = new StringWriter();
		    StreamResult result = new StreamResult(writer);
		    TransformerFactory tf = TransformerFactory.newInstance();
		    Transformer transformer = tf.newTransformer();
	    	transformer.transform(domSource, result);
		    return writer.toString();
		} catch (TransformerException e) {
			e.printStackTrace();
		}
	    return "";
	}

	public Document getDocument() {
		return document;
	}
}