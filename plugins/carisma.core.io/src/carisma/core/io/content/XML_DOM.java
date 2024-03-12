package carisma.core.io.content;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;


import java.util.logging.Logger;

public class XML_DOM implements Content {

	private static final Logger logger = Logger.getLogger(XML_DOM.class.getName());
	public static final String ID = "XML_DOM";
	private Document xmlDocument;
	
	
	public XML_DOM(final Document document){
		this.xmlDocument = document;
	}
	
	protected XML_DOM(final String xml) throws ContentException {
		InputSource in = new InputSource();
		in.setCharacterStream(new StringReader(xml));
		try {
			DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
			dbFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			dbFactory.setFeature("http://xml.org/sax/features/external-general-entities", false);
			dbFactory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
			dbFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
			dbFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
			DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
			this.xmlDocument = dBuilder.parse(in);
		} catch (ParserConfigurationException e) {
			throw new ContentException(e);
		} catch (SAXException e) {
			throw new ContentException(e);
		} catch (IOException e) {
			throw new ContentException(e);
		}
		this.xmlDocument.getDocumentElement().normalize();
	}

	@Override
	public final String getFormat() {
		return ID;
	}

	@Override
	public final String asString() {
		try {
			DOMSource domSource = new DOMSource(this.xmlDocument);
			StringWriter writer = new StringWriter();
		    StreamResult result = new StreamResult(writer);
		    TransformerFactory tf = TransformerFactory.newInstance();
		    tf.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
		    tf.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
		    tf.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
		    Transformer transformer = tf.newTransformer();
	    	transformer.transform(domSource, result);
		    return writer.toString();
		} catch (TransformerException e) {
			logger.warning("Error message: " + e.getMessage());
		}
	    return "";
	}

	public final Document getDocument() {
		return this.xmlDocument;
	}
}