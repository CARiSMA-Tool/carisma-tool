package carisma.core.io.content;

import java.io.IOException;
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

import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class XML_DOM implements Content {

	public static final String ID = "XML_DOM";
	private Document xmlDocument;
	
	
	public XML_DOM(final Document document){
		this.xmlDocument = document;
	}
	
	protected XML_DOM(final String xml) throws ContentException {
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
			xmlDocument = dBuilder.parse(in);
		} catch (SAXException e) {
			throw new ContentException(e);
		} catch (IOException e) {
			throw new ContentException(e);
		}
		xmlDocument.getDocumentElement().normalize();
	}

	@Override
	public final String getFormat() {
		return ID;
	}

	@Override
	public final String asString() {
		try {
			DOMSource domSource = new DOMSource(xmlDocument);
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

	public final Document getDocument() {
		return xmlDocument;
	}
}