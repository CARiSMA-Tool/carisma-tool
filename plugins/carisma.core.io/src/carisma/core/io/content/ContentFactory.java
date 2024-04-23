package carisma.core.io.content;

import static carisma.core.io.content.ContentFactory.ContentFormats.getFormatValue;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.StringEscapeUtils;
import org.json.JSONException;
import org.json.XML;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import carisma.core.io.content.Content.ContentException;

import java.util.logging.Logger;

/**
 * Factory for creating and converting Content types.
 * @author speldszus
 *
 */
public final class ContentFactory {
	
	private static final Logger logger = Logger.getLogger(ContentFactory.class.getName());
	/**
	 * Shared instance of this factory.
	 */
	public static final ContentFactory INSTANCE = new ContentFactory();
	
	/**
	 * Private constructor use the shared instance instead.
	 */
	private ContentFactory() { }
	
	/**
	 * Creates a Content object according to the type of the String parameter.
	 * 
	 * @param serialized String representation of data
	 * @return A Content object
	 */
	public static Content createContent(final String serialized) {
		Content content = null;
		
		if (isBase64Encoded(serialized)) {
			content = new BASE64(serialized.getBytes());
		} else if (isXmlEncoded(serialized)) {
			try {
				content = new XML_DOM(serialized);
			} catch (ContentException e) {
				content = new PLAIN(serialized);
			}
		} else if (isJsonEncoded(serialized)) {
			try {
				content = new JSON(serialized);
			} catch (JSONException e) {
				logger.warning("Error message: " + e.getMessage());
			}
		} else  {
			content = new PLAIN(serialized);
		}
		
		return content;
	}
	
	/**
	 * Converts the serialized content to the desired Content format.
	 * 
	 * @param serialized A serialized Content object
	 * @param output The desired Content format
	 * @return The created Content object
	 */
	public static Content createContent(final String serialized, final ContentFormats output){
		
		Content content = createContent(serialized);
		
		switch(output) {
		case F_BASE64:
			return convertToBase64(content);
		case F_JSON:
			return convertToJson(content);
		case F_PLAIN:
			return new PLAIN(content.asString());
		case F_XML_DOM:
			return convertToXmlDom(content);
		default:
			throw new RuntimeException("Unsupported output format");
		}
	}

	public static JSON storeInJSONField(Content content, String field){
		String asText = content.asString();
		String escaped = StringEscapeUtils.escapeJson(asText);
		StringBuilder builder = new StringBuilder("{\"");
		builder.append(field);
		builder.append("\":\"");
		builder.append(escaped);
		builder.append("\"}");
		try {
			return new JSON(builder.toString());
		} catch (JSONException e) {
			logger.warning("Error message: " + e.getMessage());
		}
		return null;
	}
	
	/**
	 * Converts the given Content object into a BASE64 content object.
	 * @param content an object of any implementation of the Content interface
	 * @return a BASE64
	 */
	public static BASE64 convertToBase64(final Content content) {
		switch (getFormatValue(content)) {
		case F_BASE64:
			return (BASE64) content;
		case F_JSON:
		case F_PLAIN:
		case F_XML_DOM:
			return new BASE64(content.toString());
		default:
			String message = "Unsupported input format: " + content.getFormat();
			throw new RuntimeException(message);
		}
	}

	/**
	 * Converts the given Content object into a XML_DOM content object.
	 * @param content an object of any implementation of the Content interface
	 * @return a XML_DOM Content object
	 * @throws RuntimeException if content is not convertible to xml
	 */
	public static XML_DOM convertToXmlDom(final Content content) {
		
		switch (getFormatValue(content)) {
		case F_BASE64:
			String contentString = content.asString();
			return convertToXmlDom(createContent(contentString));
		case F_JSON:
			String string = "";
			try {
				string = XML.toString(content);
			} catch (JSONException e1) {
				logger.warning("Error message: " + e1.getMessage());
			}
			
			Document document = null;
			try {
				DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
				factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
				factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
				factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
				factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
				factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
				DocumentBuilder documentBuilder = factory.newDocumentBuilder();
				document = documentBuilder.parse(string);
			} catch (SAXException e) {
				logger.warning("Error message: " + e.getMessage());
			} catch (IOException e) {
				logger.warning("Error message: " + e.getMessage());
			} catch (ParserConfigurationException e) {
				logger.warning("Error message: " + e.getMessage());
			}
			
			if (document != null) {
				return new XML_DOM(document);
			}
			break;
		case F_PLAIN:
			Content realContent = createContent(content.asString());
			ContentFormats format = getFormatValue(realContent);
			if (format != ContentFormats.F_PLAIN) {
				return convertToXmlDom(realContent);
			}
			break;
		case F_XML_DOM:
			return (XML_DOM) content;
		default:
			String contentFormat = content.getFormat();
			String message = "Unsupported input format: " + contentFormat;
			throw new RuntimeException(message);
		}
		throw new RuntimeException("Content cannot be converted to xml");
	}

	/**
	 * Converts the given Content object into a JSON content object.
	 * @param content an object of any implementation of the Content interface
	 * @return a JSON Content object
	 * @throws RuntimeException if content is not convertible to json
	 */
	public static JSON convertToJson(final Content content) {
		String contentString = content.asString();
		
		switch (getFormatValue(content)) {
		case F_BASE64:
			return convertToJson(createContent(contentString));
		case F_JSON:
			return (JSON) content;
		case F_PLAIN:
			Content realContent = createContent(contentString);
			ContentFormats format = getFormatValue(realContent);
			if (format != ContentFormats.F_PLAIN) {
				return convertToJson(realContent);
			}
			String escapedContent = StringEscapeUtils.escapeJson(realContent.asString());
			return convertToJson(createContent("{" + escapedContent +"}"));
		case F_XML_DOM:
			try {
				String string = XML.toJSONObject(contentString).toString();
				return new JSON(string);
			} catch (JSONException e) {
				throw new RuntimeException(e);
			}
		default:
			String contentFormat = content.getFormat();
			String message = "Unsupported input format: " + contentFormat;
			throw new RuntimeException(message);
		}
	}

	/**
	 * Checks whether the given String is Base64 encoded or not.
	 * 
	 * @param serialized String to check
	 * @return {true} if the String is Base64 encoded, otherwise {false}
	 */
	private static boolean isBase64Encoded(final String serialized) {
		return Base64.isBase64(serialized);
		//is base64 encoded?
//		String regex = "^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)$";
//		Pattern pattern = Pattern.compile(regex);
//		Matcher matcher = pattern.matcher(serialized);
//		boolean isBase64 = matcher.find();
//		return isBase64;
	}
	
	/**
	 * Checks if the String is an serialized xml Object.
	 * @param serialized 
	 * @return {true} if the String is xml encoded, otherwise {false}
	 */
	private static boolean isXmlEncoded(final String serialized) {
		boolean isXml = false;
		if (serialized.trim().startsWith("<")) {
			String regex = "<(\\S+?)(.*?)>(.*?)</\\1>";
			Pattern pattern = Pattern.compile(regex);
			Matcher matcher = pattern.matcher(serialized);
			isXml = matcher.find();
		}
		return isXml;
	}
	
	/**
	 * Checks if the String is an serialized json Object.
	 * @param serialized 
	 * @return {true} if the String is json encoded, otherwise {false}
	 */
	private static boolean isJsonEncoded(final String serialized) {
		return serialized.trim().startsWith("{");
	}
	
	/**
	 * An enumeration of all Classes implementing the Content interface.
	 * @author speldszus
	 *
	 */
	public enum ContentFormats {
		
		F_BASE64(BASE64.ID),
		F_JSON(JSON.ID),
		F_PLAIN(PLAIN.ID),
		F_XML_DOM(XML_DOM.ID);
		
		private final String id;

		ContentFormats(final String contentID) {
			this.id = contentID;
		}

		/**
		 * Returns the ID defined in the according Content implementation.
		 * @return An String ID
		 */
		public String getId() {
			return this.id;
		}

		/**
		 * Get an enum value for a Content implementation.
		 * 
		 * @param content Content object
		 * @return According enum value
		 */
		public static ContentFormats getFormatValue(final Content content) {
			String formatID = content.getFormat();
			if (formatID.startsWith("F_")) {
				return ContentFormats.valueOf(formatID);
			}
			return ContentFormats.valueOf("F_" + formatID);
		}
		
	}
}
