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
package carisma.modeltype.bpmn2;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathFactory;

import org.eclipse.bpmn2.BaseElement;
import org.eclipse.bpmn2.DocumentRoot;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.w3c.dom.Document;

import java.util.logging.Logger;
/**
 * All reusable helper methods for bpmn2 models are implemented here.
 * 
 * @author Marcel Michel
 */
public final class BPMN2Helper {
    
	private static final Logger logger = Logger.getLogger(BPMN2Helper.class.getName());
	
	/**
	 * XSLT output can be stored in the xsltOutputWriter object.  
	 */
	private static Writer xsltOutputWriter = null; 
	
    /** 
     * Hide constructor.
     */
    private BPMN2Helper() {
    }
	
	/**
     * Returns a list of all elements of the given type.
     * 
     * @param <T> The type of the desired element
     * @param root The root element of the model
     * @param type The instance of the desired element
     * @return returns a list of all elements of the given type
     */
	@SuppressWarnings("unchecked")
	public static <T> List<T> getAllElementsOfType(final DocumentRoot root, final Class<T> type) {
		ArrayList<T> result = new ArrayList<>();
		TreeIterator<EObject> iterator = root.eAllContents();
		while (iterator.hasNext()) {
			EObject element = iterator.next();
			if (type.isInstance(element)) {
				result.add((T) element);
			}
		}
		return result;
	}
	
	/**
	 * Performs an XPath Query on a given model.
	 * @param xPathQuery The XPathQuery
	 * @param inFilePath The model file
	 * @return If successful the result represented as String otherwise the empty string 
	 */
	public static String performXPathQuery(final String xPathQuery, final String inFilePath) {
		XPath xpath = XPathFactory.newInstance().newXPath();
		String result = "";
		try {
        	//Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(inFilePath);
        	DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
    		DocumentBuilder dBuilder;
    		Document doc;
        	dbFactory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			dbFactory.setFeature("http://xml.org/sax/features/external-general-entities", false);
			dbFactory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
			dbFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
			dbFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
			dBuilder = dbFactory.newDocumentBuilder();
			doc = dBuilder.parse(inFilePath);
        	result = xpath.evaluate(xPathQuery, doc);
        } catch (Exception e) {
        	return "";
        }
        return result;
	}
	
	/**
	 * Performs a XSL Transformation on an given XML file.
	 * @param inFilename The given XML file path
	 * @param outFilename The destination file path, if null the output will be stored in the writer object
	 * @param xslResourcePath The path to the XSLT file
	 * @return If successful true otherwise false
	 */
	public static boolean xslTransformation(final String inFilename, final String outFilename, final String xslResourcePath) {
		TransformerFactory factory = TransformerFactory.newInstance();

		Templates template;
		try {
			template = factory.newTemplates(new StreamSource(
				BPMN2Helper.class.getClassLoader().getResourceAsStream(xslResourcePath)
			));
		} catch (TransformerConfigurationException e) {
			logger.warning("Error message: " + e.getMessage());
			return false;
		}
		
		Transformer xformer;
		try {
			xformer = template.newTransformer();
		} catch (TransformerConfigurationException e) {
			logger.warning("Error message: " + e.getMessage());
			return false;
		}

        Source source;
		try (FileInputStream fileInputStream = new FileInputStream(inFilename);) {
			
			source = new StreamSource(fileInputStream);
		} catch (FileNotFoundException e) {
			logger.warning("Error message: " + e.getMessage());
			return false;
		} catch (IOException e1) {
			logger.warning("Error message: " + e1.getMessage());
			return false;
		}
            
        Result result;
        if (outFilename != null) {
          	try (FileOutputStream fileOutputStream = new FileOutputStream(outFilename);){
				result = new StreamResult(fileOutputStream);
			} catch (FileNotFoundException e) {
				logger.warning("Error message: " + e.getMessage());
				return false;
			} catch (IOException e1) {
				logger.warning("Error message: " + e1.getMessage());
				return false;
			}
        } else {
        	xsltOutputWriter = new StringWriter();
        	result = new StreamResult(xsltOutputWriter);
        }

        try {
			xformer.transform(source, result);
		} catch (TransformerException e) {
			logger.warning("Error message: " + e.getMessage());
			return false;
		}
            
        return true;
        
    }
	
	/**
	 * The XSLT Output Writer stores the result of a XSL Transformation.
	 * @return The XSLT Output Writer Object
	 */
	public static Writer getXsltOutputWriter() {
		return xsltOutputWriter;
	}

	/**
	 * Finds an element, which inherits of BaseElement, by the Attribute Id.
	 * @param root The root element of the model
	 * @param id The matching id represented as String
	 * @return If successful the object, otherwise null
	 */
	public static BaseElement findBaseElementById(final EObject root, final String id) {
		TreeIterator<EObject> iterator = root.eAllContents();
		
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			if (obj instanceof BaseElement) {
				String objId = ((BaseElement) obj).getId();
				if (objId != null && !objId.equals("") && objId.equals(id)) {
					return (BaseElement) obj;
				}
			}
		}
		return null;
	}
}
