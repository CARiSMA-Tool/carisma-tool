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
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

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

/**
 * All reusable helper methods for bpmn2 models are implemented here.
 * 
 * @author Marcel Michel
 */
public final class BPMN2Helper {
    
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
		ArrayList<T> result = new ArrayList<T>();
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
        	Document doc = DocumentBuilderFactory.newInstance()
        			.newDocumentBuilder().parse(inFilePath);
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
		try {
			TransformerFactory factory = TransformerFactory.newInstance();

			Templates template = factory.newTemplates(new StreamSource(
					BPMN2Helper.class.getClassLoader().getResourceAsStream(xslResourcePath)));
            Transformer xformer = template.newTransformer();

            Source source = new StreamSource(new FileInputStream(inFilename));
            
            Result result;
            if (outFilename != null) {
            	result = new StreamResult(new FileOutputStream(outFilename));
            } else {
            	xsltOutputWriter = new StringWriter();
                result = new StreamResult(xsltOutputWriter);
            }

            xformer.transform(source, result);
            
            return true;
        } catch (FileNotFoundException e) {
        	return false;
        } catch (TransformerConfigurationException e) {
        	return false;
        } catch (TransformerException e) {
        	return false;
        }
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
