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
package carisma.modeltype.bpmn2.yaoqiang;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.bpmn2.BaseElement;
import org.eclipse.bpmn2.Bpmn2Factory;
import org.eclipse.bpmn2.Documentation;
import org.eclipse.bpmn2.ExtensionAttributeValue;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.impl.EAttributeImpl;
import org.eclipse.emf.ecore.impl.EStructuralFeatureImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.ExtendedMetaData;
import org.eclipse.emf.ecore.util.FeatureMap.Entry;
import org.eclipse.emf.ecore.xml.type.AnyType;
import org.eclipse.emf.ecore.xml.type.XMLTypeFactory;

import carisma.modeltype.bpmn2.BPMN2Helper;


/**
 * All reusable helper methods for bpmn2 yaoqiang models are implemented here.
 * 
 * @author Marcel Michel
 */
public final class YaoqiangHelper {

	/**
	 * Relative path to the XSLT Yaoqiang2EMF.
	 */
	private static final String XSL_YAO_QIANG_2_EMF = "res/yaoqiang2emf.xsl";
	
	/**
	 * Relative path to the XSLT Yaoqiang2EMF.
	 */
	//private static final String XSL_EMF_2_YAO_QIANG = "res/emf2yaoqiang.xsl";
	
	/**
	 * Warning Flag for marked elements.
	 */
	private static final String WARNING_FLAG = "CARiSMA:Warning";
	
	/**
	 * Info Flag for marked elements.
	 */
	private static final String INFO_FLAG = "CARiSMA:Info";
	
	/**
	 * Color Flag for marked elements.
	 */
	private static final String COLOR_FLAG = "CARiSMA:Color=";
	
	
	
	/**
	 * Hide constructor.
	 */
	private YaoqiangHelper() {
	}
	
	/**
	 * Checks if the given model is a Yaoqiang Model.
	 * @param inFilePath The file path to the given model
	 * @return Returns true if Yaoqiang model, otherwise false
	 */
	public static boolean isYaoqiangModel(final String inFilePath) {
		String exporter = BPMN2Helper.performXPathQuery("//definitions/@exporter", inFilePath);
		return exporter.equalsIgnoreCase("Yaoqiang BPMN Editor");
	}
	
	/**
	 * Converts an EMF to a Yaoqiang model.
	 * @param inFilePath The path to the EMF model
	 * @return Returns the transformed model as String
	 */
	public static String emf2yaoqiangModel(final String inFilePath) {
		String output = "";
		try (BufferedReader in = new BufferedReader(new InputStreamReader(
					new FileInputStream(inFilePath)))){
	
			String line;
			line = in.readLine();
			while (line != null) {
				if (line.contains("<definitions")) {
					line = "<definitions "
							+ "xmlns=\"http://www.omg.org/spec/BPMN/20100524/MODEL\" ";
				} else if (line.contains("<bpmn2:definitions")) {
					line = "<bpmn2:definitions "
								+ "xmlns:bpmn2=\"http://www.omg.org/spec/BPMN/20100524/MODEL\" ";
				}
				if (line.contains("<bpmn2:definitions") || line.contains("<definitions")) {
					line += (
						"xmlns:bpmndi=\"http://www.omg.org/spec/BPMN/20100524/DI\" "
								+ "xmlns:dc=\"http://www.omg.org/spec/DD/20100524/DC\" "
								+ "xmlns:di=\"http://www.omg.org/spec/DD/20100524/DI\" "
								+ "xmlns:tns=\"http://www.jboss.org/drools\" "
								+ "xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" "
								+ "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "

								//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
								+ "xmlns:yaoqiang=\"http://bpmn.sourceforge.net\" "
								//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					
								+ "exporter=\"Yaoqiang BPMN Editor\" exporterVersion=\""
									+ BPMN2Helper.performXPathQuery("//definitions/@exporterVersion", inFilePath) + "\" "
								+ "expressionLanguage=\"http://www.w3.org/1999/XPath\" "
								+ "id=\"" + BPMN2Helper.performXPathQuery("//definitions/@id", inFilePath) + "\" "
								+ "name=\"" + BPMN2Helper.performXPathQuery("//definitions/@name", inFilePath) + "\" "
								+ "targetNamespace=\"http://www.jboss.org/drools\" "
								+ "typeLanguage=\"http://www.w3.org/2001/XMLSchema\" "
								+ "xsi:schemaLocation=\"http://www.omg.org/spec/BPMN/20100524/MODEL "
								+ "http://bpmn.sourceforge.net/schemas/BPMN20.xsd\">");				
				} else {
					if (line.contains("<dc:Bounds")) {
						if (!line.contains("x=")) {
							String[] tmp = line.split("<dc:Bounds");
							if (tmp.length == 2) {
								line = tmp[0] + "<dc:Bounds " + "x=\"0.0\"" + tmp[1];
							} else if (tmp.length == 1) {
								line = "<dc:Bounds " + "x=\"0.0\"" + tmp[0];
							}
						}
						if (!line.contains("y=")) {
							String[] tmp = line.split("<dc:Bounds");
							if (tmp.length == 2) {
								line = tmp[0] + "<dc:Bounds " + "y=\"0.0\"" + tmp[1];
							} else if (tmp.length == 1) {
								line = "<dc:Bounds " + "y=\"0.0\"" + tmp[0];
							}
						}
						if (!line.contains("height=")) {
							String[] tmp = line.split("<dc:Bounds");
							if (tmp.length == 2) {
								line = tmp[0] + "<dc:Bounds " + "height=\"0.0\"" + tmp[1];
							} else if (tmp.length == 1) {
								line = "<dc:Bounds " + "height=\"0.0\"" + tmp[0];
							}
						}
						if (!line.contains("width=")) {
							String[] tmp = line.split("<dc:Bounds");
							if (tmp.length == 2) {
								line = tmp[0] + "<dc:Bounds " + "width=\"0.0\"" + tmp[1];
							} else if (tmp.length == 1) {
								line = "<dc:Bounds " + "width=\"0.0\"" + tmp[0];
							}
						}
					} else if (line.contains("<documentation") && !line.contains("textformat")) {
						String[] tmp = line.split("<documentation");
						if (tmp.length == 2) {
							line = tmp[0] + "<documentation textFormat=\"text/plain\"" + tmp[1];
						}
					} else {
						line = line.replaceAll("tns:version=\"\\d+\"", "");
						line = line.replaceAll("tns:packageName=\"\\w+\"", "");
						line = line.replace("<tns:onEntry-script/>", "");
						line = line.replace("<tns:onExit-script/>", "");
						line = line.replaceAll("tns:priority=\"\\d+\"", "");
						line = line.replaceAll("<(/)?bpmn2:extensionElements>", "");
						line = line.replaceAll("<(/)?semantic:extensionElements>", "");
						line = line.replace("tns:", "");
					}
				}
				if (line.replace(" ", "").length() != 0) {
					output += line + "\n";
				}
				line = in.readLine();
			}
		} catch (IOException e) {
			output = "";
		}
			
		return output;
		
		//TODO
		/*if (xslTransformation(inFilePath, null, XSL_EMF_2_YAO_QIANG)) {
			return xsltOutputWriter.toString();
		} else {
			return "";
		}*/
	}
	
	/**
	 * Converts an EMF to a Yaoqiang model.
	 * @param inFilePath The path to the EMF model
	 * @param outFilePath The destination path of the Yaoqiang model.
	 * @return if successful true otherwise false
	 */
	public static boolean emf2yaoqiangModel(final String inFilePath, final String outFilePath) {
		String output = emf2yaoqiangModel(inFilePath);
		try (PrintWriter out = new PrintWriter(new FileWriter(outFilePath))){
			
			if (!output.equals("")) {
				out.print(output);
			} else {
				return false;
			}	
		} catch (IOException e) {
			return false;
		}
		return true;
		
//		TODO
//		return xslTransformation(inFilePath, outFilePath, XSL_EMF_2_YAO_QIANG);
	}
	
	/**
	 * Converts a Yaoqiang to an EMF model.
	 * @param inFilePath The path to the Yaqiang model
	 * @param outFilePath The destination path of the EMF model.
	 * @return if successful true otherwise false
	 */
	public static boolean yaoqiang2emfModel(final String inFilePath, final String outFilePath) {
		return BPMN2Helper.xslTransformation(inFilePath, outFilePath, XSL_YAO_QIANG_2_EMF);
	}
	
	/**
	 * Converts a Yaoqiang to an EMF model.
	 * @param inFilePath The path to the Yaqiang model
	 * @return Returns the transformed model as String
	 */
	public static String yaoqiang2emfModel(final String inFilePath) {
		if (BPMN2Helper.xslTransformation(inFilePath, null, XSL_YAO_QIANG_2_EMF)) {
			return BPMN2Helper.getXsltOutputWriter().toString();
		}
		return "";
	}
	
	/**
	 * Adds a CARiSMA Warning tag to an element.
	 * @param bpmn2Element The bpmn2element which should be marked
	 * @return Returns success of insertion
	 */
	public static boolean setWarningFlagToBpmn2Element(final BaseElement bpmn2Element) {
		return setWarningFlagToBpmn2Element(bpmn2Element, null);
	}
	
	/**
	 * Adds a CARiSMA Warning tag to an element.
	 * @param bpmn2Element The bpmn2element which should be marked
	 * @param desc Additional description
	 * @return Returns success of insertion
	 */
	public static boolean setWarningFlagToBpmn2Element(final BaseElement bpmn2Element, final String desc) {
		return setWarningFlagToBpmn2Element(bpmn2Element, desc, null);
	}
	
	/**
	 * Adds a CARiSMA Warning tag to an element.
	 * @param bpmn2Element The bpmn2element which should be marked
	 * @param desc Additional description
	 * @param color The color of the bpmn2Element
	 * @return Returns success of insertion
	 */
	public static boolean setWarningFlagToBpmn2Element(final BaseElement bpmn2Element, final String desc, final Color color) {
		boolean success = true;
		
		Documentation docWarning = Bpmn2Factory.eINSTANCE.createDocumentation();
		docWarning.setText(WARNING_FLAG);
		success &= bpmn2Element.getDocumentation().add(docWarning);
		
		if (desc != null && !desc.equals("")) {
			Documentation docInfo = Bpmn2Factory.eINSTANCE.createDocumentation();
			docInfo.setText(INFO_FLAG + "\n" + desc);
			success &= bpmn2Element.getDocumentation().add(docInfo);
		}
		
		if (color != null) {
			String rgb = Integer.toHexString(color.getRGB());
			rgb = rgb.substring(2, rgb.length());
			Documentation docColor = Bpmn2Factory.eINSTANCE.createDocumentation();
			docColor.setText(COLOR_FLAG + "#" + rgb);
			success &= bpmn2Element.getDocumentation().add(docColor);
			success &= bpmn2Element.getExtensionValues().add(
					createYaoqiangColorElement("#" + Integer.toHexString(color.getRGB())));
		} else {
			success &= bpmn2Element.getExtensionValues().add(
					createYaoqiangColorElement("#" + Integer.toHexString(Color.red.getRGB())));
		}
		
		return success;
	}
	
	
	/**
	 * Creates an ExtensionAttributeValue which contains
	 * the color information of an bpmn2 element. 
	 * @param color The Color represented as '#HexString'
	 * @return The ExtensionAttributeValue
	 */
	private static ExtensionAttributeValue createYaoqiangColorElement(final String color) {
		ExtendedMetaData meta = ExtendedMetaData.INSTANCE;

		ExtensionAttributeValue extV = Bpmn2Factory.eINSTANCE
				.createExtensionAttributeValue();
		org.eclipse.emf.ecore.util.FeatureMap ext = extV.getValue();

		//Work around for custom nsprefix generation
		String yaoqiangNs = "http://bpmn.sourceforge.net";
		if (!EPackage.Registry.INSTANCE.containsKey(yaoqiangNs)) {
			String yaoqiangNsPrefix = "yaoqiang";
			EPackage ePackage = EcoreFactory.eINSTANCE.createEPackage();
			ePackage.setName(yaoqiangNsPrefix);
			ePackage.setNsURI(yaoqiangNs);
			ePackage.setNsPrefix(yaoqiangNsPrefix);
			EPackage.Registry.INSTANCE.put(ePackage.getNsURI(), ePackage);
		}
		
		EStructuralFeature style = meta.demandFeature(yaoqiangNs, "style",
				true, true);
		style.setChangeable(true);
		
		AnyType extE = XMLTypeFactory.eINSTANCE.createAnyType();

		ext.add(style, extE);

		EAttributeImpl extA = (EAttributeImpl) meta
				.demandFeature(null, "fillColor", false, false);

		extE.getAnyAttribute().add(
				new EStructuralFeatureImpl.SimpleFeatureMapEntry(extA, color));
		
		return extV;
	}

	/**
	 * Clears all CARiSMA Warning tags.
	 * @param root The root element of the model
	 */
	public static void clearAllWarningAndInfoFlags(final EObject root) {
		TreeIterator<EObject> iterator = root.eAllContents();
		
		List<EObject> toBeRemoved = new ArrayList<>();
		
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			if (obj instanceof BaseElement) {
				List<ExtensionAttributeValue> extList = ((BaseElement) obj).getExtensionValues();
				for (ExtensionAttributeValue extensionAttributeValue : extList) {
					for (Entry entry : extensionAttributeValue.getValue()) {
						if (entry.getEStructuralFeature().getName().equalsIgnoreCase("style")) {
							toBeRemoved.add(extensionAttributeValue);
						}
					}
				}
				List<Documentation> docList = ((BaseElement) obj).getDocumentation();
				for (int i = 0; i < docList.size(); i++) {
					String text = docList.get(i).getText();
					if (text.equals(WARNING_FLAG)
							|| text.substring(0, INFO_FLAG.length()).equals(INFO_FLAG)
							|| text.substring(0, COLOR_FLAG.length()).equals(COLOR_FLAG)) {
						toBeRemoved.add(docList.get(i));
					}
				}
			}
		}
		for (EObject eObj : toBeRemoved) {
			EcoreUtil.remove(eObj);
		}
	}
}
