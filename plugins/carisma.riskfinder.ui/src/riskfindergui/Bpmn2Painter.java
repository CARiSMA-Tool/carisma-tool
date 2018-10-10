package riskfindergui;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.bpmn2.BaseElement;
import org.eclipse.bpmn2.Bpmn2Factory;
import org.eclipse.bpmn2.ExtensionAttributeValue;
import org.eclipse.bpmn2.util.XmlExtendedMetadata;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.impl.EAttributeImpl;
import org.eclipse.emf.ecore.impl.EStructuralFeatureImpl;
import org.eclipse.emf.ecore.util.ExtendedMetaData;
import org.eclipse.emf.ecore.xml.type.AnyType;
import org.eclipse.emf.ecore.xml.type.XMLTypeFactory;
import org.eclipse.emf.ecore.xml.type.impl.AnyTypeImpl;

import carisma.processanalysis.textmodel.ProcessEntity;

/**
 * Class for adding color to bpmn2 elements.
 */

public class Bpmn2Painter {



	/**
	 * Sets color to bpmn2 element according to relevance.
	 */
	public static void setColor(final ArrayList<ProcessEntity> pe) {
		 List<RiskActivity> riskActivityList = ModelController.createModel(
				CheckController.getActivityNames(), CheckController.getResultAr());
		for (ProcessEntity curEntity : pe) {
			if (giveRelevance(curEntity.getTexts().get(0).getEntityText(),
					riskActivityList) == 3) {
				setColorTagYaoqiang(
						(org.eclipse.bpmn2.impl.TaskImpl) curEntity.getObject(),
						"#FF0000");
			}
			if (giveRelevance(curEntity.getTexts().get(0).getEntityText(),
					riskActivityList) == 2) {
				setColorTagYaoqiang(
						(org.eclipse.bpmn2.impl.TaskImpl) curEntity.getObject(),
						"#FF9D00");
			}
			if (giveRelevance(curEntity.getTexts().get(0).getEntityText(),
					riskActivityList) == 1) {
				setColorTagYaoqiang(
						(org.eclipse.bpmn2.impl.TaskImpl) curEntity.getObject(),
						"#FFFB05");
			}
		}
		try {
			CheckController.getCurrentModel().save(Collections.EMPTY_MAP);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * gets the relevance of an activity.
	 * 
	 * @param name
	 *            name of the activity
	 * @return
	 */
	private static int giveRelevance(final String name, final List<RiskActivity> l) {
//		TODO Klaus R.: super Variablennamen die der Herr Meier hier nutzt.
		for (RiskActivity r : l) {
			if (name == r.getName()) {
				if (r.getScore() >= 1000) {
					return 3;
				}
				if (r.getScore() >= 500) {
					return 2;
				}
				if (r.getScore() >= 200) {
					return 1;
				}
			}
		}
		return 0;

	}

	/**
	 * this method creates an "extensionElement" with a "yaoqiang:style" tag to
	 * add color and adds it to a bpmn2 element.
	 * 
	 * @param bpmn2Element
	 *            element that should be colored
	 * @param color
	 *            the color for the element
	 */
	public static void setColorTagYaoqiang(final BaseElement bpmn2Element,
			final String color) {

		ExtendedMetaData meta = XmlExtendedMetadata.INSTANCE;

		ExtensionAttributeValue extV = Bpmn2Factory.eINSTANCE
				.createExtensionAttributeValue();
		org.eclipse.emf.ecore.util.FeatureMap ext = extV
				.getValue();

		// create yaoqiang:style tag
		EStructuralFeature style = meta.demandFeature("yaoqiang", "style",
				true, true);
		style.setChangeable(true);

		AnyType extE = (AnyTypeImpl) XMLTypeFactory.eINSTANCE
				.createAnyType();

		// add tag to extensions
		ext.add(style, extE);

		// create fillColor attribute
		EAttributeImpl extA = (EAttributeImpl) meta
				.demandFeature(null, "fillColor", false, false);

		extE.getAnyAttribute().add(
				new EStructuralFeatureImpl.SimpleFeatureMapEntry(
						extA, color));

		bpmn2Element.getExtensionValues().add(extV);
	}
}
