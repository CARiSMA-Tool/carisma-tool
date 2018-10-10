package riskfindergui;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

//import org.eclipse.uml2.diagram.activity.edit.parts.ActivityEditPart;
//import org.eclipse.uml2.diagram.activity.part.UMLNewDiagramFileWizard;
import org.eclipse.uml2.uml.Activity;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

import carisma.processanalysis.textmodel.ProcessEntity;

/**
 * Class for adding color to UML elements.
 *
 */
public class Uml2Painter {

	static List<RiskActivity> riskActivityList = ModelController.createModel(
			CheckController.getActivityNames(), CheckController.getResultAr());
	
	/**
	 * Sets color to UML activity according to relevance.
	 * @param pe
	 */
	public static void setColor(final ArrayList<ProcessEntity> pe) {
		for (ProcessEntity curEntity : pe) {
			if (giveRelevance(curEntity.getTexts().get(0).getEntityText(),
					riskActivityList) == 3) {
				setColorTagUMLDiagram(
						initializeDiagram((org.eclipse.uml2.uml.Activity) curEntity.getObject()), "",
						"#FF0000");
			}
			if (giveRelevance(curEntity.getTexts().get(0).getEntityText(),
					riskActivityList) == 2) {
				setColorTagUMLDiagram(
						initializeDiagram((org.eclipse.uml2.uml.Activity) curEntity.getObject()), "",
						"#FF9D00");
			}
			if (giveRelevance(curEntity.getTexts().get(0).getEntityText(),
					riskActivityList) == 1) {
				setColorTagUMLDiagram(
						initializeDiagram((org.eclipse.uml2.uml.Activity) curEntity.getObject()), "",
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
	 * this method should initialize a diagram from a given Activity.
	 * @param object
	 * @return
	 */
	private static File initializeDiagram(Activity object) {
		return null;
		
//		 Diagram diagram = ViewService.createDiagram(object,
//				 ActivityEditPart.MODEL_ID,
//				 ActivityDiagramEditorPlugin.DIAGRAM_PREFERENCES_HINT);
//		 object.eResource().getContents().add(diagram);
//		 
//		 UMLNewDiagramFileWizard w = new UMLNewDiagramFileWizard(null, diagram,
//		 null);
//			 
//		 try {
//			 object.eResource().save(DiagramEditorUtil.getSaveOptions());
//		 } catch (IOException e) {
//		 }
//		 refresh();
//		 return diagram;

	}

	/**
	 * gets the relevance of an activity.
	 * 
	 * @param name
	 *            name of the activity
	 * @return
	 */
	private static int giveRelevance(String name, List<RiskActivity> l) {
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
	 * This methods creates a color tag an adds it to
	 * an UML diagram
	 * @param diagram The diagram
	 * @param id ID of the activity
	 * @param color
	 * @return
	 */
	private static File setColorTagUMLDiagram(File diagram, String id, String color) {
		try {
			DocumentBuilderFactory dbFactory = DocumentBuilderFactory
					.newInstance();
			DocumentBuilder builder = dbFactory.newDocumentBuilder();
			Document doc = builder.parse(diagram);
			doc.getDocumentElement().normalize();

			NodeList children = doc.getElementsByTagName("children");

			for (int i = 0; i < children.getLength(); i++) {
				if ((children.item(i).getAttributes().item(0).getTextContent())
						.equalsIgnoreCase("notation:Shape")) {
					if (children.item(i).getAttributes().item(0).getTextContent() == id) {
						Attr atr = doc.createAttribute("fillColor");
						atr.setValue(color);
						children.item(i).getAttributes().setNamedItem(atr);
					}
				}

			}

		} catch (Exception e) {
			e.printStackTrace();
		}
		return diagram;
	}

}
