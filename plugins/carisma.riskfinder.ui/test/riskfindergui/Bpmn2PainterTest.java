package riskfindergui;

import static org.junit.Assert.*;

import org.eclipse.bpmn2.Bpmn2Factory;
import org.eclipse.bpmn2.ExtensionAttributeValue;
import org.eclipse.bpmn2.Task;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.emf.ecore.xml.type.AnyType;
import org.junit.Test;

public class Bpmn2PainterTest {
	
	/**
	 * Creates a bpmn2 element, lets the method set the tag, 
	 * then checks if the tag was set.
	 */
	@Test
	public void testSetColorToYaoqiang() {
		Task t = Bpmn2Factory.eINSTANCE.createTask();
		Bpmn2Painter.setColorTagYaoqiang(t, "eineFarbe");
		ExtensionAttributeValue e = t.getExtensionValues().get(0);
		FeatureMap f = e.getValue();
		EStructuralFeature ef = f.getEStructuralFeature(0);
		assertEquals(ef.getName(), "style");
		AnyType any = (AnyType)f.getValue(0);
		FeatureMap f2 = any.getAnyAttribute();
		assertEquals(f2.getValue(0), "eineFarbe");
		
	}

}
