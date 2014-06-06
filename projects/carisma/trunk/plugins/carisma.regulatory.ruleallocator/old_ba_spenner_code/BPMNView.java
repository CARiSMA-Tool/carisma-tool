package carisma.regulatory.ruleallocator.OLD;

import java.util.ArrayList;

import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import org.eclipse.ui.part.ViewPart;
import javax.xml.parsers.ParserConfigurationException;


import org.xml.sax.SAXException;

import carisma.regulatory.ruleallocator.BPMNElementListLabelProvider;
import carisma.regulatory.ruleallocator.BPMNReader;
import carisma.regulatory.ruleallocator.ContentProvider;

import java.io.IOException;


public class BPMNView extends ViewPart {
	
	public static String ID = "de.sepenn.bachelor.app.BPMNView";  
	ListViewer viewer;
	Shell shell = new Shell();
	Composite composite = new Composite(shell, SWT.NONE);
	ArrayList<Object> list = new ArrayList<Object>();

	RuleView rule = new RuleView();
	//File XmlFile = new File("c://test1.bpmn");
	BPMNReader reader = new BPMNReader();
	
	public void setFile(String path) {
		//XmlFile = new File(path); 
	}
	
	public BPMNView() throws ParserConfigurationException, SAXException, IOException {
		super();
		getList();
		
	}

	@Override
	public void createPartControl(Composite parent) {
		viewer = new ListViewer(parent);
        viewer.setContentProvider(new ContentProvider());
        viewer.setLabelProvider(new BPMNElementListLabelProvider());
		viewer.setInput(list);
		getSite().setSelectionProvider(viewer);
	
		

	  }

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub

	}


private void getList () throws ParserConfigurationException, SAXException, IOException {
	 list = reader.readXMLFile();
}
	



}