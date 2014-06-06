package carisma.regulatory.ruleallocator.OLD;



import java.util.ArrayList;
import java.util.Iterator;




import org.eclipse.jface.viewers.ISelection;

import org.eclipse.jface.viewers.ListViewer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IViewSite;

import org.eclipse.ui.IWorkbenchPart;

import org.eclipse.ui.PartInitException;

import org.eclipse.ui.part.ViewPart;

import carisma.regulatory.ruleallocator.ContentProvider;
import carisma.regulatory.ruleallocator.RuleElementListLabelProvider;
import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.RuleElement;
import carisma.regulatory.ruleallocator.datamodel.Situation;



public class RuleView extends ViewPart implements ISelectionListener {
	
	public static String ID = "de.sepenn.bachelor.app.RuleView";  

	ListViewer viewer;

	Shell shell = new Shell();
	Composite composite = new Composite(shell, SWT.NONE);
	ArrayList<Object> list = new ArrayList<Object>();

	public RuleView() {
		super();
		//TODO: (jk) Fix the following
		//readFile();	
	}

	@Override
	public void createPartControl(Composite parent) {
		viewer = new ListViewer(parent);
        viewer.setContentProvider(new ContentProvider());
        viewer.setLabelProvider(new RuleElementListLabelProvider());
		viewer.setInput(list);
		
		// TODO: (jk) Remove this test entry
		String testEntry = "This is a test entry";
		list.add(testEntry);
		
		getSite().setSelectionProvider(viewer);
	}
	
	@Override
	public void init(IViewSite site) throws PartInitException {
		super.init(site);
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
	}
	 
	public void refresh() throws PartInitException{
		getViewSite().getPage().hideView(RuleView.this);
		getViewSite().getPage().showView(ID);
	}
	
	private void readFile() {
		DataLoader loader = new DataLoader();
		Container container = loader.load2();
		for (Iterator<RuleElement> iterator = container.getContainsRuleElement().iterator(); iterator.hasNext();) {
			  
		      RuleElement rule = iterator.next();
		      list.add(rule);
		}
		for (Iterator<Situation> iterator = container.getContainsSituation().iterator(); iterator.hasNext();) {
			  
		      Situation rule = iterator.next();
		      list.add(rule);
		}
	}

	@Override
	public void selectionChanged(IWorkbenchPart part, ISelection selection) {
     	
	}
}