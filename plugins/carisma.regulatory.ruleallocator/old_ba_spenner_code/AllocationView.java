package carisma.regulatory.ruleallocator.OLD;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;



import org.eclipse.jface.action.Action;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;

import org.eclipse.jface.viewers.IStructuredSelection;

import org.eclipse.jface.viewers.ListViewer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;

import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISelectionListener;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import carisma.regulatory.ruleallocator.AllocationListLabelProvider;
import carisma.regulatory.ruleallocator.ContentProvider;
import carisma.regulatory.ruleallocator.datamodel.Allocation;
import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.RuleElement;

public  class AllocationView  extends ViewPart implements ISelectionListener  {

	public static String ID = "de.sepenn.bachelor.app.ruleallocatorview";  
	
	ListViewer viewer;
	Shell shell = new Shell();
	Composite composite = new Composite(shell, SWT.NONE);
	ArrayList<Object> listx = new ArrayList<Object>();
	static RuleElement rule1;
//	AssociationController assoCon = new AssociationController();
	static BPMNElement bpmn;
	static BPMNElement bpmn2;
	Shell shell1 = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();  
	private Label label;
	private Label label2;
	AllocationControllerOLD allocationController = new AllocationControllerOLD();
	Action AllocateItemsAction; 
	ArrayList<Object> list = new ArrayList<Object>();
	Allocation allo1;
	static Button button;
	 Boolean set1 = false;
	 Boolean set2 = false;
	
	public AllocationView() {
		super();
		readFile();
	}
	
	@Override
	public void createPartControl(Composite parent) {
		
		Composite all = new Composite(parent, SWT.NONE);
		FillLayout layout2 = new FillLayout();
		layout2.type = SWT.VERTICAL;
		all.setLayout(layout2); 
	
		Composite top = new Composite(all, SWT.NONE);
		Composite bottom = new Composite(all, SWT.NONE);
		 
		   top.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING,
				   GridData.VERTICAL_ALIGN_BEGINNING , false, false));
		  
		   GridLayout gridLayout = new GridLayout();
		   gridLayout.numColumns = 2;
		   
		   GridLayout layout = new GridLayout();
		   
		   layout.marginHeight = 5;
		   layout.marginWidth = 5;
		   // the number of pixels of vertical margin that will be placed along
		   // the top and bottom edges of the layout.
		 
		   layout.makeColumnsEqualWidth = true;// make each column have same width
		   layout.numColumns = 2; // number of columns
		   layout.verticalSpacing = 10;
		   
//		   top.setLayout(layout);  
		   top.setLayout(gridLayout);  
		RowLayout row = new RowLayout();
		row.type = SWT.VERTICAL;
		
	
		
		
		bottom.setLayout(row);
		
        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        Label label3 = new Label(top, SWT.WRAP);
        label3.setText("Regel Element:");
		label = new Label(top, SWT.WRAP);
       label.setText("Wähle Regel");
//        label.setSize(600, 100);
//        label.setLayoutData(gridData);
		 Label label4 = new Label(top, SWT.WRAP);
	        label4.setText("BPMN Element:");
        label2 = new Label(top,SWT.WRAP);
        label2.setText("Wähle BPMN Element");
//        label2.setSize(600, 100);
        button = new Button(top, 0);
        button.setText("Allocate Chosen Items");
       
//        button.setEnabled(false);
        button.addSelectionListener(new SelectionAdapter() {
    		@Override
			public void widgetSelected(SelectionEvent e) {
    			try {
					allocationController.allocate(bpmn2, rule1);
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
    			MessageDialog.openInformation(shell1,"Confirmation", "Allocation Succesfull");
    			IWorkbenchPage wbp = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

    			wbp.hideView(wbp.findView("de.sepenn.bachelor.app.AllocationView"));
    			try {
					wbp.showView("de.sepenn.bachelor.app.AllocationView");
				} catch (PartInitException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
    		}
        });
        
        Button button3 = new Button(top, 0);
        button3.setText("Refresh chosen BPMN Element");
        
        button3.addSelectionListener(new SelectionAdapter() {
    		@Override
			public void widgetSelected(SelectionEvent e) {
    			   try {
   					bpmn2 = allocationController.readBPMN();
   				} catch (IOException es) {
   					// TODO Auto-generated catch block
   					es.printStackTrace();
   				}
   	 	      if(!bpmn2.getName().equals(null) && viewer != null && !viewer.getControl().isDisposed())
   			    label2.setText(bpmn2.getName());
   	 	      
   	 	      
   	 	   
   					try {
   						if(allocationController.checkTypes(bpmn2, rule1).equals(true)){
   						   button.setEnabled(true);
   						}
   						else  button.setEnabled(false);
   					} catch (IOException es) {
   						// TODO Auto-generated catch block
   						es.printStackTrace();
   					}
   					
    			
    			
				}
    		});
        ;
    	
        Button button1 = new Button(top, 0);
        button1.setText("Remove Chosen Allocation");
        GridData gridData1 = new GridData();
        gridData1.horizontalAlignment = GridData.FILL;
        gridData1.horizontalSpan = 2;
        button1.setLayoutData(gridData1);
        button1.addSelectionListener(new SelectionAdapter() {
    		@Override
			public void widgetSelected(SelectionEvent e) {
    			
    			try {
					allocationController.removeAllocation(allo1);
				} catch (IOException e2) {
					// TODO Auto-generated catch block
					e2.printStackTrace();
				}
    			list.remove(allo1);
    			MessageDialog.openInformation(shell1,"Confirmation", "Removal Succesfull");
    			IWorkbenchPage wbp = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

    			wbp.hideView(wbp.findView("de.sepenn.bachelor.app.AllocationView"));
    			try {
					wbp.showView("de.sepenn.bachelor.app.AllocationView");
				} catch (PartInitException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
    		}
        });
        

        
        
        
	
		getSite().getPage().addSelectionListener("de.sepenn.bachelor.app.BPMNView",this);		
		getSite().getPage().addSelectionListener("de.sepenn.bachelor.app.ruleview",this);
		getSite().getPage().addSelectionListener("de.sepenn.bachelor.app.AllocationView",this);
		
	
		
		viewer = new ListViewer(top);
        viewer.setContentProvider(new ContentProvider());
        viewer.setLabelProvider(new AllocationListLabelProvider());
		viewer.setInput(list);
		getSite().setSelectionProvider(viewer);
		
	
	
	}
	
	
	
	
	@Override
	public void selectionChanged(IWorkbenchPart part, ISelection selection) {
	
	        if (selection.isEmpty()) return;
	        List<Object> list1 = ((IStructuredSelection)selection).toList();
	        
	        
	        
	        
	        if(list1.get(0) instanceof BPMNElement && !list1.get(0).equals(null)) {
	        
	        bpmn = (BPMNElement) list1.get(0);
	        set1 = true;

	        if (viewer != null && !viewer.getControl().isDisposed()) {
	        	if(!bpmn.getName().equals(null)) {
	        	label2.setText(bpmn.getName()); 
	        	}
	        
	        }

	        listx.add(0,bpmn);

	        }
	        if (list1.get(0) instanceof RuleElement) {
	     
	 	        
	 	        rule1 = (RuleElement) list1.get(0);
	 	       set2 = true;
	 	        listx.add(rule1);
	 	       if (viewer != null && !viewer.getControl().isDisposed())
	 	        label.setText(rule1.getName());
	 	       
	 	      try {
					bpmn2 = allocationController.readBPMN();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
	 	      if(!bpmn2.getName().equals(null) && viewer != null && !viewer.getControl().isDisposed())
			    label2.setText(bpmn2.getName());
	 	      
	 	      
	 	   
					try {
						if(allocationController.checkTypes(bpmn2, rule1).equals(true)){
						   button.setEnabled(true);
						}
						else  button.setEnabled(false);
					} catch (IOException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
					 
			
			     

	        }
	        
	        if (list1.get(0) instanceof Allocation) {
	        	allo1 = (Allocation) list1.get(0);
	        	
	        }
	        
	      
	      

	       
	   	
					  
	     
	    	   
	        
		}

	
	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}
	
	private void readFile() {
		DataLoader loader = new DataLoader();
		Container container = loader.load();
		
		for (Iterator<Allocation> iterator = container.getContainsAllocation().iterator(); iterator.hasNext();) {
			  
		      Allocation rule = iterator.next();
		      list.add(rule);
		}
		
		
		
	
	
}
	
	
	
	 
	
	
	
	

}
