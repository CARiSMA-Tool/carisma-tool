package carisma.regulatory.ruleallocator.OLD;





import org.eclipse.jface.action.Action;  

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;  
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;  


public class OpenFileDialogAction extends Action implements IWorkbenchAction {
	//TODO: DELETE THIS CLASS, BECAUSE IT ISNT NECESSARY ANY MORE
	
	private static final String ID = "de.sepenn.bachelor.app.openfiledialogaction";  
	public AllocationControllerOLD dynamic = new AllocationControllerOLD();
	public DataLoader dataloader = new DataLoader();

	// TODO (JK) New approach
	private String filepath;
	

    public OpenFileDialogAction(){  
    	setId(ID);
    } 
    
    @Override 
    public void run() {  
    	 Display display = Display.getCurrent();
	     Shell shell = new Shell(display);
	     FileDialog fileDialog = new FileDialog(shell);
	     fileDialog.setFilterExtensions(new String[]{"*.xmi"});
	     filepath = fileDialog.open();
    }
    
    public String getFilepath() {
    	return this.filepath;
    }
    
    @Override
	public void dispose() {
	} 
	     /*
	     dynamic.setRecource(  pathname);
		  dataloader.setRecource("file:///" +   pathname);
		*/
//		  dynamic2.setRecource("file:///" + pathname);
		/*
		  IWorkbenchPage wbp = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

			wbp.hideView(wbp.findView("de.sepenn.bachelor.app.ruleview"));
			try {
				wbp.showView("de.sepenn.bachelor.app.ruleview");
			} catch (PartInitException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		    
			
			
			wbp.hideView(wbp.findView("de.sepenn.bachelor.app.AllocationView"));
			try {
				wbp.showView("de.sepenn.bachelor.app.AllocationView");
			} catch (PartInitException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			wbp.hideView(wbp.findView("de.sepenn.bachelor.app.BPMNView"));
			try {
				wbp.showView("de.sepenn.bachelor.app.BPMNView");
			} catch (PartInitException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
	     
	     
    } */
}  
