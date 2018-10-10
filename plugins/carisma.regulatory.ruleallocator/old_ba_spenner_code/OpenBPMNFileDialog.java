package carisma.regulatory.ruleallocator.OLD;





import org.eclipse.jface.action.Action;  

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;  
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;  

import carisma.regulatory.ruleallocator.BPMNReader;

public class OpenBPMNFileDialog extends Action implements IWorkbenchAction {
	//TODO: DELETE THIS CLASS, BECAUSE IT ISNT NECESSARY ANY MORE
	
	
	//TODO: Remove iterative ID-Strings
	//private static final String ID = "de.sepenn.bachelor.app.dialogueaction7";  
	private static final String ID = "de.sepenn.bachelor.app.openbpmnfiledialog";
	
	public BPMNReader bpmnReader = new BPMNReader();
	
	// TODO (JK) New approach
	private String filepath;
	
    public OpenBPMNFileDialog(){  
    setId(ID);  
    }  
      
	@Override
	public void run() {  
		Display display = Display.getCurrent();
		Shell shell = new Shell(display);
		FileDialog fd = new FileDialog(shell);
		this.filepath = fd.open();
    }
	
	public String getFilepath() {
		return this.filepath;
	}
	     
	     /*
	     bpmnReader.setFile(pathname);
	     
	     reader.setFile("file:///" +  pathname);

 TODO: (jk) WHY hideView and after that showView for all views???
	     IWorkbenchPage wbp = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

		  wbp.hideView(wbp.findView("de.sepenn.bachelor.app.BPMNView"));
			try {
				wbp.showView("de.sepenn.bachelor.app.BPMNView");
			} catch (PartInitException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
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
*/  
      
      
    @Override
	public void dispose() {}  
      
    
}  