package carisma.xutils.regulatory.importer.superior.ui.log;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Display;

import java.util.ArrayList;
import java.util.Date;

public class LogViewEntrySet {
	private ArrayList<LogViewEntry> entrySet;
	private boolean showDebug = false;
	private TableViewer viewer;
	private Date date;
	private long lastTime;
	private Display display;
	private int logLevel;
	
	public LogViewEntrySet(ArrayList<LogViewEntry> entrySet, boolean showDebug, TableViewer viewer, Display display) {
		this.entrySet = entrySet;
		this.showDebug = showDebug;
		this.viewer = viewer;
		this.display = display;
		this.logLevel = 10;
	}
	
	public void add(LogViewEntry entry) {
		// Eintrag hat zu geringe Priorität
		if(entry.getLevel() > this.logLevel)
			return;
		
		this.entrySet.add(entry);
		//this.date = new Date();
		//if ((date.getTime() - this.lastTime) >= 100) {
			//this.lastTime = date.getTime();
			display.syncExec(
				new Runnable() {
					public void run(){
						viewer.getTable().setRedraw(false);
						
						viewer.refresh(true, false);
						viewer.getControl().setFocus();
						
						viewer.getTable().deselectAll();
						viewer.getTable().setSelection(viewer.getTable().getItemCount()-1);
						
						viewer.getTable().setRedraw(true);
					}
				});
		//}
	}
		
	public ArrayList<LogViewEntry> getEntrySet() {
		return this.entrySet;
	}
	
	public int getLogVerbosityLevel() {
		return logLevel;
	}

	public void setLogVerbosityLevel(int logLevel) {
		this.logLevel = logLevel;
	}

	public boolean showDebug() {
		return this.showDebug;
	}
}
