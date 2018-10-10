package riskfindergui;

import java.util.List;
import java.util.Observable;
import java.util.Observer;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import carisma.check.riskfinder.Check;

/**
 * View class.
 * 
 */
public class RiskfinderView extends ViewPart implements Observer {

	/**
	 * string for the GUI.
	 */
	public static final String ID = "riskfindergui.RiskView";
	
	/**
	 * View for the GUI.
	 */
	private Viewer viewer;
	
	/**
	 * list to save the {@link RiskActivity}s in it that should be displayed.
	 */
	private List<RiskActivity> riskModel = null;
	
	/**
	 * the instance of this check.
	 */
	private static RiskfinderView instance = null;
	

	@Override
	public final void createPartControl(final Composite parent) {
		instance = this;
		registerAsObserver();
		PlatformUI.getWorkbench().getHelpSystem()
				.setHelp(parent, "riskfindergui.Riskhelp");
		viewer = new TreeViewer(parent);
		((TreeViewer) viewer).setContentProvider(new RiskfinderViewContentProvider());
		((TreeViewer) viewer).getTree().setHeaderVisible(true);
		TreeViewerColumn activityColumn = new TreeViewerColumn(((TreeViewer) viewer), SWT.NONE);
		activityColumn.getColumn().setText("Activities");
		activityColumn.getColumn().setWidth(800);
		activityColumn.setLabelProvider(new EntityLabelProvider());
		if (Check.getInstance() == null) {
			return;
		} 
		updateGUI();
	}
	
	/**
	 * updates the GUI with the new content.
	 */
	private void updateGUI() {
		riskModel = ModelController.createModel(CheckController.getActivityNames(), CheckController.getResultAr());
		viewer.setInput(riskModel);
		PdfMaker.makePdf(riskModel, CheckController.getResultAr());
		if (CheckController.getCurrentModel().getURI().lastSegment().endsWith("bpmn2")) {
			Bpmn2Painter.setColor(CheckController.getDescription().getEntities());
		}
//		if (CheckController.getCurrentModel().getURI().lastSegment().endsWith("uml")) {
//			Uml2Painter.setColor(CheckController.getDescription().getEntities());
//			TODO Klaus R.: Tobias Meier hat das einfaerben von UML-Diagrammen nicht hinbekommen, hauptsaechlich
//			weil es zu der UML-Datei noch kein Diagramm gibt.
//			Ueberlegung: UML-Diagamm erwarten, ansonsten einfaerbung nicht anbieten.
//		}
	}

	@Override
	public void setFocus() {
		// update();
	}
	//TODO Klaus R. braucht man dieses Funktion?


	@Override
	public final void update(final Observable arg0, final Object arg1) {
		updateGUI();
	}
	
	/**
	 * returns the instance of this class.
	 * @return this class as instance
	 */
	public static final RiskfinderView getInstance() {
		return instance;
	}
	
	/**
	 * registers this as observer in the riskfinder check plug-in.
	 */
	public final void registerAsObserver() {
		carisma.check.riskfinder.Activator.getDefault().registerGuiObserver(this);
	}
	
	@Override
	public final void dispose() {
		super.dispose();
		carisma.check.riskfinder.Activator.getDefault().registerGuiObserver(null);
	}
	

}
