package carisma.xutils.regulatory.ui.first;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import carisma.regulatory.ontology.model.RuleElementModel;
import carisma.xutils.regulatory.ui.controller.DataController;
import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.model.Constants;
import carisma.xutils.regulatory.ui.model.ConstraintModel;
import carisma.xutils.regulatory.ui.model.CreateConstraintModel;
import org.eclipse.swt.widgets.Label;


// TODO: Auto-generated Javadoc
/**
 * Situation-dialog class for the GUI.
 *
 * @author bm
 */
public class SituationPopupMenu {
    
    /** The name of the situation. */
    private Text nameOfTheSituation;
    
    /** The ontology controller. */
    private MainController controller;
    private Table tConstraints;
    private List<RuleElementModel> ruleElements;
    private String ruleName = "";
    private String ruleClazz = "";

	/**
     * creates a new Situation-dialog.
     *
     * @param parent the parent
     * @param list the list
     */
    public SituationPopupMenu(final Shell parent, final List<RuleElementModel> ruleElements,
    		final String ruleName, final String ruleClazz) {
    	this.ruleElements = ruleElements;
    	this.ruleClazz = ruleClazz;
    	this.ruleName = ruleName;
        controller = RegulationsView.getOntologyController();
        new DataController();

        init(parent);
    }

    /**
     * creates the dialog content.
     *
     * @param shell the shell
     * @param list the list
     */
    public void init(final Shell shell) {

        GridLayout gridLayout = new GridLayout(1, false);
        shell.setLayout(gridLayout);
        shell.setText("Define Situation");

        final Group mainGroup = new Group(shell, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainGroup.setLayoutData(gridData);
        mainGroup.setLayout(null);
        
        Group grpNameOfThe = new Group(mainGroup, SWT.NONE);
        grpNameOfThe.setBounds(10, 24, 382, 50);
        grpNameOfThe.setText("Name of the Situation");
        
        nameOfTheSituation = new Text(grpNameOfThe, SWT.BORDER);
        nameOfTheSituation.setBounds(10, 19, 362, 21);
        nameOfTheSituation.setText(Constants.ENTER_NAME);
        nameOfTheSituation.setFocus();
                        
        Button btnCreate = new Button(mainGroup, SWT.PUSH);
        btnCreate.setBounds(301, 179, 75, 25);
        btnCreate.setText("OK");
        btnCreate.addSelectionListener(createListenerForBCreate(shell));
        
        Button btnCancel = new Button(mainGroup, SWT.NONE);
        btnCancel.setBounds(220, 179, 75, 25);
        btnCancel.setText("Cancel");
        btnCancel.addMouseListener(new MouseListener() {			
			@Override
			public void mouseUp(MouseEvent e) {
				// nothing to do				
			}			
			@Override
			public void mouseDown(MouseEvent e) {
				shell.close();				
			}			
			@Override
			public void mouseDoubleClick(MouseEvent e) {				
			}
		});

        
        Group grpConstraint = new Group(mainGroup, SWT.NONE);
        grpConstraint.setText("Constraint");
        grpConstraint.setBounds(10, 80, 382, 93);
        
        tConstraints = new Table(grpConstraint, SWT.BORDER | SWT.CHECK | SWT.FULL_SELECTION | SWT.MULTI);
        tConstraints.setBounds(10, 21, 362, 61);
        
        fillConstraintTable(tConstraints);
        createFakeTooltip(tConstraints, shell);
        // creates SelectionListener for creating new Situation
        btnCreate.addSelectionListener(createListenerForBCreate(shell));

        shell.open();
        shell.pack();

    }
    
    private void createFakeTooltip(final Table table, final Shell shell) {
    	/*
    	 * The following code was copied from 
    	 * @link{http://www.java2s.com/Code/Java/SWT-JFace-Eclipse/CreatefaketooltipsforitemsinaSWTtable.htm}
    	 */
    	 // Disable native tooltip
        table.setToolTipText("");

        // Implement a "fake" tooltip
        final Listener labelListener = new Listener() {
          public void handleEvent(Event event) {
            Label label = (Label) event.widget;
            Shell shell = label.getShell();
            switch (event.type) {
            case SWT.MouseDown:
              Event e = new Event();
              e.item = (TableItem) label.getData("_TABLEITEM");
              // Assuming table is single select, set the selection as if
              // the mouse down event went through to the table
              table.setSelection(new TableItem[] { (TableItem) e.item });
              table.notifyListeners(SWT.Selection, e);
            // fall through
            case SWT.MouseExit:
              shell.dispose();
              break;
            }
          }
        };

        Listener tableListener = new Listener() {
          Shell tip = null;

          Label label = null;

          @SuppressWarnings("unchecked")
		public void handleEvent(Event event) {
            switch (event.type) {
            case SWT.Dispose:
            case SWT.KeyDown:
            case SWT.MouseMove: {
              if (tip == null)
                break;
              tip.dispose();
              tip = null;
              label = null;
              break;
            }
            case SWT.MouseHover: {
              TableItem item = table.getItem(new Point(event.x, event.y));
              if (item != null) {
                if (tip != null && !tip.isDisposed())
                  tip.dispose();
                tip = new Shell(shell, SWT.ON_TOP | SWT.TOOL);
                tip.setLayout(new FillLayout());
                label = new Label(tip, SWT.NONE);
                label.setForeground(shell.getDisplay()
                    .getSystemColor(SWT.COLOR_INFO_FOREGROUND));
                label.setBackground(shell.getDisplay()
                    .getSystemColor(SWT.COLOR_INFO_BACKGROUND));
                label.setData("_TABLEITEM", item);
                label.setText("Rule Elements " + Arrays.toString((
                		(List<String>) item.getData(Constants.TOOLTIP)).toArray()));
                label.addListener(SWT.MouseExit, labelListener);
                label.addListener(SWT.MouseDown, labelListener);
                Point size = tip.computeSize(SWT.DEFAULT, SWT.DEFAULT);
                Rectangle rect = item.getBounds(0);
                Point pt = table.toDisplay(rect.x, rect.y);
                tip.setBounds(pt.x, pt.y, size.x, size.y);
                tip.setVisible(true);
              }
            }
            }
          }
        };
        table.addListener(SWT.Dispose, tableListener);
        table.addListener(SWT.KeyDown, tableListener);
        table.addListener(SWT.MouseMove, tableListener);
        table.addListener(SWT.MouseHover, tableListener);
		
	}

	private void fillConstraintTable(final Table parent) {
		List<ConstraintModel> constraints = controller.getConstraints();
		for (ConstraintModel model : constraints) {
			TableItem conItem = new TableItem(parent, SWT.NONE);
			conItem.setText(model.getName());
			conItem.setData(Constants.LIST_INDIVIDUAL, model.getInstances());
			conItem.setData(Constants.TOOLTIP, model.getRuleelements());
		}
	}

	private SelectionListener createListenerForBCreate(final Shell parent) {
    	SelectionListener listener = new SelectionListener() {
        	@SuppressWarnings("unchecked")
			public void widgetSelected(SelectionEvent e) {
        		try {
	        		if (nameOfTheSituation.getText().equals(Constants.ENTER_NAME)) {
	        			controller.makeMessageBox(parent,
	        					"Please enter a name for the Situation.");
	        		} else {
	        			TableItem[] constraint = tConstraints.getItems();
	        			List<CreateConstraintModel> list = new ArrayList<CreateConstraintModel>();
	        			for (int i = 0; i < constraint.length; i++) {
	        				if (constraint[i].getChecked()) {
	        					list.add(new CreateConstraintModel(constraint[i].getText(),
	        							(List<String>) constraint[i].getData(Constants.TOOLTIP)));
	        				}
	        			}
	        			if (list.size() > 0) {
	        				new ConstraintPopupMenu(new Shell(parent.getDisplay()),
	        						list, ruleElements, nameOfTheSituation.getText(), ruleName, ruleClazz);
	        				parent.close();				// FIXME executes a 'Widget is disposed' exception
	        				return;
	        			}        			
	        		}
        		} catch (SWTException s) {
        			// TODO catches the above thrown exception..... this should be fixed properly
        		}
        		
        	}
        	public void widgetDefaultSelected(SelectionEvent e) {
            }
        };
        return listener;
    }
}