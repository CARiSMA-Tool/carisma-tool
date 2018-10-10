package carisma.xutils.regulatory.ui.first;

import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.model.Constants;

import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Button;

/**
 * The Class NewConstraintPopupMenu.
 */
public class NewConstraintPopupMenu {
   
    /** The controller. */
    private MainController controller;
    
    /** The name of the constraint. */
    private Text nameOfTheConstraint;
    
    /** The shell. */
    private Shell shell;
    
    /** The ruleelements list. */
    private List lruleElements;
    
    /** The selected ruleelements list. */
    private List lselectedRuleElements;

	/**
	 * Instantiates a new new constraint popup menu.
	 *
	 * @param parent the parent
	 */
	public NewConstraintPopupMenu(final Shell parent) {
		shell = parent;
        controller = RegulationsView.getOntologyController();

        init(parent);
    }
	
	/**
	 * Inits the popup menu.
	 *
	 * @param shell the shell
	 * @wbp.parser.entryPoint
	 */
	private void init(final Shell shell) {
		
		GridLayout gridLayout = new GridLayout(1, false);
        shell.setLayout(gridLayout);
        shell.setText("Define Situation");
        
        Group mainGroup = new Group(shell, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainGroup.setLayoutData(gridData);
        mainGroup.setLayout(null);
        
        Group grpConstraintName = new Group(mainGroup, SWT.NONE);
        grpConstraintName.setText("Constraint Name");
        grpConstraintName.setBounds(10, 10, 213, 51);
        
        nameOfTheConstraint = new Text(grpConstraintName, SWT.BORDER);
        nameOfTheConstraint.setBounds(10, 20, 193, 21);
        nameOfTheConstraint.setText(Constants.ENTER_NAME);
        
        Group grpRuleelements = new Group(mainGroup, SWT.NONE);
        grpRuleelements.setText("Rule Elements");
        grpRuleelements.setBounds(10, 67, 213, 120);
        
        lruleElements = new List(grpRuleelements, SWT.BORDER | SWT.MULTI);
        lruleElements.setItems(Constants.RULEELEMENTS);
        lruleElements.setBounds(10, 21, 193, 89);
        
        Group grpSelectedRuleelements = new Group(mainGroup, SWT.NONE);
        grpSelectedRuleelements.setText("Selected Rule Elements");
        grpSelectedRuleelements.setBounds(306, 10, 152, 120);
        
        lselectedRuleElements = new List(grpSelectedRuleelements, SWT.BORDER | SWT.H_SCROLL);
        lselectedRuleElements.setBounds(10, 21, 132, 89);
        
        Button btnAdd = new Button(mainGroup, SWT.NONE);
        btnAdd.setBounds(229, 67, 71, 25);
        btnAdd.setText("Add");
        btnAdd.addMouseListener(createListenerForBAdd());
        
        Button btnClear = new Button(mainGroup, SWT.NONE);
        btnClear.setBounds(229, 98, 71, 25);
        btnClear.setText("Clear");
        btnClear.addMouseListener(createListenerForBClear());
        
        Button btnOk = new Button(mainGroup, SWT.NONE);
        btnOk.setBounds(393, 162, 65, 25);
        btnOk.setText("OK");
        btnOk.addMouseListener(createListenerForBOK());
        
        Button btnCancel = new Button(mainGroup, SWT.NONE);
        btnCancel.setBounds(306, 162, 65, 25);
        btnCancel.setText("Cancel");
        btnCancel.addMouseListener(createListenerForBCancel());
        
        shell.open();
        shell.pack();
	}

	/**
	 * Creates the listener for cancel button.
	 *
	 * @return the mouse listener
	 */
	private MouseListener createListenerForBCancel() {
		MouseListener listener = new MouseListener() {
			
			@Override
			public void mouseUp(MouseEvent e) {
				// nothing to do
			}
			
			@Override
			public void mouseDown(MouseEvent e) {
				shell.close();	// closes this shell
			}
			
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				// nothing to do
			}
		};
		return listener;
	}

	/**
	 * Creates the listener for ok button.
	 *
	 * @return the mouse listener
	 */
	private MouseListener createListenerForBOK() {
		MouseListener listener = new MouseListener() {
			
			/* (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseListener#mouseUp(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseUp(MouseEvent e) {
				// nothing to do				
			}			
			@Override
			public void mouseDown(MouseEvent e) {
				if (nameOfTheConstraint.getText().equals(Constants.ENTER_NAME)
						| nameOfTheConstraint.equals("")) {
					controller.makeMessageBox(shell, "Please enter a name for the Constraint.");
				} else {
					String[] sRuleElements = lselectedRuleElements.getItems();
					if (sRuleElements.length > 0) {
						String name = nameOfTheConstraint.getText().replace(" ", "_");
						controller.createNewConstraint(shell, name, sRuleElements);
						shell.close();
					} else {
						controller.makeMessageBox(shell, "Please select at least one Rule Element.");
					}
				}
			}
			
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				// nothing to do
				
			}
		};
		return listener;
	}

	/**
	 * Creates the listener for clear button.
	 *
	 * @return the mouse listener
	 */
	private MouseListener createListenerForBClear() {
		MouseListener listener = new MouseListener() {
			
			@Override
			public void mouseUp(MouseEvent e) {
				// nothing to do				
			}
			
			@Override
			public void mouseDown(MouseEvent e) {
				lselectedRuleElements.removeAll(); 	// remove all selected Ruleelements				
			}
			
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				// nothing to do				
			}
		};
		return listener;
	}

	/**
	 * Creates the listener for b add.
	 *
	 * @return the mouse listener
	 */
	private MouseListener createListenerForBAdd() {
		MouseListener listener = new MouseListener() {
			
			@Override
			public void mouseUp(MouseEvent e) {
				// nothing to do
				
			}
			
			@Override
			public void mouseDown(MouseEvent e) {
				String[] selection = lruleElements.getSelection();
				String[] old = lselectedRuleElements.getItems();
				java.util.List<String> elements = new ArrayList<String>();
				for (int i = 0; i < selection.length; i++) {
					elements.add(selection[i]);
				}
				for (int i = 0; i < old.length;i++) {
					elements.add(old[i]);
				}
				lselectedRuleElements.setItems((String[]) elements.toArray(new String[0]));
				lruleElements.deselectAll();
			}
			
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				// nothing to do
				
			}
		};
		return listener;
	}
}
