package carisma.xutils.regulatory.ui.first;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import carisma.regulatory.ontology.model.RuleElementModel;
import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.model.Constants;
import carisma.xutils.regulatory.ui.model.CreateConstraintModel;
import carisma.xutils.regulatory.ui.model.CreationException;

import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Button;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

/**
 * The Class ConstraintPopupMenu.
 */
public class ConstraintPopupMenu {
   
    /** The ontology controller. */
    private MainController controller;
    
    /** The t constraint. */
    private Table tConstraint;
    
    /** The t rule elements. */
    private Table tRuleElements;
    
    /** The name of the constraint. */
    private Text nameOfTheConstraint;
    
    /** The constraints. */
    private List<CreateConstraintModel> constraints;
    
    /** The rule elements. */
    private List<RuleElementModel> ruleElements;
    
    /** The shell. */
    private Shell shell;
    
    /**
     * The name of the situation.
     */
    private String situationName = "";
    
    private String ruleName = "";
    private String ruleClazz = "";

	/**
	 * Instantiates a new constraint popup menu.
	 *
	 * @param parent the parent
	 * @param constraints the constraints
	 * @param ruleElements the rule elements
	 */
	public ConstraintPopupMenu(final Shell parent,
			final List<CreateConstraintModel> constraints,
			final List<RuleElementModel> ruleElements,
			final String situationName,
			final String ruleName, final String ruleClazz) {
        controller = RegulationsView.getOntologyController();
        this.constraints = constraints;
        this.ruleElements = ruleElements;
        this.shell = parent;
        this.situationName = situationName;
        this.ruleClazz = ruleClazz;
        this.ruleName = ruleName;
        System.out.println("The list of ruleElements contains " + this.ruleElements.size() + " entries.");
        init(parent);
    }
	
	/**
	 * Inits the Constraint Popup Menu.
	 *
	 * @param shell the shell
	 * @wbp.parser.entryPoint
	 */
	private void init(final Shell shell) {
		
		GridLayout gridLayout = new GridLayout(1, false);
        shell.setLayout(gridLayout);
        shell.setText("Define Situation");
        
        final Group mainGroup = new Group(shell, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        mainGroup.setLayoutData(gridData);
        mainGroup.setLayout(null);
        
        Group grpConstraint = new Group(mainGroup, SWT.NONE);
        grpConstraint.setText("Constraint");
        grpConstraint.setBounds(10, 10, 196, 97);
        
        tConstraint = new Table(grpConstraint, SWT.BORDER | SWT.FULL_SELECTION | SWT.SINGLE);
        tConstraint.setBounds(10, 20, 176, 67);
        tConstraint.addSelectionListener(createListenerForConstraintTable());
        
        fillConstraintTable();
        Group grpRuleelements = new Group(mainGroup, SWT.NONE);
        grpRuleelements.setText("Rule Elements");
        grpRuleelements.setBounds(316, 10, 177, 134);
        
        tRuleElements = new Table(grpRuleelements, SWT.BORDER | SWT.CHECK | SWT.FULL_SELECTION | SWT.MULTI);
        tRuleElements.setBounds(10, 21, 157, 103);
        
        Button btnOk = new Button(mainGroup, SWT.NONE);
        btnOk.setBounds(418, 150, 75, 25);
        btnOk.setText("OK");
        btnOk.addSelectionListener(createListenerForBOK());
        
        Button btnCancel = new Button(mainGroup, SWT.NONE);
        btnCancel.setBounds(326, 150, 75, 25);
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

        
        Button btnAdd = new Button(mainGroup, SWT.NONE);
        btnAdd.setBounds(231, 68, 62, 25);
        btnAdd.setText("Add");
        btnAdd.addSelectionListener(createListenerForBAdd());
        
        Button btnRemove = new Button(mainGroup, SWT.NONE);
        btnRemove.setBounds(231, 99, 62, 25);
        btnRemove.setText("Remove");
        btnRemove.addSelectionListener(createListenerForBRemove());
        
        Group group = new Group(mainGroup, SWT.NONE);
        group.setText("Constraint Name");
        group.setBounds(10, 113, 196, 52);
        
        nameOfTheConstraint = new Text(group, SWT.BORDER);
        nameOfTheConstraint.setBounds(10, 21, 176, 21);
        nameOfTheConstraint.setText(Constants.ENTER_NAME);
        
        shell.open();
        shell.pack();
	}

	/**
	 * Creates the listener for constraint table.
	 *
	 * @return the selection listener
	 */
	private SelectionListener createListenerForConstraintTable() {
		SelectionListener listener = new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				nameOfTheConstraint.setText(Constants.ENTER_NAME);
				TableItem constraint = tConstraint.getSelection()[0]; 	// should work while the selection is swt.Single
				for (CreateConstraintModel model : constraints) {
					if (constraint.getText().equals(model.getClazz())) {
						fillTheRuleElementsTable(model);
					}
				}
				
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// nothing to do
				
			}
		};
		return listener;
	}

	/**
	 * Fill constraint table.
	 */
	private void fillConstraintTable() {
		for (CreateConstraintModel model : constraints) {
			TableItem item = new TableItem(tConstraint, SWT.NONE);
			item.setText(model.getClazz());
		}
		
	}

	/**
	 * Fill the rule elements table.
	 *
	 * @param model the model
	 */
	protected void fillTheRuleElementsTable(final CreateConstraintModel model) {
		List<String> neededRE = model.getNeededRuleElements();
		tRuleElements.clearAll();		// clear the ruleelements table
		tRuleElements.removeAll();
		System.out.println("The needed ruleElements are " + Arrays.toString(neededRE.toArray()));
		for (RuleElementModel reModel : ruleElements) {
			if (neededRE.contains(reModel.getType())) {
				TableItem item = new TableItem(tRuleElements, SWT.NONE);
				item.setText(reModel.getName());
				item.setData(Constants.RULEELEMENT_TYPE, reModel.getType());
			}
		}
		
	}

	/**
	 * Creates the listener for b remove.
	 *
	 * @return the selection listener
	 */
	private SelectionListener createListenerForBRemove() {
		SelectionListener listener = new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				TableItem constraint = tConstraint.getSelection()[0];	// should run if the selection in the table is SWT.SINGLE
				for (CreateConstraintModel model : constraints) {
					if (model.getClazz().equals(constraint.getText())) {
						model.deleteVars();
					}
				}
				
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// nothing to do
			}
		};
		return listener;
	}

	/**
	 * Creates the listener for b add.
	 *
	 * @return the selection listener
	 */
	private SelectionListener createListenerForBAdd() {
		SelectionListener listener = new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				TableItem selectedConstraint = tConstraint.getSelection()[0];	// should run if the selection in the table is SWT.SINGLE
				if (nameOfTheConstraint.getText().equals(Constants.ENTER_NAME)) {
					controller.makeMessageBox(shell, "Please enter a name for the Constraint.");
					return;
				}
				for (CreateConstraintModel model : constraints) {
					if (model.getClazz().equals(selectedConstraint.getText())) {
						if (!isRuleElementSelected(model)) {
							controller.makeMessageBox(shell, "Please choose the Rule Elements for this Constraint.");
							return;
						}
						List<OWLNamedIndividual> ruleElements = new ArrayList<OWLNamedIndividual>();
						TableItem[] items = tRuleElements.getItems();
							// add all checked ruleelements in the list for the constraint
						for (int i = 0; i < items.length; i++) {
							if (items[i].getChecked()) {
								ruleElements.add(controller.getIndividual(
										items[i].getText().replace(" ", "_"),
										(String) items[i].getData(Constants.RULEELEMENT_TYPE)));
								System.out.println("AddButton Listener: " +items[i].getText() + ", "
										+ (String) items[i].getData(Constants.RULEELEMENT_TYPE));
							}
						}
						model.setName(nameOfTheConstraint.getText());
						model.setRuleElements(ruleElements);
						
					}
				}
				
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// nothing to do
				
			}
		};
		return listener;
	}

	/**
	 * Checks if is rule element selected.
	 *
	 * @param model the model
	 * @return true, if is rule element selected
	 */
	protected boolean isRuleElementSelected(final CreateConstraintModel model) {
		int counter = 0;
		TableItem[] items = tRuleElements.getItems();
		for (int i = 0; i < items.length; i++) {			// TODO what about a summary of different constraints
			if (items[i].getChecked()) {
				counter++; 		// count all the items that are selected
			}					// there should be only those items in the list
								// which match to the constraint
		}
		return counter == model.getNeededRuleElements().size();
	}

	/**
	 * Creates the listener for button OK.
	 *
	 * @return the selection listener
	 */
	private SelectionListener createListenerForBOK() {
		return new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					controller.createSituation(situationName, ruleName, ruleClazz, constraints);
				} catch (CreationException e1) {
					controller.makeMessageBox(shell, e1.getMessage());
				} finally {
					shell.close();
				}
				
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// nothing to do
				
			}
		};
	}
	
	
}
