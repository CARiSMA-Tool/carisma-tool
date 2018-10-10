package carisma.xutils.regulatory.ui.first;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;

import carisma.xutils.regulatory.ui.controller.DataController;
import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.model.Constants;

/**
 * RuleElement-dialog class for the GUI.
 *
 * @author bm
 */
public class RuleElementPopupMenu {

    /** The ruleelement-types. */
    private static Combo ruleElementTypes;
    
    /** The ruleelement name. */
    private static Text ruleElementName;
    
    /** The data controller. */
    private DataController dataController;
    
    /** The main controller. */
    private MainController controller;

    /**
     * creates a new RuleElement-dialog.
     *
     * @param parent the parent
     * @param dataController the data controller
     */
    public RuleElementPopupMenu(Shell parent, DataController dataController) {
        this.dataController = dataController;
        controller = RegulationsView.getOntologyController();
        init(parent);
    }

    /**
     * creates the dialog content.
     *
     * @param shell the shell
     */
    public void init(final Shell shell) {

        GridLayout gridLayout = new GridLayout(1, false);
        shell.setLayout(gridLayout);

        Group group = new Group(shell, SWT.NONE);
        group.setText("Define Rule Element");
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        group.setLayoutData(gridData);

        // array of RuleElementTypes
        final String[] ITEMS = Constants.RULEELEMENTS;
        group.setLayout(null);

        ruleElementName = new Text(group, SWT.BORDER);
        ruleElementName.setBounds(8, 20, 187, 21);
        ruleElementName.setText("Enter name");

        ruleElementTypes = new Combo(group, SWT.READ_ONLY);
        ruleElementTypes.setBounds(8, 46, 187, 23);
        ruleElementTypes.setItems(ITEMS);
        ruleElementTypes.select(0);

        Button btnCreate = new Button(group, SWT.PUSH);
        btnCreate.setBounds(120, 75, 75, 24);
        btnCreate.setText("Create");
        
        Button btnCancel = new Button(group, SWT.NONE);
        btnCancel.setBounds(8, 75, 75, 25);
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

        // creates SelectionListener for creating new RuleElement
        btnCreate.addSelectionListener(createListenerForBCreate(shell));

        shell.open();
        shell.pack();

    }

	private SelectionListener createListenerForBCreate(final Shell shell) {
		SelectionListener listener = new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                try {

                    for (int i = 0; i < ConfigurationView.getTree().getItems().length; i++) {

                        // checks at which index (of the tree) the item (of the
                        // combobox) appears
                        if (ConfigurationView.getTree().getItem(i).getText()
                                .equals(ruleElementTypes.getItem(ruleElementTypes.getSelectionIndex()))) {

                            // checks if the element already exists		
                            for (TreeItem it : ConfigurationView.getTree()
                                    .getItem(i).getItems()) {
                                if (it.getText().equals(ruleElementName.getText())) {
                                    System.out.println("Element bereits vorhanden!");
                                    return;
                                }
                            }
                            // checks if the name of the element is blank or not
                            // renamed
                            if (ruleElementName.getText().trim().isEmpty()
                                    | ruleElementName.getText().equals("Enter name")) {
                            	MessageBox box = new MessageBox(shell);
                            	box.setMessage("Please enter a name.");
                            	box.open();
                            	return;
                            }
                            // creates new item with matching color
                            TreeItem item = new TreeItem(ConfigurationView
                                    .getTree().getItem(i), SWT.NONE);

                            item.setForeground(ConfigurationView.getLabelList()[i]
                                    .getBackground());
                            item.setText(ruleElementName.getText().replace('_', ' '));
                            controller.createNewRuleElement(
                                    ruleElementTypes.getItem(ruleElementTypes.getSelectionIndex()),
                                    ruleElementName.getText().replace('_', ' '));

                            // deletes the items in the list and adds the
                            // remaining ones
                            dataController.clearRuleElements();

                            for (int j = 0; j < ConfigurationView.getTree()
                                    .getItems().length; j++) {

                                for (TreeItem item1 : ConfigurationView.getTree()
                                        .getItem(j).getItems()) {

                                    dataController.addRuleElement(
                                            item1.getText(), j);
                                }
                            }
                            // synchronizes the items in the Regulations-tab
                            controller.updateRuleElements();
                            RegulationsView.updateEntriesOfRuleElementComboBox();
                        }
                    }

                    shell.close();

                } catch (Exception e) {
                	MessageBox box = new MessageBox(shell);
                	box.setMessage("Please choose a Type");
                	box.open();
                }
            }
        };
		return listener;
	}
}
