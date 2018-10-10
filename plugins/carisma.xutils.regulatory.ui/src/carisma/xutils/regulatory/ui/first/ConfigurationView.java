package carisma.xutils.regulatory.ui.first;

import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.part.ViewPart;

import carisma.regulatory.ontology.RuleElement;
import carisma.xutils.regulatory.ui.controller.DataController;
import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.model.Constants;
import carisma.xutils.regulatory.ui.model.RULEELEMENTS;
import carisma.xutils.regulatory.ui.model.RULES;

// TODO: Auto-generated Javadoc
/**
 * GUI class for creating new RuleElements.
 *
 * @author bm
 */
public class ConfigurationView extends ViewPart {

    /** The color. */
    private Color color;
    
    /** The tree. */
    private static Tree configurationTree;
    
    /** The label list. */
    private static Label[] labelList;
    
    /** The data controller. */
    private static DataController dataController = null;
    
    /** The ontology controller. */
    private static MainController controller = null;
    private GridData gd_configurationTree;
    private Text nameOfTheRuleElement;

	private Combo typeOfTheRuleElement;

    /**
     * initializes new Data-controller.
     */
    public ConfigurationView() {
        dataController = new DataController();
        controller = RegulationsView.getOntologyController();
    }
    

    /* (non-Javadoc)
     * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createPartControl(Composite composite) {

        final Composite parent = new Composite(composite, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        parent.setLayout(gridLayout);

        Group group = new Group(parent, SWT.NONE);
        group.setText("Define Rule Elements");
        group.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        gridLayout = new GridLayout(1, false);
        group.setLayout(gridLayout);

        configurationTree = new Tree(group, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL
                | SWT.MULTI);
        configurationTree.setHeaderVisible(true);
        configurationTree.setLinesVisible(true);
        gd_configurationTree = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd_configurationTree.heightHint = 200;
        configurationTree.setLayoutData(gd_configurationTree);
        configurationTree.pack();

        TreeColumn ruleElementColumn = new TreeColumn(configurationTree, SWT.NONE);
        ruleElementColumn.setText("Rule Element");
        ruleElementColumn.setWidth(120);
        TreeColumn colourColumn = new TreeColumn(configurationTree, SWT.NONE);
        colourColumn.setText("Colour");
        colourColumn.setWidth(120);
        for (String s : Constants.RULEELEMENTS) {
            TreeItem item = new TreeItem(configurationTree, SWT.NONE);
            item.setText(s);
        }

        final TreeItem[] treeItem = configurationTree.getItems();
        labelList = new Label[configurationTree.getItems().length];

        // creates labels in the tree for color selection function
        for (int i = 0; i < treeItem.length; i++) {
            TreeEditor editor = new TreeEditor(configurationTree);
            labelList[i] = new Label(configurationTree, SWT.BORDER);
            labelList[i].setText("        ");
            // initializes Color
            labelList[i].setBackground(new Color(parent.getDisplay(),
                    dataController.getColor(RULEELEMENTS
                    .valueOf(treeItem[i].getText()))));

            editor.minimumWidth = 25;
            editor.horizontalAlignment = SWT.LEFT;
            editor.setEditor(labelList[i], treeItem[i], 1);
        }

        // creates MouseListeners for the labels in the tree with ColorDialogs
        // and colored subitems
        for (int i = 0; i < treeItem.length; i++) {
            final int j = i;
            labelList[i].addMouseListener(new MouseListener() {
                public void mouseDown(final MouseEvent e) {
                }

                public void mouseUp(final MouseEvent e) {
                    // creates the ColorDialog
                    Shell shell = new Shell();
                    ColorDialog dlg = new ColorDialog(shell);
                    // sets the selected color in the dialog to predefined color
                    dlg.setRGB(labelList[j].getBackground().getRGB());
                    dlg.setText("Choose a colour");
                    // gets the new selected color from the ColorDialog
                    RGB rgb = dlg.open();
                    if (rgb != null) {
                        // disposes old color
                        if (color != null) {
                            color.dispose();
                        }
                        // creates new color and sets it into the label
                        color = new Color(shell.getDisplay(), rgb);
                        labelList[j].setBackground(color);
                        dataController.setColor(color, RULEELEMENTS
                        		.valueOf(treeItem[j].getText()));

                        // creates colored subitems
                        for (int k = 0; k < treeItem[j].getItems().length; k++) {
                            treeItem[j].getItem(k).setForeground(color);
                        }
                    }
                }
                public void mouseDoubleClick(MouseEvent e) {
                }
            });
        }

        Composite parent2 = new Composite(group, SWT.NONE);
        parent2.setLayout(null);
        GridData gd_parent2 = new GridData(SWT.LEFT, SWT.CENTER, false, false, 1, 1);
        gd_parent2.widthHint = 570;
        parent2.setLayoutData(gd_parent2);
        
        Group grpRuleelement = new Group(parent2, SWT.NONE);
        grpRuleelement.setBounds(5, 5, 146, 64);
        grpRuleelement.setText("Rule Element");
        
        nameOfTheRuleElement = new Text(grpRuleelement, SWT.BORDER);
        nameOfTheRuleElement.setText("Please enter a name");
        nameOfTheRuleElement.setBounds(10, 31, 127, 23);
        
        Group grpRuleelementtype = new Group(parent2, SWT.NONE);
        grpRuleelementtype.setBounds(156, 5, 146, 64);
        grpRuleelementtype.setText("Rule Element Type");
        
        typeOfTheRuleElement = new Combo(grpRuleelementtype, SWT.READ_ONLY);
        typeOfTheRuleElement.setItems(Constants.RULEELEMENTS);
        typeOfTheRuleElement.setBounds(10, 31, 127, 23);
        typeOfTheRuleElement.select(0);

        Button btnNew = new Button(parent2, SWT.PUSH);
        btnNew.setBounds(5, 75, 105, 25);
        btnNew.setText("New Rule Element");

        // creates SelectionListener for creating new RuleElement
        btnNew.addSelectionListener(createListenerForBNew(parent.getShell()));
         
        // delete ruleElement Button
                 
//        final Button btnDelete = new Button(parent2, SWT.PUSH);
//        btnDelete.setBounds(446, 5, 114, 25);
//        btnDelete.setText("Delete Ruleelement");
//        
//         // creates SelectionListener for deleting RuleElements
//         btnDelete.addSelectionListener(createListenerForBDelete(parent.getShell()));
    }
    	// does not delete a single occurence of a ruleelement, but deletes a complete individual
//    private SelectionListener createListenerForBDelete(final Shell parent) {
//		SelectionAdapter adapter = new SelectionAdapter() {
//	         public void widgetSelected(SelectionEvent event) {
//	             try {
//		             // gets selected items
//		             TreeItem[] array = configurationTree.getSelection();
//		             if (array.length < 1) {
//		            	 controller.makeMessageBox(parent, "Please select an Ruleelement in the Tree.");
//		            	 return;
//		             }
//		             for (TreeItem item : array) {
//			             // RuleElementTypes not allowed to be deleted
//			             if (!Arrays.asList(Constants.RULEELEMENTS).contains(item.getText())) {
//				             controller.deleteRuleElement(item.getText());
//				             item.dispose();
//				             } else {
//				            	 controller.makeMessageBox(parent, "You cannot delete a ruleelement class.");
//			             }
//		             }
//		            
//		             // deletes the items in the list and adds the
//		             // remaining ones
//		             dataController.clearRuleElements();
//		            
//		             for (int i = 0; i < configurationTree.getItems().length; i++) {
//			             for (TreeItem item : configurationTree.getItem(i).getItems()) {
//			            	 dataController.addRuleElement(item.getText(), i);
//			             }
//		             }
//		             parent.update();
//		             // synchronize the items in the Regulations-tab
//		             RegulationsView.updateEntriesOfRuleElementComboBox();
//		             configurationTree.setFocus();
//		            
//	             } catch (Exception e) {
//	            	 controller.makeMessageBox(parent, "Please choose a rulelement which attempts to be deleted.");
//	             }
//	         }
//		};
//	    return adapter;
//	}
//
	private SelectionListener createListenerForBNew(final Shell parent) {
		SelectionListener listener = new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                try {
                    for (int i = 0; i < configurationTree.getItems().length; i++) {
                        // checks at which index (of the tree) the item (of the
                        // combobox) appears
                        if (configurationTree.getItem(i)
                                .getText()
                                .equals(typeOfTheRuleElement.getItem(typeOfTheRuleElement.getSelectionIndex()))) {
                            // checks if the element already exists
                            for (TreeItem it : configurationTree.getItem(i).getItems()) {
                                if (it.getText().equals(nameOfTheRuleElement.getText())) {
                                    System.out
                                            .println("Element bereits vorhanden!");
                                    return;
                                }
                            }

                            // checks if the name of the element is blank or not
                            // renamed
                            if (nameOfTheRuleElement.getText().trim().isEmpty()
                                    | nameOfTheRuleElement.getText().equals(Constants.ENTER_NAME)) {
                                controller.makeMessageBox(parent, "Please enter a name.");
                                return;
                            }

                            // creates new item with matching color
                            TreeItem item = new TreeItem(configurationTree.getItem(i),
                                    SWT.NONE);

                            item.setForeground(labelList[i].getBackground());
                            item.setText(nameOfTheRuleElement.getText().replace('_', ' '));
                            controller.createNewRuleElement(
                                    typeOfTheRuleElement.getItem(typeOfTheRuleElement.getSelectionIndex()),
                                    nameOfTheRuleElement.getText().replace('_', ' '));

                            // deletes the items in the list and adds the
                            // remaining ones
                            dataController.clearRuleElements();

                            for (int j = 0; j < configurationTree.getItems().length; j++) {

                                for (TreeItem item1 : configurationTree.getItem(j).getItems()) {
                                    dataController.addRuleElement(
                                            item1.getText(), j);
                                }
                            }
                            // synchronizes the items in the Regulations-tab
                            controller.updateRuleElements();
                            RegulationsView.updateEntriesOfRuleElementComboBox();
                            configurationTree.setFocus();
                        }
                    }

                } catch (Exception e) {
                	controller.makeMessageBox(parent, "Please choose a Type.");
                }
            }
        };
        return listener;
	}

	/**
     * returns tree.
     *
     * @return Tree
     */
    public static Tree getTree() {
        return configurationTree;
    }

    /**
     * returns labelList for the colors.
     *
     * @return Label[]
     */
    public static Label[] getLabelList() {
        return labelList;
    }

    /**
     * adds existing RuleElements of the Ontology to the tree.
     *
     * @return the rule elements
     */
    public static void getRuleElements() {

        if (controller.getRuleElements() != null) {

            for (int k = 0; k < configurationTree.getItems().length; k++) {
                if (configurationTree.getItem(k).getItems() != null) {
                    for (TreeItem item : configurationTree.getItem(k).getItems()) {
                        item.dispose();
                    }
                }
            }

            int i = 0;
            for (String re : controller.getRuleElements()) {
            	if (re.contains("_")) {
	            	i = RULEELEMENTS.valueOf(re.split("_")[0]).getValue();
	
	                // creates new item with matching color
	                TreeItem item = new TreeItem(configurationTree.getItem(i), SWT.NONE);
	                item.setForeground(labelList[i].getBackground());
	                item.setText(re.split("_")[1]);
            	}

            }

            // deletes the items in the list and adds the
            // remaining ones
            dataController.clearRuleElements();

            for (int j = 0; j < configurationTree.getItems().length; j++) {

                for (TreeItem item1 : configurationTree.getItem(j).getItems()) {

                    dataController.addRuleElement(item1.getText(), j);
                }
            }
        }
        RegulationsView.updateEntriesOfRuleElementComboBox();
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        // TODO Auto-generated method stub

    }

}
