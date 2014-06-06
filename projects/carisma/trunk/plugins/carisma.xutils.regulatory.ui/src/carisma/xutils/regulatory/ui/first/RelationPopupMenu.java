package carisma.xutils.regulatory.ui.first;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.model.RuleElementModel;
import carisma.xutils.regulatory.ui.controller.DataController;
import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.model.Constants;
import carisma.xutils.regulatory.ui.model.CreationException;
import carisma.xutils.regulatory.ui.model.RULEELEMENTS;


// TODO: Auto-generated Javadoc
/**
 * Relation-dialog class for the GUI.
 *
 * @author bm
 */
public class RelationPopupMenu {

    /** The ontology controller. */
    private MainController controller;
    
    /** The data controller. */
    private DataController dataController;
    
    /** The source rule element. */
    private Combo firstRE;
    
    /** The target rule element. */
    private Combo secondRE;
    
    /** The text. */
//    private Text relationName;
    
    /** The color. */
    private Color color;
    
    /** The label1. */
    private Label firstREColor;
    
    /** The label2. */
    private Label secondREColor;

	private Text descriptionText;
    private static final String DEFAULT_DESCRIPTION = "Please enter a description";
    /**
     * creates a new Relation-dialog.
     *
     * @param parent the parent
     * @param list the list
     */
    public RelationPopupMenu(Shell parent, List<RuleElementModel> list) {

        controller = RegulationsView.getOntologyController();
        dataController = new DataController();

        init(parent, list);
    }

    /**
     * creates the dialog content.
     *
     * @param shell the shell
     * @param list the list
     */
    public void init(final Shell shell, final List<RuleElementModel> list) {

        GridLayout gridLayout = new GridLayout(2, false);
        shell.setLayout(gridLayout);
        shell.setText("Define Relations");

        Group sourceGroup = new Group(shell, SWT.NONE);
        sourceGroup.setText("First Rule Element");
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        sourceGroup.setLayoutData(gridData);
        gridLayout = new GridLayout(2, false);
        sourceGroup.setLayout(gridLayout);

        firstRE = new Combo(sourceGroup, SWT.READ_ONLY);
        firstRE.setText("Please choose an Element");
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.widthHint = 150;
        firstRE.setLayoutData(gridData);

        firstREColor = new Label(sourceGroup, SWT.BORDER);
        firstREColor.setText("        ");

        // creates SelectionListener for choosing ruleElement and color of
        // correspondent
        // RuleElementType
        firstRE.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent event) {
                // disposes old color
                if (color != null) {
                    color.dispose();
                }

                color = new Color(firstREColor.getParent().getDisplay(), dataController
                        .getColor(RULEELEMENTS.valueOf(
                        		firstRE.getText().substring(
                				firstRE.getText().indexOf("(") + 1,
                				firstRE.getText().indexOf(")")))));
                // System.out.println(color.getRGB());

                firstREColor.setBackground(color);

                // System.out.println("souceElementCombo getSelectionIndex(): "
//                        + firstRE.getSelectionIndex());
                firstRE.setToolTipText(getTextRepresentationFromIndices(list,
                        firstRE.getText()));

            }
        });
        		// TODO for now there are only pre defined relations
//        Group relationGroup = new Group(shell, SWT.NONE);
//        relationGroup.setText("Relation");
//        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
//        relationGroup.setLayoutData(gridData);
//        gridLayout = new GridLayout(1, false);
//        relationGroup.setLayout(gridLayout);

//        relationName = new Text(relationGroup, SWT.BORDER);
//        relationName.setText("Enter name");
//        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
//        relationName.setLayoutData(gridData);

        Group targetGroup = new Group(shell, SWT.NONE);
        targetGroup.setText("Second Rule Element");
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        targetGroup.setLayoutData(gridData);
        gridLayout = new GridLayout(2, false);
        targetGroup.setLayout(gridLayout);

        secondRE = new Combo(targetGroup, SWT.READ_ONLY);
        secondRE.setText("Please choose an Element");
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.widthHint = 150;
        secondRE.setLayoutData(gridData);

        secondREColor = new Label(targetGroup, SWT.BORDER);
        secondREColor.setText("        ");

        // creates SelectionListener for choosing ruleElement and color of
        // correspondent
        // RuleElementType
        secondRE.addSelectionListener(
        		createSelectionListenerForTargetComboBox(list));

        // sets Items into comboBoxes
        getRuleElementsFromIndices(list);
        
        Group descriptionGroup= new Group(shell, SWT.NONE);
        descriptionGroup.setText("Description");
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        descriptionGroup.setLayoutData(gridData);
        gridLayout = new GridLayout(1, false);
        descriptionGroup.setLayout(gridLayout);
        
        descriptionText = new Text(descriptionGroup, SWT.SINGLE | SWT.BORDER);
        descriptionText.setText(DEFAULT_DESCRIPTION);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        descriptionText.setLayoutData(gridData);
        
        Button bCreate = new Button(shell, SWT.PUSH);
        bCreate.setText("Create");
        gridData = new GridData(SWT.RIGHT, SWT.FILL, false, false);
        gridData.widthHint = 100;
        gridData.horizontalSpan = 3;
        bCreate.setLayoutData(gridData);

        // creates SelectionListener for creating new Relation
        bCreate.addSelectionListener(
        		createListenerForCreateButton(shell));

        shell.open();
        shell.pack();

    }
    
    private SelectionListener createListenerForCreateButton(final Shell shell) {
    	SelectionListener listener = new SelectionListener() {
			public void widgetSelected(final SelectionEvent e) {

                try {
//                    if (!relationName.getText().trim().isEmpty()
//                            && !relationName.getText().equals("Enter Name")) {

                        if (areRuleElementsChoosen()) {
                        	OWLNamedIndividual sourceRE = controller			
                        			.getRuleElement(firstRE.getText().substring(firstRE.getText().indexOf(" ") + 1,
                        					firstRE.getText().lastIndexOf(" ")),
                        					firstRE.getText().substring(firstRE.getText().indexOf("(") + 1,
                        							firstRE.getText().indexOf(")")));
                        	OWLNamedIndividual targetRE = controller
                        			.getRuleElement(secondRE.getText().substring(secondRE.getText().indexOf(" ") + 1,
                        					secondRE.getText().lastIndexOf(" ")),
                        					secondRE.getText().substring(secondRE.getText().indexOf("(") + 1,
                        							secondRE.getText().indexOf(")")));
                        	if (sourceRE != null && targetRE != null) {
                        		String description = descriptionText.getText();
                        		if (description.equals(DEFAULT_DESCRIPTION)) {	// we don't want to store the default text in the ontology
                        			description = "";
                        		}
	                            controller.createNewRelation(
	                            		sourceRE,
	                            		targetRE,
//	                            		relationName.getText().replace('_', ' '),
	                            		description);
	
	                            // shows existing relations in the table
	                            for (TableItem item : RegulationsView
	                                    .getTableForRelations().getItems()) {
	                                item.dispose();
	                            }
	                            TreeItem[] array = RegulationsView.getTree()
	                                    .getSelection();
	                            for (int i = 0; i < array.length; i++) {
		                            if (controller.isDataPresent(array[i])) {	
		                            RegulationsView.fillTheRelationsTable(controller
		                                            .getRuleElementInstances(array[i].getText(),
		                                            		(String) array[i].getData(
		                                            				Constants.RULE_CLAZZ)));
		                            }
	                            }
	                            shell.close();
	                            shell.dispose();
	                            // System.out.println("Relation created!");
                        	} else {
                        		MessageBox box = new MessageBox(shell);
                        		box.setMessage("Intern Failure. Relation could not been created.");
                        		box.open();
                        		System.err.println("Failed to create a new "
                        				+ "Relation. (source: " + firstRE.getText()
                        				+ ", target: " + secondRE.getText() + ")");
                        	}
                        } else {
                        	MessageBox box = new MessageBox(shell);
                        	box.setMessage("Please enter a name for the Relation.");
                        	box.open();
                        }
                    

                } catch (Exception ex) {
                    ex.printStackTrace();
                    // System.out
//                            .println("Relation konnte nicht erstellt werden!");

                } catch (CreationException c) {
					MessageBox box = new MessageBox(shell);
					box.setMessage("Failed to create a new Relation.");
					box.open();
					c.printStackTrace();
				}
            }

            public void widgetDefaultSelected(SelectionEvent e) {
            }
        };
        return listener;
    }
    
    private boolean areRuleElementsChoosen() {
    	return (firstRE.getSelectionIndex() != -1
    			|| secondRE.getSelectionIndex() != -1);
    }
    
    private SelectionListener createSelectionListenerForTargetComboBox(
    		final List<RuleElementModel> list) {
    	SelectionListener listener = new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent event) {

                // disposes old color
                if (color != null) {
                    color.dispose();
                }

                color = new Color(secondREColor.getParent().getDisplay(), dataController
                        .getColor(RULEELEMENTS
                        		.valueOf(secondRE.getText().substring(
                        				secondRE.getText().indexOf("(") + 1,
                        				secondRE.getText().indexOf(")")))));
                // System.out.println(color.getRGB());

                secondREColor.setBackground(color);

                secondRE
                .setToolTipText(getTextRepresentationFromIndices(list,
                        secondRE.getText()));
            }
        };
    	return listener;
    }

    /**
     * puts existing ruleElements of the ruleElementInstances to the comboBoxes.
     *
     * @param list the list
     * @return the rule elements from indices
     */
    public void getRuleElementsFromIndices(List<RuleElementModel> list) {
        String type = "";
        String name = "";
        int comboIndexCounter = 0;

        controller.updateRuleElements();
        for (RuleElementModel ruleElement : list) {
            type = ruleElement.getType();
            name = ruleElement.getName();
            // System.out.println("RuleElement-Name: " + name + ", " + type);
            // creates new TableItems for the indices
            if (firstRE.getItems().length == 0) {
                firstRE.add(name + " (" + type + ")", comboIndexCounter);
                secondRE.add(name + " (" + type + ")", comboIndexCounter);
                comboIndexCounter++;
            } else {

                boolean isRuleElementContained = false;

                // checks if same RuleElement exists in TableItems
                for (String it : firstRE.getItems()) {
                    if (it.contains(name) && it.contains("(" + type + ")")) {
                        // System.out.println("Element kommt mehrmals vor: " + it);
                        isRuleElementContained = true;

                    }
                }

                // if same RuleElement was not found, create new TableItem
                if (!isRuleElementContained) {
                    // System.out.println(name + " " + type);
                    firstRE.add(name + " (" + type + ")", comboIndexCounter);
                    secondRE.add(name + " (" + type + ")", comboIndexCounter);
                    comboIndexCounter++;
                }
            }
        }

        firstRE.setText("Rule Elements");
        secondRE.setText("Rule Elements");
    }

    /**
     * gets text of ruleElementInstances of the selected ruleElement.
     *
     * @param list the list
     * @param comboSelection the combo selection
     * @return String
     */
    public String getTextRepresentationFromIndices(
            List<RuleElementModel> list, String comboSelection) {

        String type = "";
        String representation = "";
        String name = "";
        String representations = "";

        // System.out.println("comboSelection: " + comboSelection);
        for (RuleElementModel ruleElement : list) {

            type = ruleElement.getType();
            representation = ruleElement.getRepresentation();
            name = ruleElement.getName();
            if (comboSelection.contains("(" + type + ")")
                    && comboSelection.contains(name)) {
                representations = representations + ", " + representation;
            }

        }
        return representations.substring(2);
    }
}
