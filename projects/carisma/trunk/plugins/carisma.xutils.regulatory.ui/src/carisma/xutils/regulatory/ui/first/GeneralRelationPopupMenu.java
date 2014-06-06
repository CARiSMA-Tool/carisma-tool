package carisma.xutils.regulatory.ui.first;

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
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.model.RuleElementRelationModel;
import carisma.regulatory.ontology.owl.OWL2AbstractElement;
import carisma.xutils.regulatory.ui.controller.DataController;
import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.model.Constants;
import carisma.xutils.regulatory.ui.model.CreationException;
import carisma.xutils.regulatory.ui.model.Data;
import carisma.xutils.regulatory.ui.model.RULEELEMENTS;


// TODO: Auto-generated Javadoc
/**
 * GeneralRelation-dialog class for the GUI.
 *
 * @author bm
 */
public class GeneralRelationPopupMenu {

    /** The default description. */
    private final String DEFAULT_DESCRIPTION = "Please enter a description";

	/** The ontology controller. */
    private MainController ontologyController;
    
    /** The data controller. */
    private DataController dataController;
    
    /** The data. */
    private Data data = Data.instance();
    
    /** The first ruleelement-types combo box. */
    private Combo firstRuleElementTypes;
    
    /** The first ruleelements combo box. */
    private Combo firstRuleElements;
    
    /** The second ruleelement-types combo box. */
    private Combo secondRuleElementType;
    
    /** The second ruleelements combo box. */
    private Combo secondRuleElements;
    
//    /** The text. */
//    private Text text;
//    
    /** The color. */
    private Color color;
    
    /** The parent. */
    private Shell parent = null;

	/** The description text. */
	private Text descriptionText;

    /**
     * creates a new Relation-dialog.
     *
     * @param parent the parent
     */
    public GeneralRelationPopupMenu(Shell parent) {
    	this.parent = parent;
        ontologyController = RegulationsView.getOntologyController();
        dataController = new DataController();

        init(parent);
    }

    /**
     * creates the dialog content.
     *
     * @param shell the shell
     */
    public void init(final Shell shell) {

        GridLayout gridLayout = new GridLayout(2, false);
        shell.setLayout(gridLayout);
        shell.setText("Define gerneral Relation");

        Group firstRuleElementGroup = new Group(shell, SWT.NONE);
        firstRuleElementGroup.setText("first Rule Element");
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        firstRuleElementGroup.setLayoutData(gridData);
        gridLayout = new GridLayout(2, false);
        firstRuleElementGroup.setLayout(gridLayout);

        firstRuleElementTypes = new Combo(firstRuleElementGroup, SWT.READ_ONLY);
        firstRuleElementTypes.setItems(Constants.RULEELEMENTS);
        firstRuleElementTypes.setText("Rule Element Types");
        gridData = new GridData(SWT.FILL, SWT.FILL, true, false);
        gridData.widthHint = 120;
        firstRuleElementTypes.setLayoutData(gridData);

        // creates SelectionListener for choosing ruleElements and color of
        // correspondent RuleElementType
        firstRuleElementTypes.addSelectionListener(createListenerForFirstRE());

        firstRuleElements = new Combo(firstRuleElementGroup, SWT.READ_ONLY);
        firstRuleElements.setText("Rule Elements");
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.widthHint = 120;
        firstRuleElements.setLayoutData(gridData);
        		// for now only pre defined relations are permitted
//
//        Group group2 = new Group(shell, SWT.NONE);
//        group2.setText("Relation");
//        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
//        group2.setLayoutData(gridData);
//        gridLayout = new GridLayout(1, false);
//        group2.setLayout(gridLayout);
//
//        text = new Text(group2, SWT.BORDER);
//        text.setText("Enter name");
//        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
//        text.setLayoutData(gridData);

        Group secondRuleElementGroup = new Group(shell, SWT.NONE);
        secondRuleElementGroup.setText("second Rule Element");
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        secondRuleElementGroup.setLayoutData(gridData);
        gridLayout = new GridLayout(2, false);
        secondRuleElementGroup.setLayout(gridLayout);

        secondRuleElementType = new Combo(secondRuleElementGroup, SWT.READ_ONLY);
        secondRuleElementType.setItems(Constants.RULEELEMENTS);
        secondRuleElementType.setText("Rule Element Types");
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.widthHint = 120;
        secondRuleElementType.setLayoutData(gridData);

        // creates SelectionListener for choosing ruleElements and color of
        // correspondent RuleElementType
        secondRuleElementType.addSelectionListener(createListenerForSecondRE());

        secondRuleElements = new Combo(secondRuleElementGroup, SWT.READ_ONLY);
        secondRuleElements.setText("Rule Elements");
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.widthHint = 120;
        secondRuleElements.setLayoutData(gridData);


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
        bCreate.addSelectionListener(createListenerForBCreate(shell));
        
//        Button btnCancel = new Button(shell, SWT.PUSH);
//        btnCancel.setText("Cancel");
//        gridData = new GridData(SWT.LEFT, SWT.FILL, false, false);
//        gridData.widthHint = 100;
//        gridData.horizontalSpan = 3;
//        btnCancel.setLayoutData(gridData);      

        shell.open();
        shell.pack();

    }
    
    /**
     * Creates the listener for first rulelement combo box.
     *
     * @return the selection listener
     */
    private SelectionListener createListenerForFirstRE() {
    	SelectionListener listener = new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent event) {

                // disposes old color
                if (color != null) {
                    color.dispose();
                }

                color = new Color(firstRuleElements.getParent().getDisplay(), dataController
                        .getColor(RULEELEMENTS
                        		.valueOf(firstRuleElementTypes.getText())));
//                // System.out.println(color.getRGB());

                firstRuleElements.setForeground(color);

//                // System.out.println("combo1 getSelectionIndex(): "
//                        + firstRuleElementTypes.getSelectionIndex());

                updateListsSource();
            }
        };
        return listener;
    }
    
    /**
     * Creates the listener for second ruleelement combo box.
     *
     * @return the selection listener
     */
    private SelectionListener createListenerForSecondRE() {
    	SelectionListener listener = new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent event) {
                // disposes old color
                if (color != null) {
                    color.dispose();
                }
                color = new Color(secondRuleElements.getParent().getDisplay(), dataController
                        .getColor(RULEELEMENTS
                        		.valueOf(secondRuleElementType.getText())));
//                // System.out.println(color.getRGB());
                secondRuleElements.setForeground(color);
                updateListsTarget();
            }
        };
        return listener;
    }
    
    /**
     * Creates the listener for create button.
     *
     * @param shell the shell
     * @return the selection listener
     */
    private SelectionListener createListenerForBCreate(final Shell shell) {
    	SelectionListener listener = new SelectionListener() {
            public void widgetSelected(SelectionEvent e) {
                try {
//                    if (!text.getText().trim().isEmpty()
//                            && !text.getText().equals("Enter name")) {

                        if (firstRuleElementTypes.getSelectionIndex() != -1
                                || secondRuleElementType.getSelectionIndex() != -1) {
                        	OWLNamedIndividual firstRE = ontologyController.getRuleElement(firstRuleElements
                                    .getText(), firstRuleElementTypes.getText());
                        	OWLNamedIndividual secondRE = ontologyController.getRuleElement(secondRuleElements
                                    .getText(), secondRuleElementType.getText());
                            // creates the relation
                        	String description = descriptionText.getText();
                        	if (description.equals(DEFAULT_DESCRIPTION)) { // we don't want to store the default text in the ontology
                        		description = "";
                        	}
                            ontologyController.createNewRelation(firstRE, secondRE, description);

                            shell.close();
                            shell.dispose();
//                            // System.out.println("Relation created!");
//                        } else {
//                            // System.out
//                                    .println("Bitte Namen für die Relation eingeben!");
//                        }
                    }

                } catch (Exception ex) {
                    ex.printStackTrace();
//                    // System.out
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

    /**
     * loads RuleElements of correspondent RuleElementType for Source.
     */
    public void updateListsSource() {
        try {

            // get index of selected RuleElementType in Combobox1
            int i = firstRuleElementTypes.getSelectionIndex();
//            // System.out.println("Index " + firstRuleElementTypes.getSelectionIndex());

            // show correspondent RuleElements in Combobox2
            if (i == 0) {
                firstRuleElements.setItems(data.listToStringArray(data.getRole()));
                firstRuleElements.setText(data.listToStringArray(data.getRole())[0]);
            }
            if (i == 1) {
                firstRuleElements.setItems(data.listToStringArray(data.getActivity()));
                firstRuleElements.setText(data.listToStringArray(data.getActivity())[0]);
            }
            if (i == 2) {
                firstRuleElements.setItems(data.listToStringArray(data.getProperty()));
                firstRuleElements.setText(data.listToStringArray(data.getProperty())[0]);
            }
            if (i == 3) {
                firstRuleElements.setItems(data.listToStringArray(data.getProcess()));
                firstRuleElements.setText(data.listToStringArray(data.getProcess())[0]);
            }
            if (i == 4) {
                firstRuleElements.setItems(data.listToStringArray(data.getArtifact()));
                firstRuleElements.setText(data.listToStringArray(data.getArtifact())[0]);
            }
        } catch (Exception e) {
             e.printStackTrace();
             MessageBox box = new MessageBox(parent);
             box.setMessage("Please define Rule Elements.");
             box.open();
//            // System.out.println("Bitte Regelelemente definieren!");
            firstRuleElements.setText("empty");
        }
    }

    /**
     * loads RuleElements of correspondent RuleElementType for Target.
     */
    public void updateListsTarget() {
        try {

            // get index of selected RuleElementType in Combobox3
            int i = secondRuleElementType.getSelectionIndex();
//            // System.out.println("Index " + secondRuleElementType.getSelectionIndex());

            // show correspondent RuleElements in Combobox4
            if (i == 0) {
                secondRuleElements.setItems(data.listToStringArray(data.getRole()));
                secondRuleElements.setText(data.listToStringArray(data.getRole())[0]);
            }
            if (i == 1) {
                secondRuleElements.setItems(data.listToStringArray(data.getActivity()));
                secondRuleElements.setText(data.listToStringArray(data.getActivity())[0]);
            }
            if (i == 2) {
                secondRuleElements.setItems(data.listToStringArray(data.getProperty()));
                secondRuleElements.setText(data.listToStringArray(data.getProperty())[0]);
            }
            if (i == 3) {
                secondRuleElements.setItems(data.listToStringArray(data.getProcess()));
                secondRuleElements.setText(data.listToStringArray(data.getProcess())[0]);
            }
            if (i == 4) {
                secondRuleElements.setItems(data.listToStringArray(data.getArtifact()));
                secondRuleElements.setText(data.listToStringArray(data.getArtifact())[0]);
            }
        } catch (Exception e) {
             e.printStackTrace();
             MessageBox box = new MessageBox(parent);
             box.setMessage("Please define Rule Elements.");
             box.open();
            // System.out.println("Bitte Regelelemente definieren!");
            secondRuleElements.setText("empty");
        }
    }

}
