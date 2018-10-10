package carisma.xutils.regulatory.ui.first;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Paragraph;
import carisma.regulatory.ontology.Rule;
import carisma.regulatory.ontology.Section;
import carisma.regulatory.ontology.model.RuleElementModel;
import carisma.regulatory.ontology.owl.OWL2BSIElement;
import carisma.regulatory.ontology.owl.OWL2BSIMeasure;
import carisma.regulatory.ontology.owl.OWL2BSIThreat;
import carisma.regulatory.ontology.owl.OWL2Law;
import carisma.regulatory.ontology.owl.OWL2MariskBinding;
import carisma.regulatory.ontology.owl.OWL2MariskComment;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.regulatory.ontology.utils.WrongOrderException;
import carisma.xutils.regulatory.ui.comparator.NaturalOrderComparatorBSIElement;
import carisma.xutils.regulatory.ui.comparator.NaturalOrderComparatorBSIMeasure;
import carisma.xutils.regulatory.ui.comparator.NaturalOrderComparatorBSIThreat;
import carisma.xutils.regulatory.ui.comparator.NaturalOrderComparatorLaw;
import carisma.xutils.regulatory.ui.comparator.NaturalOrderComparatorMaRiskBinding;
import carisma.xutils.regulatory.ui.comparator.NaturalOrderComparatorMaRiskComment;
import carisma.xutils.regulatory.ui.comparator.NaturalOrderComparatorParagraph;
import carisma.xutils.regulatory.ui.comparator.NaturalOrderComparatorSection;
import carisma.xutils.regulatory.ui.controller.DataController;
import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.model.Constants;
import carisma.xutils.regulatory.ui.model.RULEELEMENTS;
import carisma.xutils.regulatory.ui.model.RULES;

/**
 * GUI class for working on Ontology.
 *
 * @author bm
 */
public class RegulationsView extends ViewPart {

	/** The rules. */
	private static List<Rule> rules = null;
    
    /** The color. */
    private Color color;
    
    /** The text. */
    private static StyledText text;
        
    /** The testtext. */
    private String testtext = "";
    /**
     * combo box to display the rule element types.
     */
    private static Combo ruleElementTypesCombo;
    /**
     * combo box to display the rule elements.
     */
    private static Combo ruleElementsCombo;
    /**
     * the tree of rule elements.
     */
    private static Tree RuleTree;

    /** The tree situations. */
    private static Tree situationsTree;

    /** The re popup menu. */
    private static RuleElementPopupMenu rePopupMenu;

    /** The table relations. */
    private static Table relationTable;

    /** The table search. */
    private static Table searchTable;

    /** The ontology controller. */
    private static MainController controller;

    /** The data controller. */
    private DataController dataController;
    
    /** The folder. */
    private static CTabFolder tabContainer;
    
    /** The parent. */
    private static Composite mainComposite = null;
    
    private static DataController data = new DataController();

    /**
     * initializes controllers and list.
     */
    public RegulationsView() {
        controller = MainController.getInstance();
        dataController = new DataController();
        // list = ontologyController.loadOntology();

    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createPartControl(final Composite composite) {

        mainComposite = new Composite(composite, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        mainComposite.setLayout(gridLayout);
        RuleTree = new Tree(mainComposite, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL
                | SWT.SINGLE);
        RuleTree.setLinesVisible(false);
        RuleTree.setHeaderVisible(true);
        Menu searchMenu = new Menu(RuleTree);
        MenuItem rightClickSearch = new MenuItem(searchMenu, SWT.CASCADE);
        rightClickSearch.setText("Search for a word");
        rightClickSearch.addSelectionListener(createListenerForRightClickSearch());
        RuleTree.setMenu(searchMenu);
        GridData gridData = new GridData(SWT.LEFT, SWT.FILL, false, true);
        gridData.heightHint = 400;
        gridData.widthHint = 194;
        RuleTree.setLayoutData(gridData);
        RuleTree.pack();
        createTreeColumn(RuleTree, SWT.NONE, Constants.RULE,
        		Constants.RULE_TREE_COLUMN_WIDTH);

        text = new StyledText(mainComposite, SWT.MULTI | SWT.WRAP | SWT.BORDER
                | SWT.READ_ONLY | SWT.SCROLL_LINE);
        text.setText(testtext);
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.widthHint = 500;
        text.setLayoutData(gridData);

        // creates Menu for creating new RuleElements in textfield
        Menu popupMenu = new Menu(text);
        MenuItem rightClickNewRuleElement = new MenuItem(popupMenu, SWT.CASCADE);
        rightClickNewRuleElement.setText("Create new Rule Element");

        text.setMenu(popupMenu);

        // creates SelectionListener for creating RuleElementPopupMenu in
        // textField
        rightClickNewRuleElement.addSelectionListener(
        		createListenerForRightClickNewRuleElement());

        // creates MouseListener for getting content of selected section in the
        // tree
        RuleTree.addMouseListener(createMouseListenerForRuleTree());

        Composite ruleElementsTypeComposite = createComposite(mainComposite, SWT.NONE,
        		Constants.TWO_COLUMN, false);

        ruleElementTypesCombo = new Combo(ruleElementsTypeComposite, SWT.READ_ONLY);
        ruleElementTypesCombo.setItems(Constants.RULEELEMENTS); 	// TODO change to RULEELEMENTS
        ruleElementsTypeComposite.setEnabled(true);
        ruleElementTypesCombo.select(0);
        ruleElementTypesCombo.setText("Rule Element Types");
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.widthHint = 100;
        ruleElementTypesCombo.setLayoutData(gridData);

        ruleElementsCombo = new Combo(ruleElementsTypeComposite, SWT.NONE);
//        ruleElementsCombo.setText("Rule Elements");
        	// load the ruleelements from which ruleelement type is selected
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.widthHint = 100;
        ruleElementsCombo.setLayoutData(gridData);

        // creates SelectionListener for loading RuleElements of correspondent
        // RuleElementType
        ruleElementTypesCombo.addSelectionListener(
        		createListenerForRuleElementsCombo());

        Button bMark = new Button(ruleElementsTypeComposite, SWT.PUSH);
        bMark.setText("Mark Rule Element");
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.horizontalSpan = 2;
        gridData.widthHint = 100;
        bMark.setLayoutData(gridData);

        bMark.addSelectionListener(createSelectionListenerForBMark(mainComposite));

        Button bDefineSituation = new Button(ruleElementsTypeComposite, SWT.PUSH);
        bDefineSituation.setText("Define Situation");
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.widthHint = 100;
        gridData.horizontalSpan = 2;
        bDefineSituation.setLayoutData(gridData);

        // creates SelectionListener for creating SituationPopupMenu
        bDefineSituation.addSelectionListener(
        		createListenerForBDefineSituation());

        Button bDefineRelation = new Button(ruleElementsTypeComposite, SWT.PUSH);
        bDefineRelation.setText("Define Relation");
        gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        gridData.widthHint = 100;
        gridData.horizontalSpan = 2;
        bDefineRelation.setLayoutData(gridData);

        // creates SelectionListener for creating RelationPopupMenu
        bDefineRelation.addSelectionListener(
        		createListenerForBDefineRelation());

        tabContainer = new CTabFolder(mainComposite, SWT.TOP);
        gridData = new GridData(GridData.FILL, GridData.FILL, true, true, 1, 1);
        tabContainer.setLayoutData(gridData);
        tabContainer.setFocus();
        CTabItem situationsTab = createCTabItem(tabContainer, SWT.NONE,
        		Constants.SITUATION);
        CTabItem relationsTab = createCTabItem(tabContainer, SWT.NONE,
        		Constants.RELATION);
        CTabItem searchResultsTab = createCTabItem(tabContainer, SWT.NONE,
        		Constants.SEARCH_RESULT);

        Composite sitationsTabComposite = createComposite(tabContainer,
        		SWT.NONE, Constants.ONE_COLUMN, false);
        situationsTab.setControl(sitationsTabComposite);

        situationsTree = new Tree(sitationsTabComposite,
        		SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        situationsTree.setHeaderVisible(true);
        situationsTree.setLinesVisible(false);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        situationsTree.setLayoutData(gridData);
        createTreeColumn(situationsTree, SWT.NONE, Constants.SITUATION,
        		Constants.SITUATIONS_TREE_COLUMN_WIDTH);
// TODO used for the screenshot 
//        TreeItem item = new TreeItem(situationsTree, SWT.NONE);
//        item.setText("Vier Augen Prinzip");
//        TreeItem constraint = new TreeItem(item, SWT.NONE);
//        constraint.setText("Restriktion");
//        TreeItem sod = new TreeItem(constraint, SWT.NONE);
//        sod.setText("Separation of Duty");
//        TreeItem ruleelement = new TreeItem(item, SWT.NONE);
//        ruleelement.setText("Regelelemente");
//        TreeItem datenschutz = new TreeItem(ruleelement, SWT.NONE);
//        datenschutz.setText("Datenschutz");
//        TreeItem verwendung = new TreeItem(ruleelement, SWT.NONE);
//        verwendung.setText("Verwendung von Daten");
// =========================================================
        situationsTree.pack();
        Composite relationsTabComposite = createComposite(tabContainer, SWT.NONE,
        		Constants.ONE_COLUMN, false);
        relationsTab.setControl(relationsTabComposite);
        
        relationTable = new Table(relationsTabComposite,
        		SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        relationTable.setHeaderVisible(true);
        relationTable.setLinesVisible(true);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.heightHint = 100;
        relationTable.setLayoutData(gridData);
        createTableColumn(relationTable, SWT.NONE, Constants.SOURCE,
        		Constants.RELATION_TABLE_COLUMN_WIDTH);
        createTableColumn(relationTable, SWT.NONE, Constants.RELATION,
        		Constants.RELATION_TABLE_COLUMN_WIDTH);
        createTableColumn(relationTable, SWT.NONE, Constants.TARGET,
        		Constants.RELATION_TABLE_COLUMN_WIDTH);
        relationTable.pack();

        Composite searchResultComposite = createComposite(tabContainer, SWT.NONE,
        		Constants.ONE_COLUMN, false);
        searchResultsTab.setControl(searchResultComposite);

        searchTable = new Table(searchResultComposite, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        searchTable.setHeaderVisible(true);
        searchTable.setLinesVisible(true);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.heightHint = 100;
        searchTable.setLayoutData(gridData);

        createTableColumn(searchTable, SWT.NONE, Constants.REGULATION,
        		Constants.SEARCH_TABLE_COLUMN_WIDTH);
        createTableColumn(searchTable, SWT.NONE, Constants.RULE,
        		Constants.SEARCH_TABLE_COLUMN_WIDTH);
        createTableColumn(searchTable, SWT.NONE, Constants.SEARCH_TERM,
        		Constants.SEARCH_TABLE_COLUMN_WIDTH);
        searchTable.pack();
        // creates MouseListener for getting content of selected section in the
        // SearchTable
        searchTable.addMouseListener(createListenerForSearchTable());

        // Hotkey for Search-function (CTRL+F)
        RuleTree.getParent().getDisplay().addFilter(
        		SWT.KeyDown, creatListenerForHotkey());

    }

    private SelectionListener createListenerForRightClickSearch() {
		SelectionListener listener = new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				search(RuleTree.getSelection());				
			}			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				// nothing to do				
			}
		};
		return listener;
	}

	/**
     * Creates the composite.
     *
     * @param parent the parent
     * @param style the style
     * @param numColumns the num columns
     * @param makeColumnsEqualWidth the make columns equal width
     * @return the composite
     */
    private Composite createComposite(final Composite parent, final int style,
    		final int numColumns, final boolean makeColumnsEqualWidth) {
    	Composite composite = new Composite(parent, style);
        composite.setLayout(new GridLayout(numColumns, makeColumnsEqualWidth));
        return composite;
    }
    
    /**
     * Creates the c tab item.
     *
     * @param folder the folder
     * @param style the style
     * @param text the text
     * @return the c tab item
     */
    private CTabItem createCTabItem(final CTabFolder folder,
    		final int style, final String text) {
    	CTabItem item = new CTabItem(folder, SWT.NONE);
        item.setText(text);
        return item;
    }

    /**
     * Creates the tree column.
     *
     * @param parent the parent
     * @param style the style
     * @param text the text
     * @param width the width
     * @return the tree column
     */
    private TreeColumn createTreeColumn(final Tree parent,
    		final int style, final String text, final int width) {
    	TreeColumn column = new TreeColumn(parent, style);
        column.setText(text);
        column.setWidth(width);
        return column;
    }
    
    /**
     * Creates the table column.
     *
     * @param parent the parent
     * @param style the style
     * @param text the text
     * @param width the width
     * @return the table column
     */
    private TableColumn createTableColumn(final Table parent,
    		final int style, final String text, final int width) {
    	TableColumn column = new TableColumn(parent, style);
        column.setText(text);
        column.setWidth(width); 
        return column;
    }
    
    /**
     * Creat listener for hotkey.
     *
     * @return the listener
     */
    private Listener creatListenerForHotkey() {
    	Listener listener = new Listener() {
            public void handleEvent(Event e) {
                if (((e.stateMask & SWT.CTRL) == SWT.CTRL)
                        && (e.keyCode == 'f')) {

                    // if active View has the title "Regularien" the search gets
                    // started
                    if (PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                            .getActivePage().getActivePart().getTitle()
                            .equals("Regulations")) {
                    	
                        search(RuleTree.getSelection());
                    }
                }

            }
        };
        return listener;
    }
    		// TODO check if all following methods are needed...
    /**
		     * Creates the listener for search table.
		     *
		     * @return the mouse listener
		     */
    private MouseListener createListenerForSearchTable() {
    	MouseListener listener = new MouseListener() {
            public void mouseDown(MouseEvent e) {
            }

            public void mouseUp(MouseEvent e) {

            }

            public void mouseDoubleClick(MouseEvent e) {
                try {
                    // gets content of section
                    getOntologyContentInSearchTable();

                } catch (Exception ex) {
                	controller.makeMessageBox(mainComposite.getShell(),
                			"Please choose a Paragraph!");
//                    System.out.println("Please choose a paragraph!");
                    ex.printStackTrace();
                }
            }
        };
        return listener;
    }
    
    /**
     * Creates the listener for b define relation.
     *
     * @return the selection listener
     */
    private SelectionListener createListenerForBDefineRelation() {
    	SelectionListener listener = new SelectionListener() {
            public void widgetSelected(final SelectionEvent e) {
                try {
                    TreeItem[] array = RuleTree.getSelection();
                    if (array.length < 1) {
                    	throw new Exception();
                    }
                    for (int i = 0; i < array.length; i++) {
                    	// creates the RelationPopupMenu
                    	if (controller.isDataPresent(array[i])) {
	                        new RelationPopupMenu(new Shell(), controller
	                                .getRuleElementInstances(
	                                		(String) array[i].getData(
	                                				Constants.RULE_NUMBER),
	                                		(String) array[i].getData(
	                                				Constants.RULE_CLAZZ)));
                    	} else {
                    		throw new Exception();
                    	}
                    }
                } catch (Exception ex) {
                	controller.makeMessageBox(mainComposite.getShell(),
                			"Please choose Regulation!");
                }
            }
            public void widgetDefaultSelected(final SelectionEvent e) {
            }
        };
        return listener;
    }
    
    

    /**
     * Creates the listener for b define situation.
     *
     * @return the selection listener
     */
    private SelectionListener createListenerForBDefineSituation() {
    	SelectionListener listener = new SelectionListener() {
            public void widgetSelected(SelectionEvent e) {
                try {
                    // gets indices of section
                    TreeItem[] rules = RuleTree.getSelection();
//                    System.out.println("Selected from the laws... " + rules[0].getText());

                    // creates the SituationPopupMenu with indices of
                    // RuleElementInstances
                    for (int i = 0; i < rules.length; i++) {
                    	if (controller.isDataPresent(rules[i])) {
		                    new SituationPopupMenu(new Shell(), controller
		                            .getRuleElementInstances(
		                            		(String) rules[i].getData(Constants.RULE_NUMBER),
		                            		(String) rules[i].getData(Constants.RULE_CLAZZ)),
		                            		(String) rules[i].getData(Constants.RULE_NUMBER),
		                            		(String) rules[i].getData(Constants.RULE_CLAZZ));
                    	} else {
                    		throw new Exception();
                    	}
                    }
                } catch (Exception ex) {
                	controller.makeMessageBox(mainComposite.getShell(),
                			"Please choose a Regulation!");
                }
            }

            public void widgetDefaultSelected(SelectionEvent e) {
            }
        };
        return listener;
    }
    
    /**
     * Creates the mouse listener for rule tree.
     *
     * @return the mouse listener
     */
    private MouseListener createMouseListenerForRuleTree() {
    	MouseListener listener = new MouseListener() {
            public void mouseDown(MouseEvent e) {
            }

            public void mouseUp(MouseEvent e) {

            }

            public void mouseDoubleClick(MouseEvent e) {
                try {

                    if (ConfigurationView.getLabelList() == null) {

                        // if no RuleElements are created, the Configuration-tab
                        // must be called to initialize variables and create
                        // tree
                        try {
                            IWorkbenchPage activePage = PlatformUI
                                    .getWorkbench().getActiveWorkbenchWindow()
                                    .getActivePage();
                            String viewIdToOpen = Constants.CLS_CONFIGURATION;
                            activePage.showView(viewIdToOpen);
                        } catch (PartInitException p) {
                            p.printStackTrace();
                        }
                        controller.makeMessageBox(mainComposite.getShell(),
                        		"There are no Rule Elements defined. Please load an Ontology or define new Rule Elements in the Configuration tab.");
                    } else {

                        // gets content of section
//                        getOntologyContentTree();		// takes much too long^^
                        TreeItem[] array = RuleTree.getSelection();
                        if (array.length > 0) {
                        	TreeItem item = array[0];
                        	String number = (String) item.getData(Constants.RULE_NUMBER);
                        	String clazz = (String) item.getData(Constants.RULE_CLAZZ);
                        		// workaround because the class definition in the ROH
                        		// doesn't match the class names of Marisk classes
                        	if (clazz.equals("MARiskBinding")) {
                    			clazz = "MariskBinding";
                    		} else if (clazz.equals("MARiskComment")) {
                    			clazz = "MariskComment";
                    		}
//                        	System.out.println(item.getData(Constants.RULE_NUMBER));
//                        	System.out.println(item.getData(Constants.RULE_CLAZZ));
                        	if (controller.isDataPresent(array[0])) {                        		
	                        	text.setText(controller.
	                        			getTextFromIndividual(
	                        					(String) item.getData(Constants.RULE_NUMBER),
	                        					clazz));
	                        	text.setData(Constants.RULE_NUMBER, number);
	                        	text.setData(Constants.RULE_CLAZZ, clazz);
                        	} else {
                        		controller.makeMessageBox(mainComposite.getShell(), "No Regulation selected.");
                        		return;
                        	}
                            // shows highlights in the text
                            setHighlights(controller
                                    .getRuleElementInstances(
                                    		(String) item.getData(Constants.RULE_NUMBER),
                                    		(String) item.getData(Constants.RULE_CLAZZ)));

                            // shows existing relations in the table
                            fillTheRelationsTable(controller
                                    .getRuleElementInstances(
                                    		(String) item.getData(Constants.RULE_NUMBER),
                                    		(String) item.getData(Constants.RULE_CLAZZ)));
                            // shows existing situations in the table
                            fillSituationTree(
                            		(String) item.getData(Constants.RULE_NUMBER));
                        } else {
                        	MessageBox box = new MessageBox(mainComposite.getShell());
                        	box.setMessage("Please select an Entry.");
                        	box.open();
                        }
                    }
                } catch (Exception ex) {
                	controller.makeMessageBox(mainComposite.getShell(),
                			"Please choose a Regulation!");
                	ex.printStackTrace();
//                    System.out.println("Bitte einen Paragraphen auswählen!");
                }
            }
        };
        return listener;
    }
    
    /**
     * Creates the listener for menu item.
     *
     * @return the selection listener
     */
    private SelectionListener createListenerForRightClickNewRuleElement() {
    	SelectionListener listener = new SelectionListener() {
            public void widgetSelected(final SelectionEvent e) {
                if (ConfigurationView.getLabelList() == null) {
                    // if no RuleElements are created, the Configuration-tab
                    // must be called to initialize variables and create tree
                    try {
                        IWorkbenchPage activePage = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getActivePage();
                        String viewIdToOpen = Constants.CLS_CONFIGURATION;
                        activePage.showView(viewIdToOpen);
                    } catch (PartInitException p) {
                        p.printStackTrace();
                    }
//                    System.out.println(
//                    		"Define colours in the configuration tab!");
                } else {
                    // creates the RuleElementPopupMenu
                    rePopupMenu = new RuleElementPopupMenu(new Shell(),
                            dataController);
                }
            }
            public void widgetDefaultSelected(final SelectionEvent e) {
            }

        };
        return listener;
    }
    
    /**
     * Creates the listener for rule elements combo.
     *
     * @return the selection listener
     */
    private SelectionListener createListenerForRuleElementsCombo() {
    	SelectionListener listener = new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {

                if (ConfigurationView.getLabelList() == null) {

                    // if no RuleElements are created, the Configuration-tab
                    // must be called to initialize variables and create tree
                    try {
                        IWorkbenchPage activePage = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getActivePage();
                        String viewIdToOpen = Constants.CLS_CONFIGURATION;
                        activePage.showView(viewIdToOpen);
                    } catch (PartInitException p) {
                        p.printStackTrace();
                    }
                    controller.makeMessageBox(mainComposite.getShell(),
                    		"There are no Rule Elements defined. Please load an Ontology or define new Rule Elements in the Configuration tab.");
//                    System.out.println("Define colours in the configuration tab!");
                } else {
                    updateEntriesOfRuleElementComboBox();
                }
            }
        };
    	return listener;
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        // nothing to do
    }
    
    public static void preLoadRuleElement() {
    	if (ConfigurationView.getLabelList() == null) {

            // if no RuleElements are created, the Configuration-tab
            // must be called to initialize variables and create tree
            try {
                IWorkbenchPage activePage = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage();
                String viewIdToOpen = Constants.CLS_CONFIGURATION;
                activePage.showView(viewIdToOpen);
            } catch (PartInitException p) {
                p.printStackTrace();
            }
            controller.makeMessageBox(mainComposite.getShell(),
            		"There are no Rule Elements defined. Please load an Ontology or define new Rule Elements in the Configuration tab.");
//            System.out.println("Define colours in the configuration tab!");
        } else {
            updateEntriesOfRuleElementComboBox();
        }
    }

    /**
     * loads RuleElements of correspondent RuleElementType into comboBox.
     */
    public static void updateEntriesOfRuleElementComboBox() {
        try {

            // get index of selected RuleElementType in Combobox1
            int selectionIndex = ruleElementTypesCombo.getSelectionIndex();
//            System.out.println("Index of the ruleElementComboBox "
//            + ruleElementTypesCombo.getSelectionIndex());
         // show correspondent RuleElements in Combobox2
            switch (selectionIndex) {
			case 0:
				setItemsForCombobox(data.getRole());
				break;
			case 1:
				setItemsForCombobox(data.getActivity());
				break;
			case 2:
				setItemsForCombobox(data.getProperty());
				break;
			case 3:
				setItemsForCombobox(data.getProcess());
				break;
			case 4:
				setItemsForCombobox(data.getArtifact());
				break;
			default:
				break;
			}
        } catch (Exception e) {
        	controller.makeMessageBox(mainComposite.getShell(),
        			"Please define Rule Elements!");
             e.printStackTrace();
//            System.out.println("Please define ruleelements!");
            ruleElementsCombo.setText("empty");
        }
    }
    
    /**
     * Sets the items for combobox.
     *
     * @param list the new items for combobox
     */
    private static void setItemsForCombobox(final List<String> list) {
    	ruleElementsCombo.setItems(data.listToStringArray(list));
        ruleElementsCombo.setText(data.listToStringArray(list)[0]);
    }
    
    /**
     * creates SelectionListener for creating new RuleElements and marking
     * words in sections.
     *
     * @param parent the parent
     * @return the selection listener
     */
    private SelectionListener createSelectionListenerForBMark(
    		final Composite parent) {
    	SelectionListener listener = new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent event) {

                if (ConfigurationView.getLabelList() == null) {

                    // if no RuleElements are created, the Configuration-tab
                    // must be called to initialize variables and create tree
                    try {
                        IWorkbenchPage activePage = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getActivePage();
                        String viewIdToOpen = Constants.CLS_CONFIGURATION;
                        activePage.showView(viewIdToOpen);
                    } catch (PartInitException p) {
                        p.printStackTrace();
                    }

//                    System.out.println(
//                    		"Define colours in the configuration tab!");
                } else {

                    // saves combo2 input-text for checking later
                    String combo2Text = ruleElementsCombo.getText(); 	//.replace('_', ' ');

                    // if no RuleElementType is selected, the first one will be
                    // chosen
                    if (ruleElementTypesCombo.getSelectionIndex() != -1) {
                        // combo1.select(0);
                        // updateLists();

                        // gets index of RuleElementType
                        int i = ruleElementTypesCombo.getSelectionIndex();

                        // for checking if text is selected then inserting new
                        // RuleElement in correspondent list
                        int r_length = text.getSelectionRange().y;

                        // if there are no RuleElements in Combobox2 you see
                        // "empty"
                        if (arePreconditionsGranted(i, 0, r_length)) {
                            addREinConfiguration(
                                    ruleElementsCombo.getText(), 	//.replace('_', ' '),
                                    ruleElementTypesCombo.getItem(i));

                        }
                        if (arePreconditionsGranted(i, 1, r_length)) {
                            addREinConfiguration(
                                    ruleElementsCombo.getText(), 	//.replace('_', ' '),
                                    ruleElementTypesCombo.getItem(i));

                        }
                        if (arePreconditionsGranted(i, 2, r_length)) {
                            addREinConfiguration(
                                    ruleElementsCombo.getText(),	//.replace('_', ' '),
                                    ruleElementTypesCombo.getItem(i));

                        }
                        if (arePreconditionsGranted(i, 3, r_length)) {
                        	addREinConfiguration(
                                    ruleElementsCombo.getText(), 	//.replace('_', ' '),
                                    ruleElementTypesCombo.getItem(i));

                        }
                        if (arePreconditionsGranted(i, 4, r_length)) {
                            addREinConfiguration(
                                    ruleElementsCombo.getText(), 	//.replace('_', ' '),
                                    ruleElementTypesCombo.getItem(i));

                        }

                        // check if original combo2 input-text is not empty
                        if (!(combo2Text.trim().isEmpty())
                                && !(combo2Text.equals("empty"))) {
                            ruleElementsCombo.setText(combo2Text);

//                            System.out.println(text.getSelectionRange());

                            // only if length of selected text is bigger than 0
                            // it gets colored
                            if (isSelectedTextBiggerThanZero()) {

                                // defining StyleRange for coloring text
                                StyleRange style = new StyleRange();
                                style.start = text.getSelectionRange().x;
                                style.length = text.getSelectionRange().y;
//                                System.out.println("Selection Index: " + style.start + " , " + style.length);

                                // checks if colors are defined
                                if ((parent.getDisplay() == null)
                                        || (dataController.getColor(RULEELEMENTS.valueOf(
                                        		ruleElementTypesCombo.getItem(
                        						ruleElementTypesCombo.getSelectionIndex()))) 
                        						== null)) {
                                  
//                                    System.out.println("Colours have not yet been initialized!");
                                } else {
                                    color = new Color(parent.getDisplay(),
                                            dataController.getColor(RULEELEMENTS.valueOf(
                                            		ruleElementTypesCombo.getItem(
                                            		ruleElementTypesCombo.getSelectionIndex()))));
                                }
                                style.foreground = color;
                                text.setStyleRange(style);

                                // create new RuleElement with startIndex and
                                // endIndex

                                // gets index of RuleElementType
                                String ruleElementType = ruleElementTypesCombo
                                		.getItem(ruleElementTypesCombo
                                				.getSelectionIndex());

                                // creates new RuleElementInstance
                                controller
                                        .createNewRuleElementInstance(		
                                        		getOntologySelectedRule(),	
                                                ruleElementsCombo.getText(),
                                                ruleElementType,
                                                text.getSelectionText(),
                                                style.start,
                                                (style.start + style.length));
                            }
                        }
                        controller.updateRuleElements();
                        updateEntriesOfRuleElementComboBox();
                        ruleElementsCombo.setText(combo2Text);
                        text.setSelection(0, 0);	// TODO check
                    }
                }
            }
        };
        return listener;
    }
    
    /**
     * check if several Preconditions are fulfilled.
     * - ruleElementCombo must not be empty
     * - ruleElementCombo text must not equal 'empty'
     * - length of selected text must not be zero
     * - the index i should match a given target
     *
     * @param i the i
     * @param target the target
     * @param r_length the r_length
     * @return true, if successful
     */
    private boolean arePreconditionsGranted(final int i, final int target, final int r_length) {
    	return (i == target && !(ruleElementsCombo.getText().trim().isEmpty())
                && !(ruleElementsCombo.getText().equals("empty"))
                && r_length > 0);
    }
    
    /**
     * checks if the selected text is bigger than zero.
     *
     * @return true, if is selected text bigger than zero
     */
    private boolean isSelectedTextBiggerThanZero() {
    	return(text.getSelectionRange().y > 0
    			&& !text.getSelectionText().equals(" ")
                && !(ruleElementsCombo.getText().trim().isEmpty())
                && !(ruleElementsCombo.getText().equals("empty")));
    }

    /**
     * loads Ontology from given Law-list into tree.
     */
    public static void fillTheRuleTree() {
        try {
        	List<OWL2MariskBinding> bindings = new ArrayList<OWL2MariskBinding>();
        	List<OWL2MariskComment> comments = new ArrayList<OWL2MariskComment>();
        	List<OWL2BSIElement> bsiElements = new ArrayList<OWL2BSIElement>();
        	List<OWL2BSIMeasure> bsiMeasures = new ArrayList<OWL2BSIMeasure>();
        	List<OWL2BSIThreat> bsithreats = new ArrayList<OWL2BSIThreat>();
        	List<OWL2Law> laws = new ArrayList<OWL2Law>();
            // adds Laws to tree
            for (Rule r : rules) {
            	String ruleName = r.getClass().getName();
            	switch (RULES.valueOf(ruleName.substring(ruleName.indexOf("OWL2") + 4))) {
				case Law:
					laws.add((OWL2Law) r);
					break;
				case BSIElement:
					bsiElements.add((OWL2BSIElement) r);
					break;
				case BSIMeasure:
					bsiMeasures.add((OWL2BSIMeasure) r);
					break;
				case BSIThreat:
					bsithreats.add((OWL2BSIThreat) r);
					break;
				case MariskBinding:	
					bindings.add((OWL2MariskBinding) r);					
					break;
				case MariskComment:
					comments.add((OWL2MariskComment) r);
					break;
				default:
					break;
				}
                
            }
            createLawEntry(laws);
            sortListOfRules(bindings, comments, bsiElements, bsiMeasures, bsithreats);
//            RuleTree.getColumn(0).pack();		// not really needed, used to make the column smaller
        } catch (Exception e) {
             e.printStackTrace();
//            System.out.println("Ontology could not be load!");

        }

    }
    
    /**
     * this method sorts the given list. Therfore it uses the natural order comparator.
     *
     * @param bindings the bindings
     * @param comments the comments
     * @param bsiElements the bsi elements
     * @param bsiMeasures the bsi measures
     * @param bsithreats the bsithreats
     */
    private static void sortListOfRules(List<OWL2MariskBinding> bindings,
			List<OWL2MariskComment> comments, List<OWL2BSIElement> bsiElements,
			List<OWL2BSIMeasure> bsiMeasures, List<OWL2BSIThreat> bsithreats) {
    	Collections.sort(bindings, new NaturalOrderComparatorMaRiskBinding());
    	Collections.sort(comments, new NaturalOrderComparatorMaRiskComment());
    	Collections.sort(bsiElements, new NaturalOrderComparatorBSIElement());
    	Collections.sort(bsiMeasures, new NaturalOrderComparatorBSIMeasure());
    	Collections.sort(bsithreats, new NaturalOrderComparatorBSIThreat());
    	createBSITreeElements(bsiElements, bsiMeasures, bsithreats);
    	createMaRiskTreeElements(bindings, comments);
	}

	/**
	 * creates new tree elements and appends the elements from the given lists
	 * to the correct tree element.
	 *
	 * @param bsiElements the bsi elements
	 * @param bsiMeasures the bsi measures
	 * @param bsithreats the bsithreats
	 */
	private static void createBSITreeElements(List<OWL2BSIElement> bsiElements,
			List<OWL2BSIMeasure> bsiMeasures, List<OWL2BSIThreat> bsithreats) {
		TreeItem bsi = new TreeItem(RuleTree, SWT.NONE);
    	bsi.setText("BSI");
    	TreeItem bsiElement = new TreeItem(bsi, SWT.NONE);
    	bsiElement.setText("BSIElement");
    	TreeItem bsiMeasure = new TreeItem(bsi, SWT.NONE);
    	bsiMeasure.setText("BSIMeasure");
    	TreeItem bsiThreat = new TreeItem(bsi, SWT.NONE);
    	bsiThreat.setText("BSIThreat");
    	for (OWL2BSIElement element : bsiElements) {
    		createBSIElement(element, bsiElement);
    	}
    	for (OWL2BSIMeasure measure : bsiMeasures) {
    		createBSIMeasure(measure, bsiMeasure);
    	}
    	for (OWL2BSIThreat threat : bsithreats) {
    		createBSIThreat(threat, bsiThreat);
    	}
		
	}

	/**
	 * creates new tree elements and appends the elements from the given lists
	 * to the correct tree element.
	 *
	 * @param bindings the bindings
	 * @param comments the comments
	 */
	private static void createMaRiskTreeElements(List<OWL2MariskBinding> bindings,
			List<OWL2MariskComment> comments) {
		TreeItem mariskVA = new TreeItem(RuleTree, SWT.NONE);
    	mariskVA.setText("MaRiskVA");
    	TreeItem mariskVABinding = new TreeItem(mariskVA, SWT.NONE);
    	mariskVABinding.setText("MaRiskBinding");
    	TreeItem mariskVAComment = new TreeItem(mariskVA, SWT.NONE);
    	mariskVAComment.setText("MaRiskComment");
		for (OWL2MariskBinding binding : bindings) {
			createMaRiskVABinding(binding, mariskVABinding);
		}
		for (OWL2MariskComment comment : comments) {
			createMaRiskComment(comment, mariskVAComment);
		}
	}

	/**
	 * Creates the ma risk comment.
	 *
	 * @param mc the mc
	 * @param mariskVAComment the marisk va comment
	 */
	private static void createMaRiskComment(OWL2MariskComment mc, TreeItem mariskVAComment) {
		TreeItem c = new TreeItem(mariskVAComment, SWT.NONE);
		String title = mc.getOWLNamedIndividual().getIRI().getFragment();
		c.setText(title);
		c.setData(Constants.RULE_NUMBER, title);
		c.setData(Constants.RULE_CLAZZ, "MariskComment");
	}

	/**
	 * Creates the ma risk va binding.
	 *
	 * @param mb the mb
	 * @param mariskVABinding the marisk va binding
	 */
	private static void createMaRiskVABinding(OWL2MariskBinding mb, TreeItem mariskVABinding) {
		TreeItem b = new TreeItem(mariskVABinding, SWT.NONE);
		String title = mb.getOWLNamedIndividual().getIRI().getFragment();
		b.setText(title);
		b.setData(Constants.RULE_NUMBER, title);
		b.setData(Constants.RULE_CLAZZ, "MARiskBinding");
	}

	/**
	 * Creates the bsi threat.
	 *
	 * @param threat the threat
	 * @param bsiThreat the bsi threat
	 */
	private static void createBSIThreat(OWL2BSIThreat threat, TreeItem bsiThreat) {
	   	String number = threat.getOWLNamedIndividual().getIRI().getFragment();
	   	TreeItem t = new TreeItem(bsiThreat, SWT.NONE);
	   	t.setText(number + " " + threat.getTitle()); 	
		t.setData(Constants.RULE_NUMBER, number);
		t.setData(Constants.RULE_CLAZZ, RegulatoryOntologyHelper.CLS_BSITHREAT);
	}

	/**
	 * Creates the bsi measure.
	 *
	 * @param measure the measure
	 * @param bsiMeasure the bsi measure
	 */
	private static void createBSIMeasure(OWL2BSIMeasure measure, TreeItem bsiMeasure) {
    	String number = measure.getOWLNamedIndividual().getIRI().getFragment();
    	TreeItem m = new TreeItem(bsiMeasure, SWT.NONE);
   	 	m.setText(number + " " + measure.getTitle());
   	 	m.setData(Constants.RULE_NUMBER, number);
   	 	m.setData(Constants.RULE_CLAZZ, RegulatoryOntologyHelper.CLS_BSIMEASURE);
		
	}

	/**
	 * Creates the bsi element.
	 *
	 * @param element the element
	 * @param bsiElement the bsi element
	 */
	private static void createBSIElement(final OWL2BSIElement element, final TreeItem bsiElement) {
    	String number = element.getOWLNamedIndividual().getIRI().getFragment();
	   	TreeItem e = new TreeItem(bsiElement, SWT.NONE);
	   	e.setText(number + " " + element.getTitle());
	   	e.setData(Constants.RULE_NUMBER, number);
	   	e.setData(Constants.RULE_CLAZZ, RegulatoryOntologyHelper.CLS_BSIELEMENT);
	}

	/**
	 * Creates the law entry.
	 *
	 * @param laws the laws
	 */
	private static void createLawEntry(final List<OWL2Law> laws) {
		Collections.sort(laws, new NaturalOrderComparatorLaw());
		for (OWL2Law l : laws) {
	        TreeItem law = new TreeItem(RuleTree, SWT.NONE);
	        law.setText(l.getTitle());
	        ArrayList<Paragraph> paragraphs = new ArrayList<Paragraph>(
	                l.getParagraphs());
	        // sorts Paragraph-elements
	        Collections.sort(paragraphs, new NaturalOrderComparatorParagraph());
	        int counter = 0;
	        // adds Paragraphs to tree
	        for (Paragraph p : paragraphs) {
	        	counter++;
	            if (!p.getSections().isEmpty()) {		// to provide of empty paragraphs
	                TreeItem paragraph = new TreeItem(law, SWT.NONE);
	                paragraph.setText("§ " + p.getNumber());
	                paragraph.setData(Constants.RULE_NUMBER, p.getNumber());
	                paragraph.setData(Constants.RULE_CLAZZ, RegulatoryOntologyHelper.CLS_PARAGRAPH);
	//                       System.out.println("  " + p.getNumber());
	                 ArrayList<Section> sections = new ArrayList<Section>(
	                        p.getSections());
	                // sorts Section-elements
	                 Collections.sort(sections, new NaturalOrderComparatorSection());
	                 // adds Sections to tree
	                for (Section s : sections) {	
	                    TreeItem section = new TreeItem(paragraph, SWT.NONE);
	                    section.setText(s.getNumber());
	                    section.setData(Constants.RULE_NUMBER, s.getNumber());
	                    section.setData(Constants.RULE_CLAZZ, RegulatoryOntologyHelper.CLS_SECTION);
	                }
	            }
	        }
	        System.out.println("Added " + counter + " paragraphs.");
	        counter = 0;
		}
       
   }

    /**
     * gets content of selected section in the tree.
     *
     * @return the ontology content tree
     */
//    public void getOntologyContentTree() {		// not useful now, takes about 10 seconds to update the textbox^^
//        try {
//
//            TreeItem[] array = RuleTree.getSelection();
//
//            // for (Rule r : onto.getRules()) {
//            // if (r instanceof Law) {
//            // Law l = (Law) r;
//
//            for (Rule l : rules) {
//
//                // check name of given Law
//                if (l.getTitle().equals(
//                        array[0].getParentItem().getParentItem().getText())) {
//
//                    for (Paragraph p : l.getParagraphs()) {
//
//                        // check name of given Paragraph
//                        if (("§ " + p.getNumber()).equals(array[0]
//                                .getParentItem().getText())) {
//
//                            for (Section s : p.getSections()) {
//
//                                // check name of given Section
//                                if ((s.getNumber()).equals(array[0].getText())) {
//
//                                    text.setText(s.getContent());
//                                    System.out.println(s.getContent());
//                                }
//
//                            }
//                        }
//
//                    }
//
//                }
//
//                RuleTree.getColumn(0).pack();
//
//            }
//
//        } catch (Exception e) {
//             e.printStackTrace();
//             controller.makeMessageBox("Please choose a section!");
////            System.out.println("Bitte einen Absatz auswählen!");
//
//        }
//
//    }

    /**
     * gets content of selected section in the SearchTable.
     */
    public void getOntologyContentInSearchTable() {
        try {
            TableItem[] array = searchTable.getSelection();
            TableItem selectionItem = null;
            if (array.length == 1) {
            	selectionItem = array[0];
            } else {
            	controller.makeMessageBox(mainComposite.getShell(),
            			"There are more than one element selected.");
            	return;
            }
            text.setText(controller.getTextFromIndividual(
            		(String) selectionItem.getData(Constants.RULE_NUMBER),
            		(String) selectionItem.getData(Constants.RULE_CLAZZ)));

            // while selecting item in searchResults, the correspondent
            // item in tree should be selected too
            for (TreeItem rule : RuleTree.getItems()) {
            	for (TreeItem entry : rule.getItems()) {
                   	if (entry.getItems().length > 0) {
                   		for (TreeItem subEntry : entry.getItems()) {
                   			if (isItemMatching(
                   					(String) selectionItem.getData(Constants.RULE_NUMBER),
                   					(String) selectionItem.getData(Constants.RULE_CLAZZ),
                   					(String) subEntry.getData(Constants.RULE_NUMBER),
                   					(String) subEntry.getData(Constants.RULE_CLAZZ))) {
                   				RuleTree.setSelection(subEntry);
                   			}
                   		}
                   	} else {
                   		if (isItemMatching(
               					(String) selectionItem.getData(Constants.RULE_NUMBER),
               					(String) selectionItem.getData(Constants.RULE_CLAZZ),
               					(String) entry.getData(Constants.RULE_NUMBER),
               					(String) entry.getData(Constants.RULE_CLAZZ))) {
               				RuleTree.setSelection(entry);
                   		}
                   	}
               }
               RuleTree.getColumn(0).pack();
           }
       } catch (Exception e) {
           e.printStackTrace();
           controller.makeMessageBox(mainComposite.getShell(),
        		   "Please choose a search result!");
        }
    }

    private boolean isItemMatching(final String firstNumber, final String firstClazz,
    		final String secondNumber, final String secondClazz) {
    	return (firstNumber.equals(secondNumber)
    			&& firstClazz.equals(secondClazz));
    }

    /**
     * gets the selected section in the tree.
     *
     * @return Section
     */
    public static List<OWLNamedIndividual> getOntologySelectedRule() {
        List<OWLNamedIndividual> individuals = new ArrayList<OWLNamedIndividual>();
        TreeItem[] array = RuleTree.getSelection();
        for (TreeItem item : array) {
        	individuals.add(controller.getIndividual(
            		(String) item.getData(Constants.RULE_NUMBER),
            		(String) item.getData(Constants.RULE_CLAZZ)));
        }
        return individuals;
    }

    /**
     * searches the tree for entered SearchString.
     * based on the work of George Kyriacou, Reliability Test Engineer, IBM in
     * the article on
     * http://www.ibm.com/developerworks/lotus/library/expeditor-swt/
     */
    public static void search(final TreeItem[] rootElement) {
    	if (rootElement != null && rootElement.length > 0) {
	        // selects SearchResult-tab
	        tabContainer.setSelection(2);
	        String searchString = showSearchPopup();
	
	        // cancel search if searchString is blank
	        if ((searchString == null) || (searchString.trim().isEmpty())) {
	            return;
	        }
	
	        // deletes existing tableItems in search table
	        removeTable();
	
	        // searches with entered SearchString in the Tree
	        searchStringInGivenTreeItems(searchString, rootElement);
    	} else {
    		controller.makeMessageBox(mainComposite.getShell(),
    				"Please choose a regulation.");
    	}
    }

    /**
     * creates Search-dialog.
     *
     * @return String
     */
    private static String showSearchPopup() {

        InputDialog d = new InputDialog(RuleTree.getParent().getShell(), "Suche",
                "Text suchen", "", null);
        d.open();
        return d.getValue();
    }

    /** The detected. */
    private static boolean detected;
    
    /** The content text. */
    private static String contentText;

    /**
     * searches recursively in the Tree for any hierarchy-levels.
     *
     * @param searchString the search string
     * @param treeItems the tree items
     */
    public static void searchStringInGivenTreeItems(String searchString, TreeItem[] treeItems) {
        detected = false;
        searchString(searchString, treeItems);
        // Message-dialog appears if search finishes
        controller.makeMessageBox(mainComposite.getShell(), "Search finished");
    }

    /**
     * findString-HelpMethod for recursive calls.
     *
     * @param searchString the search string
     * @param treeItems the tree items
     */
    private static void searchString(String searchString, TreeItem[] treeItems) {
        for (TreeItem treeItem : treeItems) {
            // // checks if treeItem is a section
            // if (treeItem.getText().contains("Abs.")) {
            if (treeItem.getParentItem() != null) {
                if (treeItem.getParentItem().getParentItem() != null) {

                    // checks if item is found and search is aborted
                    if (detected == false) {

                        // gets section content
                        contentText = controller.getTextFromIndividual(
                        		(String) treeItem.getData(Constants.RULE_NUMBER),
                        		(String) treeItem.getData(Constants.RULE_CLAZZ));

                        // checks if section content is blank
                        if ((contentText != " ") && (contentText != "")) {

                            // if ((text.toUpperCase()
                            // .contains(searchString.toUpperCase()))) {
                            if ((contentText.contains(searchString))) {

                                // if section contains SearchString, create new
                                // SearchResult-tableItem
                                setTable(treeItem.getParentItem()
                                        .getParentItem().getText(),
                                        treeItem.getText(), searchString,
                                        (String) treeItem.getData(Constants.RULE_NUMBER),
                                        (String) treeItem.getData(Constants.RULE_CLAZZ));

                                // // checks after every occurrence if you want
                                // to
                                // // abort search
                                // if (MessageDialog.openQuestion(
                                // tree1.getShell(), "Element gefunden",
                                // "Suche beenden?")) {
                                //
                                // detected = true;
                                // }
                            }
                        }
                    }
                }
            }
            // recursive call for childItems
            searchString(searchString, treeItem.getItems());
        }
    }

    /**
     * Gets the text.
     *
     * @return the text
     */
    public static StyledText getText() {
        return text;
    }

    /**
     * returns tree.
     *
     * @return Tree
     */
    public static Tree getTree() {
        return RuleTree;
    }

    /**
     * returns tree for situations.
     *
     * @return Tree
     */
    public static Tree getTreeForSituations() {
        return situationsTree;
    }

    /**
     * creates new SearchResult-tableItem.
     *
     * @param rule the law
     * @param regulation the section
     * @param searchWord the search word
     */
    public static void setTable(final String rule, final String regulation,
    		final String searchWord, final String number, final String clazz) {
        TableItem item = new TableItem(searchTable, SWT.NONE);
        item.setData(Constants.RULE_NUMBER, number);
        item.setData(Constants.RULE_CLAZZ, clazz);
        item.setText(new String[] {regulation, rule, searchWord });	
    }

    /**
     * removes all items in SearchResult-table.
     */
    public static void removeTable() {
        if (searchTable.getItemCount() > 0) {
            for (TableItem item : searchTable.getItems()) {
                item.dispose();
            }
        }
    }

    /**
     * adds new ruleElement to correspondent list of ruleElementTypes in the
     * Configuration-Tab.
     *
     * @param text the text
     * @param combo the combo
     */
    public final void addREinConfiguration(final String text,
    		final String combo) {
        for (int i = 0; i < ConfigurationView.getTree().getItems().length; i++) {

            // checks at which index (of the tree) the item (of the
            // combobox) appears
            if (ConfigurationView.getTree().getItem(i).getText().equals(combo)) {

                // checks if the element already exists
                for (TreeItem it : ConfigurationView.getTree().getItem(i)
                        .getItems()) {
                    if (it.getText().equals(text)) {
//                        System.out.println("Element bereits vorhanden!");
                        return;
                    }

                }

                // checks if the name of the element is blank or not
                // renamed
                if (text.trim().isEmpty() | text.equals(Constants.ENTER_NAME)) {
//                    System.out.println("Bitte einen Namen eingeben!");
                    return;
                }

                // creates new item with matching color
                TreeItem item = new TreeItem(
                        ConfigurationView.getTree().getItem(i), SWT.NONE);

                item.setForeground(ConfigurationView.getLabelList()[i]
                        .getBackground());
                item.setText(text);

                // deletes the items in the list and adds the
                // remaining ones
                dataController.clearRuleElements();

                for (int j = 0; j < ConfigurationView.getTree().getItems().length; j++) {

                    for (TreeItem item1 : ConfigurationView.getTree().getItem(j)
                            .getItems()) {

                        dataController.addRuleElement(item1.getText(), j);
                    }
                }
                // synchronizes the items
                updateEntriesOfRuleElementComboBox();
            }
        }
    }

    /**
     * gets the initialized OntologyController.
     *
     * @return OntologyController
     */
    public static MainController getOntologyController() {
        return controller;
    }

    /**
     * sets the highlights of the ruleElementInstances in the text for the
     * selected section.
     *
     * @param ruleElementList the new highlights
     */
    public void setHighlights(List<RuleElementModel> ruleElementList) {

        // defining StyleRange for coloring text
        StyleRange style = new StyleRange();
        int start = 0;
        int end = 0;
        String type = "";
        
        for (RuleElementModel ruleElement : ruleElementList) {
            start = ruleElement.getStartIndex();
            end = ruleElement.getEndIndex();
            type = ruleElement.getType();
//            System.out.println(type);

            // gets index of RuleElementType
//            int ruleElementIndex = RULEELEMENTS.valueOf(type).getValue();

            if (start != -1 && end != -1) {
	            style.start = start;
	            style.length = end - start;
	
	            // disposes old color
	            if (color != null) {
	                color.dispose();
	            }
	
	            color = new Color(RuleTree.getParent().getDisplay(),
	                    dataController.getColor(RULEELEMENTS.valueOf(type)));
	//            System.out.println(color.getRGB());
	
	            style.foreground = color;
	            text.setStyleRange(style);
	
	            // create new StyleRange and modify Color as work-around for bug!
	            style = new StyleRange();
	            color = null;
            } else {
            	System.err.println("The indices of ruleelement " + ruleElement.getName()
            			+ " were corrupt. (" + start + ", " + end + ")");
            }

        }
    }

    /**
     * fills the Situations Tree with the existing situations, related constraints and ruleelements.
     *
     * @param rule the new situations into situations tree
     */
    private static void fillSituationTree(final String rule) {

        situationsTree.removeAll();	// remove the old situations
        
        // creates items for the situations, constraints and ruleelements
        for (OWLNamedIndividual s : controller.loadSituationsforSpecificRule(rule)) {
            TreeItem situation = new TreeItem(situationsTree, SWT.NONE);
            situation.setText(controller.getAnnotationFromIndividual(
            		s, RegulatoryOntologyHelper.PROP_SITUATION_NAME));
            TreeItem constraintTreeElement = new TreeItem(situation, SWT.NONE);
            constraintTreeElement.setText("Constraint");
            TreeItem ruleElementTreeElement = new TreeItem(situation, SWT.NONE);
            ruleElementTreeElement.setText("Ruleelement");
            for (OWLNamedIndividual con : controller.getRelatedIndividuals(
            		s, RegulatoryOntologyHelper.REL_SITUATION_CHECKED_BY)) {
            	TreeItem constraintItem = new TreeItem(constraintTreeElement, SWT.NONE);
            	constraintItem.setText(con.getIRI().getFragment());
            	for (OWLNamedIndividual re : controller.getRelatedIndividuals(
            			con, RegulatoryOntologyHelper.REL_CONSTRAINT_HASPARAMETERS)) {
            		TreeItem ruleElementItem = new TreeItem(ruleElementTreeElement, SWT.NONE);
            		ruleElementItem.setText(re.getIRI().getFragment() + " (" + con.getIRI().getFragment() + ")");
            	}
            }
        }
    }

    /**
     * fills the relation table with the ruleelements, which have been given in the list.
     * For relations between ruleelements in this list, the concerning target ruleelement 
     * is computed and the concerning relation is added.
     *
     * @param ruleElementList the rule element list
     */
    public static void fillTheRelationsTable(
    		final List<RuleElementModel> ruleElementList) {
    	relationTable.removeAll();
    	try {
	    	for (RuleElementModel sourceRE : ruleElementList) {
	    		OWLNamedIndividual sourceREIndividual = controller.getIndividual(
	    				sourceRE.getName().replace(" ", "_"), sourceRE.getType());
	    		Set<OWLNamedIndividual> targetREIndividuals = controller
	    				.getRelatedIndividuals(sourceREIndividual,
	    						RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
	    		if (targetREIndividuals != null) {
		    		for (OWLNamedIndividual target : targetREIndividuals) {
		    			for (RuleElementModel targetRE : ruleElementList) {
		    				if (target.getIRI().getFragment().equals(targetRE.getName().replace(" ", "_"))) {
		    					createNewTableEntryForRelationTable(sourceRE, targetRE);
		    				}
		    			}
		    		}
	    		}
	    	}
    	} catch (WrongOrderException w) {
    		System.err.println("Failed to fill the relation table. Cause: " + w.getLocalizedMessage());
    	}
    }

    /**
     * creates a new table entry for the relation table.
     * @param sourceRE
     * @param targetRE
     * @throws WrongOrderException
     */
    private static void createNewTableEntryForRelationTable(final RuleElementModel sourceRE, final RuleElementModel targetRE) throws WrongOrderException {
    	String sourceRuleElement = sourceRE.getName() + " (" + sourceRE.getType() + ")";
		String relation = controller.getRuleElementRelation(sourceRE.getType(), targetRE.getType());
		String targetRuleElement = targetRE.getName() + " (" + targetRE.getType() + ")";
		TableItem[] actualItems = relationTable.getItems();
		TableItem ruleelement = new TableItem(relationTable, SWT.NONE);
		ruleelement.setText(new String[] {sourceRuleElement, relation, targetRuleElement});
		for (int i = 0; i < actualItems.length; i++) {
			if (actualItems[i].getText().equals(ruleelement.getText())) {
				relationTable.remove(i);	// if two of the same relations resist in the entry,
				break;						// the the old one should be deleted.
			}
		}
		
	}

	/**
     * returns relationsTable.
     *
     * @return Table
     */
    public static Table getTableForRelations() {
        return relationTable;
    }

    /**
     * sets the lawList for loading the ontology into the tree.
     *
     * @param r the new rule list
     */
    public static void setRuleList(List<Rule> r) {
        rules = r;
        fillTheRuleTree();
    }

	public static void createNewConstraint() {
		new NewConstraintPopupMenu(new Shell(mainComposite.getShell().getDisplay()));
	}

}
