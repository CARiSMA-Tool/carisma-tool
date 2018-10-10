package carisma.xutils.regulatory.ui.examples;

import java.util.Random;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

// TODO: Auto-generated Javadoc
/**
 * The Class ExpandAll.
 */
public class ExpandAll {
    
    /** The tree. */
    private static Tree tree = null;
    
    /**
     * The main method.
     *
     * @param args the arguments
     */
    public static void main(String[] args) {
        Display display = new Display();

        Shell shell = new Shell(display);
        shell.setText("Expand All Items");
        GridLayout gridLayout = new GridLayout();
        gridLayout.numColumns = 2;
        shell.setLayout(gridLayout);
        shell.setSize(400, 300);

        Button expButton = new Button(shell, SWT.PUSH);
        expButton.setText("+");
        GridData gridData = new GridData(23, 23);
        expButton.setLayoutData(gridData);
        expButton.addSelectionListener(new ExpandAllItemsListener(true));
       
        Button colButton = new Button(shell, SWT.PUSH);
        colButton.setText("-");
        gridData = new GridData(23, 23);
        colButton.setLayoutData(gridData);
        colButton.addSelectionListener(new ExpandAllItemsListener(false));

        tree = new Tree(shell, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        tree.setHeaderVisible(true);

        gridData = new GridData(GridData.FILL_BOTH);
        gridData.horizontalSpan = 2;
        tree.setLayoutData(gridData);
        
        TreeColumn column1 = new TreeColumn(tree, SWT.NONE);
        column1.setText("TreeColumn0");
        column1.setWidth(200);
        column1.setAlignment(SWT.LEFT);
       column1.addSelectionListener(new SortTreeListener());

        TreeColumn column2 = new TreeColumn(tree, SWT.NONE);
        column2.setText("TreeColumn1");
        column2.setWidth(200);
        column2.setAlignment(SWT.CENTER);
        column2.addSelectionListener(new SortTreeListener());

        Random generator = new Random();

       for (int i = 0; i < 5; i++) {
            TreeItem treeItem = new TreeItem(tree, 0);
            treeItem.setText(new String[] { "TreeItem" + i,
                    Integer.toString(generator.nextInt()) });
            for (int j = 0; j < 5; j++) {
                TreeItem subTreeItem = new TreeItem(treeItem, SWT.NONE);
                subTreeItem.setText(new String[] { "SubTreeItem" + j,
                        Integer.toString(generator.nextInt()) });
                for (int k = 0; k < 5; k++) {
                    TreeItem subSubTreeItem = new TreeItem(subTreeItem,
                            SWT.NONE);
                    subSubTreeItem.setText(new String[] { "SubSubTreeItem" + k,
                            Integer.toString(generator.nextInt()) });
                }
            }
        }

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }
        display.dispose();
    }
    
    /**
     * The listener interface for receiving expandAllItems events.
     * The class that is interested in processing a expandAllItems
     * event implements this interface, and the object created
     * with that class is registered with a component using the
     * component's <code>addExpandAllItemsListener<code> method. When
     * the expandAllItems event occurs, that object's appropriate
     * method is invoked.
     *
     * @see ExpandAllItemsEvent
     */
    static class ExpandAllItemsListener implements SelectionListener {
        
        /** The expand. */
        private boolean expand = false;

        /**
         * Instantiates a new expand all items listener.
         *
         * @param expand the expand
         */
        public ExpandAllItemsListener(Boolean expand) {
            this.expand = expand;
        }

        /* (non-Javadoc)
         * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
         */
        @Override
        public void widgetDefaultSelected(SelectionEvent e) {
            expandTreeItems();
       }

        /* (non-Javadoc)
         * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        @Override
       public void widgetSelected(SelectionEvent e) {
            expandTreeItems();
        }
        
        /**
         * Expand tree items.
         */
        public void expandTreeItems() {
           TreeItem[] treeItems = tree.getItems();
            if (treeItems != null) {
               for (TreeItem treeItem : treeItems) {
                    treeItem.setExpanded(expand);
                }
            }
        }
    }
}
