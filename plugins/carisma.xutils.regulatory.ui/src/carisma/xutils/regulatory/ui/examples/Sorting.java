package carisma.xutils.regulatory.ui.examples;

import java.util.Random;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

// TODO: Auto-generated Javadoc
/**
 * The Class Sorting.
 */
public class Sorting {
    
    /**
     * The main method.
     *
     * @param args the arguments
     */
    public static void main(String[] args) {
        Display display = new Display();

        Shell shell = new Shell(display);
        shell.setText("Sorting Trees");
        shell.setLayout(new FillLayout());
        shell.setSize(400, 300);

        Tree tree = new Tree(shell, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        tree.setHeaderVisible(true);

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
}
