package carisma.xutils.regulatory.ui.examples;

import java.util.Random;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

// TODO: Auto-generated Javadoc
/**
 * The Class Searching.
 */
public class Searching {
    
    /**
     * The main method.
     *
     * @param args the arguments
     */
    public static void main(String[] args) {
        Display display = new Display();

        Shell shell = new Shell(display);
        shell.setText("Searching");
        shell.setLayout(new FillLayout());
        shell.setSize(400, 600);

        final Tree tree = new Tree(shell, SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.FULL_SELECTION);
        tree.setHeaderVisible(true);
        tree.setLinesVisible(true);
        tree.addKeyListener(new SearchListener());

        // PopupMenü im Tree um Baum ausblenden zu können

        Menu popupMenu = new Menu(tree);
        MenuItem newItem = new MenuItem(popupMenu, SWT.CASCADE);
        newItem.setText("Baum ausblenden");
        tree.setMenu(popupMenu);

        // Listener für das Popup-Menü im Tree

        newItem.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent e) {
                System.out.println("Baum aus!");
                tree.setVisible(false);
            }

            public void widgetDefaultSelected(SelectionEvent e) {
            }

        });

        // PopupMenü in der Shell um Baum einblenden zu können

        Menu popupMenu2 = new Menu(tree);
        MenuItem newItem2 = new MenuItem(popupMenu2, SWT.CASCADE);
        newItem2.setText("Baum einblenden");
        shell.setMenu(popupMenu2);

        // Listener für das Popup-Menü in der Shell

        newItem2.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent e) {
                System.out.println("Baum ein!");
                tree.setVisible(true);
                // tree.set
            }

            public void widgetDefaultSelected(SelectionEvent e) {
            }

        });

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
