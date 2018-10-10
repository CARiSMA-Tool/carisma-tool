package carisma.xutils.regulatory.ui.examples;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

// TODO: Auto-generated Javadoc
/**
 * The Class PopupMenuCreate.
 */
public class PopupMenuCreate {
    
    /**
     * The main method.
     *
     * @param args the arguments
     */
    public static void main(String[] args) {
        Display display = new Display();
        Shell shell = new Shell(display);
        shell.setLayout(new GridLayout());

        Button bn = new Button(shell, SWT.FLAT);
        bn.setText("Right Click to see the popup menu");

        Menu popupMenu = new Menu(bn);
        MenuItem newItem = new MenuItem(popupMenu, SWT.CASCADE);
        newItem.setText("New");
        MenuItem refreshItem = new MenuItem(popupMenu, SWT.NONE);
        refreshItem.setText("Refresh");
        MenuItem deleteItem = new MenuItem(popupMenu, SWT.NONE);
        deleteItem.setText("Delete");

        Menu newMenu = new Menu(popupMenu);
        newItem.setMenu(newMenu);

        MenuItem shortcutItem = new MenuItem(newMenu, SWT.NONE);
        shortcutItem.setText("Shortcut");
        MenuItem iconItem = new MenuItem(newMenu, SWT.NONE);
        iconItem.setText("Icon");

        bn.setMenu(popupMenu);

        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }
    }
}