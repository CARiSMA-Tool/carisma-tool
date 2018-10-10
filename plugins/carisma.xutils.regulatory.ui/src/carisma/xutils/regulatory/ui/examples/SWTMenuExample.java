package carisma.xutils.regulatory.ui.examples;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

// TODO: Auto-generated Javadoc
/**
 * The Class SWTMenuExample.
 */
public class SWTMenuExample {

  /** The display. */
  Display display;

  /** The shell. */
  Shell shell;

  /** The help menu. */
  Menu menuBar, fileMenu, helpMenu;

  /** The help menu header. */
  MenuItem fileMenuHeader, helpMenuHeader;

  /** The help get help item. */
  MenuItem fileExitItem, fileSaveItem, helpGetHelpItem;

  /** The label. */
  Label label;

  /**
   * Instantiates a new sWT menu example.
   */
  public SWTMenuExample() {

    display = new Display();
    shell = new Shell(display);
    shell.setText("Menu Example");
    shell.setSize(300, 200);

    label = new Label(shell, SWT.CENTER);
    label.setBounds(shell.getClientArea());

    menuBar = new Menu(shell, SWT.BAR);
    fileMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
    fileMenuHeader.setText("&File");

    fileMenu = new Menu(shell, SWT.DROP_DOWN);
    fileMenuHeader.setMenu(fileMenu);

    fileSaveItem = new MenuItem(fileMenu, SWT.PUSH);
    fileSaveItem.setText("&Save");

    fileExitItem = new MenuItem(fileMenu, SWT.PUSH);
    fileExitItem.setText("E&xit");

    helpMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
    helpMenuHeader.setText("&Help");

    helpMenu = new Menu(shell, SWT.DROP_DOWN);
    helpMenuHeader.setMenu(helpMenu);

    helpGetHelpItem = new MenuItem(helpMenu, SWT.PUSH);
    helpGetHelpItem.setText("&Get Help");

    fileExitItem.addSelectionListener(new fileExitItemListener());
    fileSaveItem.addSelectionListener(new fileSaveItemListener());
    helpGetHelpItem.addSelectionListener(new helpGetHelpItemListener());

    shell.setMenuBar(menuBar);
    shell.open();
    while (!shell.isDisposed()) {
      if (!display.readAndDispatch())
        display.sleep();
    }
    display.dispose();
  }

  /**
   * The listener interface for receiving fileExitItem events.
   * The class that is interested in processing a fileExitItem
   * event implements this interface, and the object created
   * with that class is registered with a component using the
   * component's <code>addfileExitItemListener<code> method. When
   * the fileExitItem event occurs, that object's appropriate
   * method is invoked.
   *
   * @see fileExitItemEvent
   */
  class fileExitItemListener implements SelectionListener {
    
    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetSelected(SelectionEvent event) {
      shell.close();
      display.dispose();
    }

    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetDefaultSelected(SelectionEvent event) {
      shell.close();
      display.dispose();
    }
  }

  /**
   * The listener interface for receiving fileSaveItem events.
   * The class that is interested in processing a fileSaveItem
   * event implements this interface, and the object created
   * with that class is registered with a component using the
   * component's <code>addfileSaveItemListener<code> method. When
   * the fileSaveItem event occurs, that object's appropriate
   * method is invoked.
   *
   * @see fileSaveItemEvent
   */
  class fileSaveItemListener implements SelectionListener {
    
    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetSelected(SelectionEvent event) {
      label.setText("Saved");
    }

    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetDefaultSelected(SelectionEvent event) {
      label.setText("Saved");
    }
  }

  /**
   * The listener interface for receiving helpGetHelpItem events.
   * The class that is interested in processing a helpGetHelpItem
   * event implements this interface, and the object created
   * with that class is registered with a component using the
   * component's <code>addhelpGetHelpItemListener<code> method. When
   * the helpGetHelpItem event occurs, that object's appropriate
   * method is invoked.
   *
   * @see helpGetHelpItemEvent
   */
  class helpGetHelpItemListener implements SelectionListener {
    
    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetSelected(SelectionEvent event) {
      label.setText("No worries!");
    }

    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetDefaultSelected(SelectionEvent event) {
      label.setText("No worries!");
    }
  }

  /**
   * The main method.
   *
   * @param args the arguments
   */
  public static void main(String[] args) {
    SWTMenuExample menuExample = new SWTMenuExample();
  }
}
