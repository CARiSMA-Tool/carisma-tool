package carisma.xutils.regulatory.ui.examples;


import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

// TODO: Auto-generated Javadoc
/**
 * The Class ColorDialogExample.
 */
public class ColorDialogExample {

  /** The d. */
  Display d;

  /** The s. */
  Shell s;

  /**
   * Instantiates a new color dialog example.
   */
  ColorDialogExample() {
    d = new Display();
    s = new Shell(d);
    s.setSize(400, 400);
    
    s.setText("A ColorDialog Example");
    s.setLayout(new FillLayout(SWT.VERTICAL));
    final Text t = new Text(s, SWT.BORDER | SWT.MULTI);
    final Button b = new Button(s, SWT.PUSH | SWT.BORDER);
    b.setText("Change Color");
    b.addSelectionListener(new SelectionAdapter() {
      public void widgetSelected(SelectionEvent e) {
        ColorDialog cd = new ColorDialog(s);
        cd.setText("ColorDialog Demo");
        cd.setRGB(new RGB(255, 255, 255));
        RGB newColor = cd.open();
        if (newColor == null) {
          return;
        }
        t.setBackground(new Color(d, newColor));
      }
    });
    s.open();

    while (!s.isDisposed()) {
      if (!d.readAndDispatch())
        d.sleep();
    }
    d.dispose();
  }

  /**
   * The main method.
   *
   * @param argv the arguments
   */
  public static void main(String[] argv) {
    new ColorDialogExample();
  }
}
