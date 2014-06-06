package carisma.xutils.regulatory.ui.examples;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

// TODO: Auto-generated Javadoc
/*
 * Exclude a widget from a GridLayout
 * 
 * For a list of all SWT example snippets see
 * http://dev.eclipse.org/viewcvs/index.cgi/%7Echeckout%7E/platform-swt-home/dev.html#snippets
 */

/**
 * The Class Snippet175.
 */
public class Snippet175 {

    /**
     * The main method.
     *
     * @param args the arguments
     */
    public static void main(String[] args) {

        Display display = new Display();
        final Shell shell = new Shell(display);
        shell.setLayout(new GridLayout(3, false));

        Button b = new Button(shell, SWT.PUSH);
        b.setText("Button 0");

        final Button bHidden = new Button(shell, SWT.PUSH);
        bHidden.setText("Button 1");
        GridData data = new GridData();
        data.exclude = true;
        data.horizontalSpan = 2;
        data.horizontalAlignment = SWT.FILL;
        bHidden.setLayoutData(data);

        b = new Button(shell, SWT.PUSH);
        b.setText("Button 2");
        b = new Button(shell, SWT.PUSH);
        b.setText("Button 3");
        b = new Button(shell, SWT.PUSH);
        b.setText("Button 4");

        b = new Button(shell, SWT.CHECK);
        b.setText("hide");
        b.setSelection(true);
        b.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event e) {
                Button b = (Button) e.widget;
                GridData data = (GridData) bHidden.getLayoutData();
                data.exclude = b.getSelection();
                bHidden.setVisible(!data.exclude);
                shell.layout(false);
            }
        });
        shell.setSize(400, 400);
        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }
        display.dispose();
    }
}
