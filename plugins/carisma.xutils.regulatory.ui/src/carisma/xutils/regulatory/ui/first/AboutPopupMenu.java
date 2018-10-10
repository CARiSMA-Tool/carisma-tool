package carisma.xutils.regulatory.ui.first;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

// TODO: Auto-generated Javadoc
/**
 * About-dialog class for the GUI.
 *
 * @author bm
 */
public class AboutPopupMenu {

    /**
     * creates a new About-dialog.
     *
     * @param parent the parent
     */
    public AboutPopupMenu(Shell parent) {
        init(parent);
    }

    /**
     * creates the dialog content.
     *
     * @param shell the shell
     */
    public static void init(final Shell shell) {

        GridLayout gridLayout = new GridLayout(1, false);
        shell.setLayout(gridLayout);
        shell.setText("About the GUI");

        Composite parent = new Composite(shell, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, false, false);
        parent.setLayoutData(gridData);
        gridLayout = new GridLayout(1, false);
        parent.setLayout(gridLayout);

        Label about = new Label(parent, SWT.WRAP);
        about.setText("Entwicklung eines interaktiven Werkzeugs zur Extraktion von Sachverhalten aus Regularien\n\nv0.1\n\n© 2012 by Babak Mansour\n\nTU Dortmund\nLehrstuhl 14\nSoftware Engineering");
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.widthHint = 200;
        about.setLayoutData(gridData);

        Button btn_ok = new Button(parent, SWT.PUSH);
        btn_ok.setText("OK");
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.widthHint = 200;
        // data.horizontalSpan = 2;
        btn_ok.setLayoutData(gridData);

        // creates SelectionListener for closing the window
        btn_ok.addSelectionListener(new SelectionListener() {
            public void widgetSelected(SelectionEvent e) {
                shell.close();
                shell.dispose();
            }

            public void widgetDefaultSelected(SelectionEvent e) {
            }
        });

        shell.open();
        shell.pack();
        // shell.setSize(200, 400);

    }
}
