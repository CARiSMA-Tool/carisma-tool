package carisma.xutils.regulatory.ui.examples;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

// TODO: Auto-generated Javadoc
/**
 * The Class Snippet129.
 */
public class Snippet129 {

	/**
	 * The main method.
	 *
	 * @param args the arguments
	 */
	public static void main(String[] args) {
		Display display = new Display();
		Color red = display.getSystemColor(SWT.COLOR_RED);
		Color blue = display.getSystemColor(SWT.COLOR_BLUE);
		Color white = display.getSystemColor(SWT.COLOR_WHITE);
		Color gray = display.getSystemColor(SWT.COLOR_GRAY);
		Shell shell = new Shell(display);
		shell.setLayout(new FillLayout());
		Table table = new Table(shell, SWT.BORDER);
		table.setBackground(gray);
		TableColumn column1 = new TableColumn(table, SWT.NONE);
		TableColumn column2 = new TableColumn(table, SWT.NONE);
		TableColumn column3 = new TableColumn(table, SWT.NONE);
		TableItem item = new TableItem(table, SWT.NONE);
		item.setText(new String[] { "entire", "row", "red foreground" });
		item.setForeground(red);
		item = new TableItem(table, SWT.NONE);
		item.setText(new String[] { "entire", "row", "red background" });
		item.setBackground(red);
		item = new TableItem(table, SWT.NONE);
		item.setText(new String[] { "entire", "row", "white fore/red back" });
		item.setForeground(white);
		item.setBackground(red);
		item = new TableItem(table, SWT.NONE);
		item.setText(new String[] { "normal", "blue foreground",
				"red foreground" });
		item.setForeground(1, blue);
		item.setForeground(2, red);
		item = new TableItem(table, SWT.NONE);
		item.setText(new String[] { "normal", "blue background",
				"red background" });
		item.setBackground(1, blue);
		item.setBackground(2, red);
		item = new TableItem(table, SWT.NONE);
		item.setText(new String[] { "white fore/blue back", "normal",
				"white fore/red back" });
		item.setForeground(0, white);
		item.setBackground(0, blue);
		item.setForeground(2, white);
		item.setBackground(2, red);

		column1.pack();
		column2.pack();
		column3.pack();

		shell.pack();
		shell.open();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}
}
