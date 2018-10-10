package carisma.xutils.regulatory.ui.examples;

import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;

// TODO: Auto-generated Javadoc
/**
 * The Class ChildShellExample2.
 */
public class ChildShellExample2 {
	
	/** The d. */
	Display d = new Display();

	/**
	 * Instantiates a new child shell example2.
	 */
	ChildShellExample2() {
		Shell s = new Shell(d);
		s.setSize(500, 500);
		s.open();
		ChildShell cs1 = new ChildShell(s);

		Monitor primary = d.getPrimaryMonitor();
		Rectangle bounds = primary.getBounds();
		Rectangle rect = s.getBounds();
		int x = bounds.x + (bounds.width - rect.width) / 2;
		int y = bounds.y + (bounds.height - rect.height) / 2;
		s.setLocation(x, y);

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
		new ChildShellExample2();
	}
}

class ChildShell {

	ChildShell(Shell parent) {
		Shell child = new Shell(parent);
		child.setSize(200, 200);
		// System.out.println(child.getBounds());
		// child.setBounds(0, 0, 200, 500);
		child.open();
	}
}
