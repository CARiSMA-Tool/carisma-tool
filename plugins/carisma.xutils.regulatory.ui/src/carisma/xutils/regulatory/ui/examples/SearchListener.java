package carisma.xutils.regulatory.ui.examples;

//import org.eclipse.jface.dialogs.InputDialog;
//import org.eclipse.jface.dialogs.MessageDialog;
//import org.eclipse.swt.events.SelectionEvent;
//import org.eclipse.swt.events.SelectionListener;
//import org.eclipse.swt.widgets.Tree;
//import org.eclipse.swt.widgets.TreeItem;
//
//public class SearchListener implements SelectionListener {
//
//    // private Button button = null;
//    private static Regularien regularien;
//    private static Tree tree = regularien.getTree();
//
//    @Override
//    public void widgetSelected(SelectionEvent e) {
//        // button = (Button) e.widget;
//
//        search();
//    }
//
//    @Override
//    public void widgetDefaultSelected(SelectionEvent arg0) {
//        // TODO Auto-generated method stub
//
//    }
//
//    public static void search() {
//
//        String searchString = showSearchPopup();
//        if ((searchString == null) || (searchString == "")) {
//            return;
//        }
//
//        regularien.removeTable();
//        findString(searchString, tree.getItems());
//    }
//
//    private static String showSearchPopup() {
//        // InputDialog d = new InputDialog(this.tree.getParent().getShell(),
//        // "Search", "Search text", "", null);
//        InputDialog d = new InputDialog(tree.getParent().getShell(), "Suche",
//                "Text suchen", "", null);
//        d.open();
//        return d.getValue();
//    }
//
//    // findString-Methode rekursiv für beliebige Ebenen
//
//    private static boolean gefunden;
//    private static String text;
//
//    private static void findString(String searchString, TreeItem[] treeItems) {
//        gefunden = false;
//        findString2(searchString, treeItems);
//    }
//
//    // findString-Hilfsmethode für rekursive Aufrufe
//
//    private static void findString2(String searchString, TreeItem[] treeItems) {
//        for (TreeItem treeItem : treeItems) {
//            System.out.println(treeItem.getText());
//            if (treeItem.getText().contains("Abs.")) {
//
//                if (gefunden == false) {
//                    // System.out.println("Der Name lautet " +
//                    // treeItem.getText());
//
//                    text = regularien.getOntologyContentSearch(treeItem);
//                    // Falls externe Methode nicht static ist, wird
//                    // NullPointerException
//                    // geworfen
//                    // String text = treeItem.getText();
//                    // String text = "Rechtsfähigkeit ghfhgf htdtgd hfzhf fhf";
//
//                    if ((text != " ") && (text != "")) {
//
//                        // if ((text.toUpperCase()
//                        // .contains(searchString.toUpperCase()))) {
//                        if ((text.contains(searchString))) {
//                            // tree.setSelection(treeItem);
//                            regularien.setTable(treeItem.getParentItem()
//                                    .getParentItem().getText(), treeItem
//                                    .getParentItem().getText(), treeItem
//                                    .getText(), searchString);
//                            if (MessageDialog.openQuestion(tree.getShell(),
//                                    "Element gefunden", "Suche beenden?")) {
//                                gefunden = true;
//                            }
//                        }
//                    }
//                    // if (!(treeItem.getItems() == null))
//                    // findString2(searchString, treeItem.getItems());
//
//                    // }
//
//                }
//            }
//            findString2(searchString, treeItem.getItems());
//        }
//    }
//}

import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

// TODO: Auto-generated Javadoc
/**
 * The listener interface for receiving search events.
 * The class that is interested in processing a search
 * event implements this interface, and the object created
 * with that class is registered with a component using the
 * component's <code>addSearchListener<code> method. When
 * the search event occurs, that object's appropriate
 * method is invoked.
 *
 * @see SearchEvent
 */
public class SearchListener implements KeyListener {
    
    /** The tree. */
    private Tree tree = null;

    /* (non-Javadoc)
     * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
     */
    @Override
    public void keyPressed(KeyEvent e) {
        tree = (Tree) e.widget;

        String searchString = showSearchPopup();
        if (searchString == null) {
            return;
        }

        findString(searchString, tree.getItems());

        // if (!findString(searchString, tree.getItems())) {
        // MessageBox messageBox = new MessageBox(tree.getShell(), SWT.OK |
        // SWT.ICON_ERROR);
        // messageBox.setMessage("Could not find: '" + searchString + "'");
        // messageBox.setText("Search Error");
        // messageBox.open();
        // }

    }

    /* (non-Javadoc)
     * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
     */
    @Override
    public void keyReleased(KeyEvent e) {

    }

    /**
     * Show search popup.
     *
     * @return the string
     */
    private String showSearchPopup() {
        InputDialog d = new InputDialog(this.tree.getParent().getShell(),
                "Search", "Search text", "", null);
        d.open();
        return d.getValue();
    }

    // findString-Methode iterativ für 3 Ebenen

    // private void findString(String searchString, TreeItem[] treeItems) {
    // for (TreeItem treeItem : treeItems) {
    // String text = treeItem.getText();
    // if ((text.toUpperCase().contains(searchString.toUpperCase()))) {
    // tree.setSelection(treeItem);
    // if (MessageDialog.openQuestion(tree.getShell(),
    // "Element gefunden", "Richtiges Element?"))
    // return;
    // }
    //
    // for (TreeItem treeItem2 : treeItem.getItems()) {
    // text = treeItem2.getText();
    // if ((text.toUpperCase().contains(searchString.toUpperCase()))) {
    // tree.setSelection(treeItem2);
    // if (MessageDialog.openQuestion(tree.getShell(),
    // "Element gefunden", "Richtiges Element?"))
    // return;
    // }
    // for (TreeItem treeItem3 : treeItem2.getItems()) {
    // text = treeItem3.getText();
    // if ((text.toUpperCase()
    // .contains(searchString.toUpperCase()))) {
    // tree.setSelection(treeItem3);
    // if (MessageDialog.openQuestion(tree.getShell(),
    // "Element gefunden", "Richtiges Element?"))
    // return;
    // }
    // }
    // }
    // }
    // }

    // findString-Methode rekursiv für beliebige Ebenen

    /** The gefunden. */
    private boolean gefunden;

    /**
     * Find string.
     *
     * @param searchString the search string
     * @param treeItems the tree items
     */
    private void findString(String searchString, TreeItem[] treeItems) {
        gefunden = false;
        findString2(searchString, treeItems);
    }

    // findString-Hilfsmethode für rekursiv Aufrufe

    /**
     * Find string2.
     *
     * @param searchString the search string
     * @param treeItems the tree items
     */
    private void findString2(String searchString, TreeItem[] treeItems) {
        for (TreeItem treeItem : treeItems) {
            if (gefunden == false) {

                String text = treeItem.getText();
                if ((text.toUpperCase().contains(searchString.toUpperCase()))) {
                    tree.setSelection(treeItem);
                    if (MessageDialog.openQuestion(tree.getShell(),
                            "Element gefunden", "Richtiges Element?"))
                        gefunden = true;
                }
                if (!(treeItem.getItems() == null))
                    findString2(searchString, treeItem.getItems());

            }
        }
        return;
    }

}
