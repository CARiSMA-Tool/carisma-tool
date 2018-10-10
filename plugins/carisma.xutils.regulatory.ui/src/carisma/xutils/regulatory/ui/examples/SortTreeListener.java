package carisma.xutils.regulatory.ui.examples;

import java.text.Collator;
import java.util.Locale;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

// TODO: Auto-generated Javadoc
/**
 * The listener interface for receiving sortTree events.
 * The class that is interested in processing a sortTree
 * event implements this interface, and the object created
 * with that class is registered with a component using the
 * component's <code>addSortTreeListener<code> method. When
 * the sortTree event occurs, that object's appropriate
 * method is invoked.
 *
 * @see SortTreeEvent
 */
public class SortTreeListener implements SelectionListener {
    
    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
     */
    @Override
    public void widgetDefaultSelected(SelectionEvent e) {

    }

    /* (non-Javadoc)
     * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    @Override
    public void widgetSelected(SelectionEvent e) {
        sortTree(e);
    }

    /**
     * Sort tree.
     *
     * @param e the e
     */
    private void sortTree(SelectionEvent e) {
        TreeColumn column = (TreeColumn) e.widget;
        Tree tree = column.getParent();
        TreeItem[] treeItems = tree.getItems();
        TreeColumn sortColumn = tree.getSortColumn();
        TreeColumn columns[] = tree.getColumns();
        tree.setSortColumn(column);
        int numOfColumns = columns.length;
        int columnIndex = this.findColumnIndex(columns, column, numOfColumns);
        Collator collator = Collator.getInstance(Locale.getDefault());
        Boolean sort = false;
        Pattern pattern = Pattern.compile("([\\+]*|[\\-]*)\\d+");
        if ((column.equals(sortColumn)) && (tree.getSortDirection() == SWT.UP)) {
            tree.setSortDirection(SWT.DOWN);
            for (int i = 0; i < treeItems.length; i++) {
                sortTreeItemDown(treeItems[i], treeItems[i].getItems(),
                        columnIndex, pattern, sort, collator, numOfColumns);

                String value1 = treeItems[i].getText(columnIndex).trim();
                for (int j = 0; j < i; j++) {
                    String value2 = treeItems[j].getText(columnIndex).trim();
                    if (pattern.matcher(value1).matches()
                            && pattern.matcher(value2).matches()) {
                        double d1 = this.getDouble(value1);
                        double d2 = this.getDouble(value2);
                        if (d1 > d2) {
                            sort = true;
                        }
                    } else if (collator.compare(value1, value2) > 0) {
                        sort = true;
                    }
                    if (sort) {
                        String[] values = this.getColumnValues(treeItems[i],
                                numOfColumns);
                        TreeItem[] subItems = treeItems[i].getItems();
                        TreeItem item = new TreeItem(tree, SWT.NONE, j);
                        item.setText(values);
                        for (TreeItem si : subItems) {
                            TreeItem[] subSubItems = si.getItems();
                            TreeItem subItem = new TreeItem(item, SWT.NONE);
                            subItem.setText(this.getColumnValues(si,
                                    numOfColumns));
                            for (TreeItem ssi : subSubItems) {
                                TreeItem subSubItem = new TreeItem(subItem,
                                        SWT.NONE);
                                subSubItem.setText(this.getColumnValues(ssi,
                                        numOfColumns));
                            }
                        }
                        treeItems[i].dispose();
                        treeItems = tree.getItems();
                        sort = false;
                        break;
                    }
                }
                sortTreeItemDown(treeItems[i], treeItems[i].getItems(),
                        columnIndex, pattern, sort, collator, numOfColumns);

            }
        } else {
            tree.setSortDirection(SWT.UP);
            for (int i = 0; i < treeItems.length; i++) {
                sortTreeItemUp(treeItems[i], treeItems[i].getItems(),
                        columnIndex, pattern, sort, collator, numOfColumns);

                String value1 = treeItems[i].getText(columnIndex).trim();
                for (int j = 0; j < i; j++) {

                    String value2 = treeItems[j].getText(columnIndex).trim();
                    if (pattern.matcher(value1).matches()
                            && pattern.matcher(value2).matches()) {
                        double d1 = this.getDouble(value1);
                        double d2 = this.getDouble(value2);
                        if (d1 < d2) {
                            sort = true;
                        }
                    } else if (collator.compare(value1, value2) < 0) {
                        sort = true;
                    }
                    if (sort) {
                        String[] values = this.getColumnValues(treeItems[i],
                                numOfColumns);
                        TreeItem[] subItems = treeItems[i].getItems();
                        TreeItem item = new TreeItem(tree, SWT.NONE, j);
                        item.setText(values);
                        for (TreeItem si : subItems) {
                            TreeItem[] subSubItems = si.getItems();
                            TreeItem subItem = new TreeItem(item, SWT.NONE);
                            subItem.setText(this.getColumnValues(si,
                                    numOfColumns));
                            for (TreeItem ssi : subSubItems) {
                                TreeItem subSubItem = new TreeItem(subItem,
                                        SWT.NONE);
                                subSubItem.setText(this.getColumnValues(ssi,
                                        numOfColumns));
                            }
                        }
                        treeItems[i].dispose();
                        treeItems = tree.getItems();
                        sort = false;
                        break;
                    }
                }
                sortTreeItemUp(treeItems[i], treeItems[i].getItems(),
                        columnIndex, pattern, sort, collator, numOfColumns);

            }
        }
    }

    /**
     * Find the index of a column.
     *
     * @param columns the columns
     * @param column the column
     * @param numOfColumns the num of columns
     * @return int
     */
    private int findColumnIndex(TreeColumn[] columns, TreeColumn column,
            int numOfColumns) {
        int index = 0;
        for (int i = 0; i < numOfColumns; i++) {
            if (column.equals(columns[i])) {
                index = i;
                break;
            }
        }
        return index;
    }

    /**
     * Get the double value from a string.
     *
     * @param str the str
     * @return double
     */
    private double getDouble(String str) {
        double d;
        if (str.startsWith("+")) {
            d = Double.parseDouble(str.split("\\+")[1]);
        } else {
            d = Double.parseDouble(str);
        }
        return d;
    }

    /**
     * Get the array of string value from the provided TreeItem.
     *
     * @param treeItem the tree item
     * @param numOfColumns the num of columns
     * @return String[]
     */
    private String[] getColumnValues(TreeItem treeItem, int numOfColumns) {
        String[] values = new String[numOfColumns];
        for (int i = 0; i < numOfColumns; i++) {
            values[i] = treeItem.getText(i);
        }
        return values;
    }

    /**
     * Sort tree item down.
     *
     * @param tree the tree
     * @param treeItems the tree items
     * @param columnIndex the column index
     * @param pattern the pattern
     * @param sort the sort
     * @param collator the collator
     * @param numOfColumns the num of columns
     */
    public void sortTreeItemDown(TreeItem tree, TreeItem[] treeItems,
            int columnIndex, Pattern pattern, Boolean sort, Collator collator,
            int numOfColumns) {
        for (int i = 0; i < treeItems.length; i++) {
            sortTreeItemDown(treeItems[i], treeItems[i].getItems(),
                    columnIndex, pattern, sort, collator, numOfColumns);

            String value1 = treeItems[i].getText(columnIndex).trim();
            for (int j = 0; j < i; j++) {
                String value2 = treeItems[j].getText(columnIndex).trim();
                if (pattern.matcher(value1).matches()
                        && pattern.matcher(value2).matches()) {
                    double d1 = this.getDouble(value1);
                    double d2 = this.getDouble(value2);
                    if (d1 > d2) {
                        sort = true;
                    }
                } else if (collator.compare(value1, value2) > 0) {
                    sort = true;
                }
                if (sort) {
                    String[] values = this.getColumnValues(treeItems[i],
                            numOfColumns);
                    TreeItem[] subItems = treeItems[i].getItems();
                    TreeItem item = new TreeItem(tree, SWT.NONE, j);
                    item.setText(values);
                    for (TreeItem si : subItems) {
                        TreeItem[] subSubItems = si.getItems();
                        TreeItem subItem = new TreeItem(item, SWT.NONE);
                        subItem.setText(this.getColumnValues(si, numOfColumns));
                        for (TreeItem ssi : subSubItems) {
                            TreeItem subSubItem = new TreeItem(subItem,
                                    SWT.NONE);
                            subSubItem.setText(this.getColumnValues(ssi,
                                    numOfColumns));
                        }
                    }
                    treeItems[i].dispose();
                    treeItems = tree.getItems();
                    sort = false;
                    break;
                }
            }
            sortTreeItemDown(treeItems[i], treeItems[i].getItems(),
                    columnIndex, pattern, sort, collator, numOfColumns);

        }
    }

    /**
     * Sort tree item up.
     *
     * @param tree the tree
     * @param treeItems the tree items
     * @param columnIndex the column index
     * @param pattern the pattern
     * @param sort the sort
     * @param collator the collator
     * @param numOfColumns the num of columns
     */
    public void sortTreeItemUp(TreeItem tree, TreeItem[] treeItems,
            int columnIndex, Pattern pattern, Boolean sort, Collator collator,
            int numOfColumns) {
        for (int i = 0; i < treeItems.length; i++) {
            sortTreeItemUp(treeItems[i], treeItems[i].getItems(), columnIndex,
                    pattern, sort, collator, numOfColumns);

            String value1 = treeItems[i].getText(columnIndex).trim();
            for (int j = 0; j < i; j++) {
                String value2 = treeItems[j].getText(columnIndex).trim();
                if (pattern.matcher(value1).matches()
                        && pattern.matcher(value2).matches()) {
                    double d1 = this.getDouble(value1);
                    double d2 = this.getDouble(value2);
                    if (d1 < d2) {
                        sort = true;
                    }
                } else if (collator.compare(value1, value2) < 0) {
                    sort = true;
                }
                if (sort) {
                    String[] values = this.getColumnValues(treeItems[i],
                            numOfColumns);
                    TreeItem[] subItems = treeItems[i].getItems();
                    TreeItem item = new TreeItem(tree, SWT.NONE, j);
                    item.setText(values);
                    for (TreeItem si : subItems) {
                        TreeItem[] subSubItems = si.getItems();
                        TreeItem subItem = new TreeItem(item, SWT.NONE);
                        subItem.setText(this.getColumnValues(si, numOfColumns));
                        for (TreeItem ssi : subSubItems) {
                            TreeItem subSubItem = new TreeItem(subItem,
                                    SWT.NONE);
                            subSubItem.setText(this.getColumnValues(ssi,
                                    numOfColumns));
                        }
                    }
                    treeItems[i].dispose();
                    treeItems = tree.getItems();
                    sort = false;
                    break;
                }
            }
            sortTreeItemUp(treeItems[i], treeItems[i].getItems(), columnIndex,
                    pattern, sort, collator, numOfColumns);

        }
    }

}