package carisma.xutils.regulatory.ui.examples;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;

import carisma.xutils.regulatory.ui.first.SituationPopupMenu;
import carisma.xutils.regulatory.ui.model.Data;


// TODO: Auto-generated Javadoc
/**
 * The Class FirstSWTApplication.
 */
public class FirstSWTApplication {

    /**
     * The main method.
     *
     * @param args the arguments
     */
    public static void main(String[] args) {

        // getOntology();

        // Ontology onto = OWL2Ontology
        // .loadFromFile(new File(
        // "/home/bm/workspace2/carisma.regulatory/resources/Entire_Law_Ontology.owl"));
        //
        // for (Rule r : onto.getRules()) {
        // if (r instanceof Law) {
        // Law l = (Law) r;
        // System.out.println(l.getTitle());
        // for (Paragraph p : l.getParagraphs()) {
        // System.out.println("  " + p.getNumber());
        // for (Section s : p.getSections()) {
        // System.out.println(s.getContent());
        // }
        // }
        // }
        // }
        //
        // onto.getRuleElements();

        Display display = new Display();
        Shell shell = new Shell(display);

        GridLayout layout = new GridLayout(2, false);
        shell.setLayout(layout);
        shell.setText("GUI Hauptfenster");

        // Menü oben

        Menu menuBar = new Menu(shell, SWT.BAR);

        MenuItem fileMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuHeader.setText("Datei");

        Menu fileMenu = new Menu(shell, SWT.DROP_DOWN);
        fileMenuHeader.setMenu(fileMenu);

        MenuItem fileLoadItem = new MenuItem(fileMenu, SWT.PUSH);
        fileLoadItem.setText("Laden");

        MenuItem fileSaveItem = new MenuItem(fileMenu, SWT.PUSH);
        fileSaveItem.setText("Speichern");

        MenuItem fileExitItem = new MenuItem(fileMenu, SWT.PUSH);
        fileExitItem.setText("Beenden");

        MenuItem editMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        editMenuHeader.setText("Bearbeiten");

        Menu editMenu = new Menu(shell, SWT.DROP_DOWN);
        editMenuHeader.setMenu(editMenu);

        MenuItem fileSearchItem = new MenuItem(editMenu, SWT.PUSH);
        fileSearchItem.setText("Suche");

        MenuItem helpMenuHeader = new MenuItem(menuBar, SWT.CASCADE);
        helpMenuHeader.setText("Hilfe");

        Menu helpMenu = new Menu(shell, SWT.DROP_DOWN);
        helpMenuHeader.setMenu(helpMenu);

        MenuItem helpTipsTricksItem = new MenuItem(helpMenu, SWT.PUSH);
        helpTipsTricksItem.setText("Tips und Tricks");

        MenuItem helpAboutItem = new MenuItem(helpMenu, SWT.PUSH);
        helpAboutItem.setText("Über die GUI");

        shell.setMenuBar(menuBar);

        // Tabs

        CTabFolder folder = new CTabFolder(shell, SWT.TOP);
        GridData data = new GridData(GridData.FILL, GridData.FILL, true, true,
                2, 1);
        folder.setLayoutData(data);

        CTabItem cTabItem1 = new CTabItem(folder, SWT.NONE);
        cTabItem1.setText("Regularien");
        CTabItem cTabItem2 = new CTabItem(folder, SWT.NONE);
        cTabItem2.setText("Konfiguration");

        // Tab 1

        Composite parent = new Composite(folder, SWT.NONE);
        layout = new GridLayout(2, false);
        parent.setLayout(layout);
        cTabItem1.setControl(parent);

        // Tabelle

        Table table = new Table(parent, SWT.MULTI | SWT.BORDER
                | SWT.FULL_SELECTION);
        table.setLinesVisible(true);
        table.setHeaderVisible(true);
        data = new GridData(SWT.LEFT, SWT.FILL, false, true);
        data.heightHint = 400;
        data.widthHint = 194;
        table.setLayoutData(data);

        String TABLENAME = "Paragraph";

        TableColumn column = new TableColumn(table, SWT.NONE);
        column.setText(TABLENAME);

        for (int i = 1; i <= 50; i++) {
            TableItem item = new TableItem(table, SWT.NONE);
            item.setText(0, "§ " + i);
        }
        table.getColumn(0).pack();

        // Textfeld

        String testtext = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.";

        Data daten = Data.instance();
        // String testtext = daten.getGListe().get(1).getPList().get(6)
        // .getInhalt();
        System.out.println(testtext);

        Text text = new Text(parent, SWT.MULTI | SWT.WRAP | SWT.BORDER);
        text.setText(testtext);
        data = new GridData(SWT.FILL, SWT.FILL, true, false);
        data.widthHint = 500;
        text.setLayoutData(data);

        // Combobox und Buttons Tab 1

        Composite parent3 = new Composite(parent, SWT.NONE);
        layout = new GridLayout(2, false);
        parent3.setLayout(layout);

        Combo combo = new Combo(parent3, SWT.NONE);
        combo.setItems(new String[] { "Regelelement1", "Regelelement2",
                "Regelelement3" });
        // combo.setText(combo.getItem(0));
        combo.setText("Regelelement");
        data = new GridData(SWT.FILL, SWT.FILL, false, false);
        data.widthHint = 130;
        combo.setLayoutData(data);

        // Label label1 = new Label(parent, SWT.NONE);

        Button markieren = new Button(parent3, SWT.PUSH);
        markieren.setText("Markieren");
        data = new GridData(SWT.FILL, SWT.FILL, false, false);
        data.widthHint = 70;
        markieren.setLayoutData(data);

        // combo = new Combo(parent3, SWT.NONE);
        // combo.setItems(new String[] { "Sachverhalt1", "Sachverhalt2",
        // "Sachverhalt3" }); //
        // combo.setText(combo.getItem(0));
        // combo.setText("Sachverhalt");
        // data = new GridData(SWT.FILL, SWT.FILL, false, false);
        // data.widthHint = 100;
        // combo.setLayoutData(data);

        Button sachverhalt = new Button(parent3, SWT.PUSH);
        sachverhalt.setText("Sachverhalt definieren");
        data = new GridData(SWT.FILL, SWT.FILL, false, false);
        data.widthHint = 100;
        data.horizontalSpan = 2;
        sachverhalt.setLayoutData(data);

        // Listener als SelectionListener für "Sachverhalt definieren"-Button

        SelectionListener listener = new SelectionListener() {
            public void widgetSelected(SelectionEvent e) {
                // System.out.println("Sachverhalt erstellt!");
//                SituationPopupMenu test = new SituationPopupMenu(new Shell(),
//                        null);

            }

            public void widgetDefaultSelected(SelectionEvent e) {
            }
        };
        sachverhalt.addSelectionListener(listener);

        // //Listener mit Adapter
        //
        // SelectionListener listener = new SelectionAdapter() {
        // public void widgetSelected(SelectionEvent e) {
        // System.out.println("Sachverhalt erstellt!");
        // }
        // };
        // sachverhalt.addSelectionListener(listener);

        // Dialog dialog = new Dialog(parent, SWT.NONE);
        // sachverhalt.addListener(SWT.Selection, listener);

        Button speichern = new Button(parent3, SWT.PUSH);
        speichern.setText("Speichern");
        data = new GridData(SWT.FILL, SWT.FILL, false, false);
        data.widthHint = 100;
        data.horizontalSpan = 2;
        speichern.setLayoutData(data);

        // Baum erstellen für Sachverhalte

        Tree tree = new Tree(parent, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        tree.setHeaderVisible(true);
        tree.setLinesVisible(true);
        data = new GridData(SWT.FILL, SWT.FILL, false, false);
        // data.heightHint = 200;
        tree.setLayoutData(data);

        TreeColumn column1 = new TreeColumn(tree, SWT.NONE);
        column1.setText("Sachverhalt");
        column1.setWidth(100);
        for (int i = 1; i < 4; i++) {
            TreeItem item = new TreeItem(tree, SWT.NONE);
            item.setText(new String[] { "Sachverhalt " + i });
            for (int j = 1; j < 4; j++) {
                TreeItem subItem = new TreeItem(item, SWT.NONE);
                subItem.setText(new String[] { "Regelelement " + j });
            }
        }
        tree.pack();

        // Tab 2

        parent = new Composite(folder, SWT.NONE);
        layout = new GridLayout(1, false);
        parent.setLayout(layout);
        cTabItem2.setControl(parent);

        Composite parentx = new Composite(parent, SWT.NONE);
        layout = new GridLayout(1, false);
        parentx.setLayout(layout);

        // // Label
        //
        // Label label1 = new Label(parent, SWT.NONE);
        // label1.setText("Regelelement definieren");
        // data = new GridData(SWT.CENTER, SWT.FILL, true, false);
        // label1.setLayoutData(data);
        //
        // label1 = new Label(parent, SWT.NONE);

        // Label label1 = new Label(parent, SWT.NONE);

        Group group = new Group(parent, SWT.NONE);
        group.setText("Regelelement definieren");
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        group.setLayoutData(data);
        layout = new GridLayout(1, false);
        group.setLayout(layout);

        // Baum erstellen

        tree = new Tree(group, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
        tree.setHeaderVisible(true);
        tree.setLinesVisible(true);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.heightHint = 200;
        tree.setLayoutData(data);
        tree.pack();

        column1 = new TreeColumn(tree, SWT.NONE);
        column1.setText("Regelelement");
        column1.setWidth(100);
        TreeColumn column2 = new TreeColumn(tree, SWT.NONE);
        column2.setText("Farbe");
        column2.setWidth(100);
        for (int i = 0; i < 3; i++) {
            TreeItem item = new TreeItem(tree, SWT.NONE);
            item.setText(new String[] { "item " + i, "grün" });
            for (int j = 0; j < 3; j++) {
                TreeItem subItem = new TreeItem(item, SWT.NONE);
                subItem.setText(new String[] { "subitem " + j, "rot" });
            }
        }

        // Buttons Tab 2

        Composite parent2 = new Composite(group, SWT.NONE);
        layout = new GridLayout(3, false);
        parent2.setLayout(layout);

        Button neu = new Button(parent2, SWT.PUSH);
        neu.setText("Neu");
        data = new GridData(SWT.LEFT, SWT.FILL, false, false);
        data.widthHint = 100;
        neu.setLayoutData(data);

        text = new Text(parent2, SWT.BORDER);
        text.setText("Namen eingeben");
        data = new GridData(SWT.FILL, SWT.FILL, false, false);
        text.setLayoutData(data);

        combo = new Combo(parent2, SWT.NONE);
        combo.setItems(new String[] { "Blau", "Grün", "Rot" });
        // combo.setText(combo.getItem(0));
        combo.setText("Farbe");
        data = new GridData(SWT.FILL, SWT.FILL, false, false);
        data.widthHint = 100;
        combo.setLayoutData(data);

        Button loeschen = new Button(parent2, SWT.PUSH);
        loeschen.setText("Löschen");
        data = new GridData(SWT.LEFT, SWT.FILL, false, false);
        data.widthHint = 100;
        loeschen.setLayoutData(data);

        shell.open();
        shell.pack();

        // Hauptfenster in der Mitte des Bildschirms zentrieren

        Monitor primary = display.getPrimaryMonitor();
        Rectangle bounds = primary.getBounds();
        Rectangle rect = shell.getBounds();
        int x = bounds.x + (bounds.width - rect.width) / 2;
        int y = bounds.y + (bounds.height - rect.height) / 2;
        shell.setLocation(x, y);

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }
        display.dispose();
    }
    // public static void getOntology() {
    //
    // // Neues Daten-Objekt erstellen mit Dten.instance() Methode
    // Daten daten = Daten.instance();
    //
    // Ontology onto = OWL2Ontology
    // .loadFromFile(new File(
    // "/home/mansour/workspace2/carisma.regulatory/resources/Entire_Law_Ontology.owl"));
    //
    // for (Rule r : onto.getRules()) {
    // if (r instanceof Law) {
    // Law l = (Law) r;
    // Gesetz g = new Gesetz(l.getTitle());
    // System.out.println(l.getTitle());
    // for (Paragraph p : l.getParagraphs()) {
    // Paragraf para = new Paragraf(p.getNumber());
    // String inhalt = "";
    // System.out.println("  " + p.getNumber());
    // for (Section s : p.getSections()) {
    // inhalt = inhalt + s.getContent() + " ";
    // System.out.println(s.getContent());
    // }
    // para.setInhalt(inhalt);
    // g.addParagraph(para);
    // }
    // daten.addGesetz(g);
    // }
    // }
    //
    // // ((OWL2Ontology)onto).getGenericOntologyHelper().saveOntologyToFile(new
    // // File(""));
    //
    // }
}
