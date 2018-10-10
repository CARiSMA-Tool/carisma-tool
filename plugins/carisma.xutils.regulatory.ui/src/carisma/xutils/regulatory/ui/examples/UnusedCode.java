package carisma.xutils.regulatory.ui.examples;

/**
 * The Class UnusedCode.
 */
public class UnusedCode {

    // // Label
    //
    // Label label1 = new Label(parent, SWT.NONE);
    // label1.setText("Regelelement definieren");
    // data = new GridData(SWT.CENTER, SWT.FILL, true, false);
    // label1.setLayoutData(data);
    //
    // label1 = new Label(parent, SWT.NONE);

    // Label label1 = new Label(parent, SWT.NONE);

    // Start with green
    // color = new Color(parent.getDisplay(), new RGB(0, 150, 0));

    // // Use a label full of spaces to show the color
    // final Label colorLabel = new Label(parent2, SWT.BORDER);
    // colorLabel.setText("      ");
    // colorLabel.setBackground(color);
    //
    // colorLabel.addMouseListener(new MouseListener() {
    // public void mouseDown(MouseEvent e) {
    // }
    //
    // public void mouseUp(MouseEvent e) {
    // System.out.println("Hallo! Es funktioniert!");
    // }
    //
    // public void mouseDoubleClick(MouseEvent e) {
    //
    // }
    // });
    //
    // Button button = new Button(parent2, SWT.PUSH);
    // button.setText("Farbe...");
    // button.addSelectionListener(new SelectionAdapter() {
    // public void widgetSelected(SelectionEvent event) {
    // // Create the color-change dialog
    // Shell shell = new Shell();
    // ColorDialog dlg = new ColorDialog(shell);
    //
    // // Set the selected color in the dialog from
    // // user's selected color
    // dlg.setRGB(colorLabel.getBackground().getRGB());
    //
    // // Change the title bar text
    // dlg.setText("Wähle eine Farbe");
    //
    // // Open the dialog and retrieve the selected color
    // RGB rgb = dlg.open();
    // if (rgb != null) {
    // // Dispose the old color, create the
    // // new one, and set into the label
    // color.dispose();
    // color = new Color(shell.getDisplay(), rgb);
    // colorLabel.setBackground(color);
    // }
    // }
    // });

    // final StyledText text2 = new StyledText(parent2, SWT.BORDER);
    // text2.setText("TEST");
    // data = new GridData(SWT.FILL, SWT.FILL, false, false);
    // text2.setLayoutData(data);
    //
    // StyleRange style = new StyleRange();
    // style.start = 0;
    // style.length = 2;
    // style.foreground = color;
    // text2.setStyleRange(style);
    //
    // // getSelectionText() aufrufen um ein markiertes Wort als String
    // // auslesen zu können
    // // getSelectionCount() bestimmt die Länge des Strings
    // // getSelection() Posiion als Point-Tupel ausgeben
    //
    // Button test = new Button(parent2, SWT.PUSH);
    // test.setText("Markieren");
    // data = new GridData(SWT.LEFT, SWT.FILL, false, false);
    // data.widthHint = 100;
    // test.setLayoutData(data);
    //
    // test.addSelectionListener(new SelectionAdapter() {
    // public void widgetSelected(SelectionEvent event) {
    // System.out.println(text2.getSelectionRange().x);
    //
    // }
    // });

    // // Tabelle
    //
    // Table table = new Table(parent, SWT.MULTI | SWT.BORDER
    // | SWT.FULL_SELECTION);
    // table.setLinesVisible(true);
    // table.setHeaderVisible(true);
    // GridData data = new GridData(SWT.LEFT, SWT.FILL, false, true);
    // data.heightHint = 400;
    // data.widthHint = 194;
    // table.setLayoutData(data);
    //
    // String TABLENAME = "Paragraph";
    //
    // TableColumn column = new TableColumn(table, SWT.NONE);
    // column.setText(TABLENAME);

    // column.addSelectionListener(new SortTreeListener());

    // for (int i = 1; i <= 50; i++) {
    // TreeItem item = new TreeItem(tree1, SWT.NONE);
    // item.setText(0, "§ " + i);
    // }

    // Gesetze und Paragraphen aus den selbstdefinierten Klassen Daten,
    // Gesetz und Paragraf laden

    // for (Gesetz g : daten.getGListe()) {
    //
    // // daten.getGListe().get(1).getPList().get(6).getInhalt();
    // // Gesetz g = new Gesetz(l.getTitle());
    //
    // TreeItem itemG = new TreeItem(tree1, SWT.NONE);
    // itemG.setText(g.getName());
    //
    // for (Paragraf p : g.getPList()) {
    // TreeItem itemP = new TreeItem(itemG, SWT.NONE);
    // itemP.setText("§ " + p.getNummer());
    // }
    // }

    // tree1.getColumn(0).pack();

    // Textfeld

    // String BEISPIELTEXT =
    // "Lorem <html><br />ipsum dolor</html> sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.";
    // String testtext =
    // daten.getGListe().get(1).getPList().get(6).getInhalt();
    // System.out.println(testtext);

    // Label label1 = new Label(parent, SWT.NONE);

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

    // // Suche-Button
    //
    // Button suche = new Button(parent3, SWT.PUSH);
    // suche.setText("Suche");
    // data = new GridData(SWT.FILL, SWT.FILL, false, false);
    // data.widthHint = 100;
    // data.horizontalSpan = 2;
    // suche.setLayoutData(data);
    //
    // // suche.addSelectionListener(new SearchListener());
    // suche.addSelectionListener(new SelectionAdapter() {
    //
    // @Override
    // public void widgetSelected(SelectionEvent e) {
    // // button = (Button) e.widget;
    //
    // search();
    //
    // }
    //
    // @Override
    // public void widgetDefaultSelected(SelectionEvent arg0) {
    // // TODO Auto-generated method stub
    //
    // }
    // });

    // // TreeItems alphabetisch sortieren
    //
    // LinkedList<String> list = new LinkedList<String>();
    // list.add("abc");
    // list.add("Bcd");
    // list.add("aAb");
    // Collections.sort(list, new Comparator<String>() {
    // public int compare(String o1, String o2) {
    // return Collator.getInstance().compare(o1, o2);
    // }
    // });
    //
    // for (String s : list) {
    // System.out.println(s);
    // }

    // // Listen für TreeItems erstellen
    //
    // LinkedList<String> treeItems = new LinkedList<String>();
    // for (TreeItem item : tree1.getItems()) {
    //
    // treeItems.add(item.getText());
    //
    // }
    //
    // Collections.sort(treeItems, new Comparator<String>() {
    // public int compare(String o1, String o2) {
    // return Collator.getInstance().compare(o1, o2);
    // }
    // });
    //
    // for (String s : treeItems) {
    // System.out.println(s);
    // }

    // // Daten daten = Daten.instance();
    //
    // onto = OWL2Ontology
    // .loadFromFile(new File(
    // "/home/bm/workspace/carisma.regulatory/resources/Entire_Law_Ontology.owl"));
    //
    // // Gesetze sortieren
    //
    // ArrayList<Law> listL = new ArrayList<Law>();
    //
    // for (Rule r : onto.getRules()) {
    // if (r instanceof Law) {
    // Law l = (Law) r;
    // listL.add(l);
    // }
    // }
    // Collections.sort(listL, new NaturalOrderComparatorLaw());

    // onto.getRuleElements();

    // Collections.sort(list, new Comparator<Paragraph>() {
    //
    // @Override
    // public int compare(Paragraph arg0, Paragraph arg1) {
    // // TODO Auto-generated method stub
    // int x = 0;
    // if () {
    // x = 1;
    // }
    // if () {
    // x = -1;
    // }
    // if (x==0) {
    // if () {
    // x = 1;
    // }
    // if () {
    // x = -1;
    // }
    // }
    // return x;
    // }
    // });

    // public void getOntologyContentTable() {
    // try {
    //
    // TableItem[] array = table.getSelection();
    //
    // for (Rule r : onto.getRules()) {
    // if (r instanceof Law) {
    // Law l = (Law) r;
    //
    // // Namen des Parents des gewählten Paragraphens prüfen
    // if (l.getTitle().equals(
    // array[0].getParentItem().getParentItem().getText())) {
    //
    // for (Paragraph p : l.getParagraphs()) {
    //
    // // Namen des gewählten Paragraphens prüfen
    // if (("§ " + p.getNumber()).equals(array[0]
    // .getParentItem().getText())) {
    //
    // for (Section s : p.getSections()) {
    // if (("Abs. " + s.getNumber())
    // .equals(array[0].getText())) {
    //
    // text.setText(s.getContent());
    // System.out.println(s.getContent());
    // }
    //
    // // TreeItem itemS = new TreeItem(itemP,
    // // SWT.NONE);
    // // itemS.setText("Abs. " + s.getNumber());
    //
    // // inhalt = inhalt + s.getContent() + " ";
    // // System.out.println(s.getContent());
    //
    // }
    // }
    // // para.setInhalt(inhalt);
    // // g.addParagraph(para);
    // }
    // // daten.addGesetz(g);
    // }
    //
    // tree1.getColumn(0).pack();
    //
    // }
    // }
    // } catch (Exception e) {
    // // e.printStackTrace();
    // System.out.println("Bitte einen Absatz auswählen!");
    //
    // }
    //
    // // onto.getRuleElements();
    // }

    // Falls externe Methode nicht static ist, wird
    // NullPointerException
    // geworfen
}
