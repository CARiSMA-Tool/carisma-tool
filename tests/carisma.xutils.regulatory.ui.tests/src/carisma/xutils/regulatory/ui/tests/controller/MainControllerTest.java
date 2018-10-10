package carisma.xutils.regulatory.ui.tests.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.junit.BeforeClass;
import org.junit.Test;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.first.ConfigurationView;
import carisma.xutils.regulatory.ui.first.RegulationsView;
import carisma.xutils.regulatory.ui.model.ConstraintModel;
import carisma.xutils.regulatory.ui.model.CreationException;

/**
 * This JUnit test-class tests the MainController class of the regulatory GUI.
 * @author Klaus Rudack
 *
 */
public class MainControllerTest {
	
	/**
	 * display for the gui.
	 */
	private static Display display = null;
	
	/**
	 * shell for the gui.
	 */
	private static Shell shell = null;
	
	/**
	 * path to a correct ontology.
	 * the ontology in this project is resources/model/Ontology.owl
	 */
	private String ontoPath = "resources" + System.getProperty("file.separator")
			+ "model" + System.getProperty("file.separator") + "Ontology.owl"; //"Entire_Law_Ontology.owl";

	
	/**
	 * initializes the display and the shell before class.
	 */
	@BeforeClass
	public static void init() {
		display = new Display();
		shell = new Shell(display);
	}
	
	/**
	 * This test tests the getInstance() method of the MainController class.
	 */
	@Test
	public final void testGetInstance() {
		MainController mc = MainController.getInstance();
		assertNotNull(mc);
	}
	
//	/**
//	 * This test tests the loadOntology() method of the MainController class with invalid input.
//	 */	
//	@Test
//	public final void testLoadOntologyInvalidInput() {
//		RegulationsView regulationsView = new RegulationsView();
//		ConfigurationView cv = new ConfigurationView();
//		MainController mc = MainController.getInstance();
//		assertNotNull(mc);
//		Display display = new Display();
//		final Shell shell = new Shell(display);
//		shell.setSize(300, 300);
//	    shell.setLayout(new RowLayout());
//		Composite composite = new Composite(shell, 0);
//		cv.createPartControl(shell);
//		regulationsView.createPartControl(composite);
//		assertFalse(mc.loadOntology(null));
//		RegulatoryOntologyHelper roh = mc.getROH();
//		assertNull(roh);
//		assertFalse(mc.loadOntology("absurdFileName"));
//		RegulatoryOntologyHelper roh = mc.getROH();
//		assertNull(roh);
//	}
	
	/**
	 * This test tests the loadOntology() method of the MainController class with valid input.
	 */
	@Test
	public final void testLoadOntology() {
		RegulationsView regulationsView = new RegulationsView();
		ConfigurationView cv = new ConfigurationView();
		MainController mc = MainController.getInstance();
		assertNotNull(mc);
		shell.setSize(300, 300);
	    shell.setLayout(new RowLayout());
		Composite composite = new Composite(shell, 0);
		cv.createPartControl(shell);
		regulationsView.createPartControl(composite);
		mc.loadOntology(ontoPath);
		RegulatoryOntologyHelper roh = mc.getROH();
		assertNotNull(roh);
	}


	/**
	 * This test tests the createNewRuleElement() method of the MainController class.
	 */
	@Test
	public final void testCreateNewRuleElement() {
		String ruleElementType = "Role";
		String ruleElementName = "RuleElementName";
		String expectation = ruleElementType + "_" + ruleElementName;
		MainController mc = MainController.getInstance();
		assertNotNull(mc);
		testLoadOntology();
		mc.createNewRuleElement(ruleElementType, ruleElementName);
		List<String> allElements = mc.getRuleElements();
		assertTrue(allElements.contains(expectation));
		System.out.println(allElements);
	}
	
	/**
	 * This test tests the getTextFromIndividual() method of the MainController class.
	 */
	@Test
	public final void testGetTextFromIndividual() {
		String ruleId = "B_3.204";
		String ruleClazz = "BSIElement";
		String expectation = getTextForTest();
		String result = null;
		MainController mc = MainController.getInstance();
		assertNotNull(mc);
		testLoadOntology();
		result = mc.getTextFromIndividual(ruleId, ruleClazz);
		assertEquals(expectation, result);
	}
	
	/**
	 * This test tests the getTextFromIndividual() method of the MainController class with invalid input.
	 */
	@Test
	public final void testGetTextFromIndividualIndalidInput() {
		String validId = "B_3.204";
		String validClass = "BSIElement";
		MainController mc = MainController.getInstance();
		assertNotNull(mc);
		testLoadOntology();
		try {
			mc.getTextFromIndividual(null, validClass);
			fail("Expected NullPointerException for getTextFromIndividual(null, validClass) hasn't been thrown");
		} catch (NullPointerException e) { }
		try {
			mc.getTextFromIndividual(validId, null);
			fail("Expected NullPointerException for getTextFromIndividual(validId, null) hasn't been thrown");
		} catch (NullPointerException e) { }
		try {
			mc.getTextFromIndividual("blablub", validClass);
			fail("Expected NullPointerException for getTextFromIndividual(\"blablub\", validClass) hasn't been thrown");
		} catch (NullPointerException e) { }
		try {
			mc.getTextFromIndividual(validId, "blablub");
			fail("Expected IllegalArgumentException for getTextFromIndividual(validId, \"blablub\") hasn't been thrown");
		} catch (IllegalArgumentException e) { }
	}
	
	/**
	 * This test tests the checkSelectedRuleElements() method of the MainController class.
	 */
	@Test
	public final void testCheckSelectedRuleElements() {
		String firstRuleElement = "Role_Wartungspersonal";
		String secondRuleElement = "Role_Telearbeiter";
		String thirdRuleElement = "Activity_funktionierendes";
		MainController mc = MainController.getInstance();
		assertNotNull(mc);
		testLoadOntology();
		assertFalse(mc.checkSelectedRuleElements(firstRuleElement, secondRuleElement));
		assertTrue(mc.checkSelectedRuleElements(firstRuleElement, thirdRuleElement));
	}
	
//	/**
//	 * This test tests the createNewRelation() and getRuleElementRelations() methods of the MainController class.
//	 */
//	@Test
//	public final void testCreateNewRelation() {
//		String firstIdString = "B_3.204";
//		String firstClassString = "BSIElement";
//		String secondIdString = "M_2.212";
//		String secondClassString = "BSIMeasure";
//		String description = "Example description";
//		OWLNamedIndividual firstRuleElement = null;
//		OWLNamedIndividual secondRuleElement = null;
//		MainController mc = MainController.getInstance();
//		assertNotNull(mc);
//		testLoadOntology();
//		firstRuleElement = mc.getIndividual(firstIdString, firstClassString);
//		assertNotNull(firstRuleElement);
//		secondRuleElement = mc.getIndividual(secondIdString, secondClassString);
//		assertNotNull(secondRuleElement);
//		System.out.println("Erstes: " + firstRuleElement.getIRI().getFragment() + "|| Zweites: " + secondRuleElement.getIRI().getFragment());
//		assertTrue(mc.checkSelectedRuleElements(firstRuleElement.toString(), secondRuleElement.toString()));
//		try {
//			mc.createNewRelation(firstRuleElement, secondRuleElement, description);
//		} catch (CreationException e) {
//			fail("CreationException has been thrown!");
//		}
//		System.out.println(mc.getRuleElementRelations(firstIdString, firstClassString).size());
//	}
	
	/**
	 * This test tests the getConstraints() method of the MainController class.
	 */
	@Test
	public final void testGetConstraints() {
		List<ConstraintModel> results;
		MainController mc = MainController.getInstance();
		assertNotNull(mc);
		testLoadOntology();
		results = mc.getConstraints();
		assertEquals(3, results.size());
//		TODO sinvoller test
	}
	
	/**
	 * This test tests the createSituation() and getSituation() methods of the MainController class.
	 */
	@Test
	public final void testCreateSituation() {
		MainController mc = MainController.getInstance();
		assertNotNull(mc);
//		testLoadOntology();
//		TODO sinvoller test
	}
	
	/**
	 * This test tests the getSubElementsOfRe() method of the MainController class.
	 */
	@Test
	public final void testgetSubElementsOfRe() {
		MainController mc = MainController.getInstance();
		assertNotNull(mc);
//		testLoadOntology();
//		TODO sinvoller test
	}
	
	/**
	 * This test tests the createNewRuleElementInstance() method of the MainController class.
	 */
	@Test
	public final void testcreateNewRuleElementInstance() {
		MainController mc = MainController.getInstance();
		assertNotNull(mc);
//		testLoadOntology();
//		TODO sinvoller test
	}

	
	/**
	 * Method that returns the text that it expected in the testGetTextFromIndividual() method.
	 * @return - the expectation of the testGetTextFromIndividual() method.
	 */
	private String getTextForTest() {
		return "\nB 3.204 Client unter Unix\n"
			+ "Beschreibung\n\n"
			
			+ "Betrachtet wird ein Unix-System, das entweder im Stand-Alone-Betrieb oder als Client in einem Netz genutzt wird. Es können Terminals, Laufwerke"
			+ ", Drucker und andere Geräte angeschlossen sein. Weiterhin kann eine graphische Benutzeroberfläche wie X-Window eingesetzt sein. Entsprechend"
			+ " können dann auch X-Terminals und graphische Eingabegeräte angeschlossen sein. Bei den weiteren Betrachtungen wird davon ausgegangen, dass ein"
			+ " Unix-System üblicherweise von mehreren Personen benutzt wird.\n\n"
			
			+ "Beispiele für klassische Unix-Systeme sind die BSD-Reihe (FreeBSD, OpenBSD und NetBSD), Solaris und AIX. Obwohl Linux kein klassisches, sondern"
			+ " ein funktionelles Unix ist (der Kernel basiert nicht auf dem ursprünglichen Quelltext, aus dem sich die verschiedenen Unix-Derivate entwickelt"
			+ " haben), wird Linux ebenfalls in diesem Baustein betrachtet.\n"
			+ "Gefährdungslage\n\n"
			
			+ "Für den IT-Grundschutz eines Unix-Systems werden folgende typische Gefährdungen angenommen:\n"
			+ "Höhere Gewalt\n"
			+ "G 1.1PersonalausfallG 1.2Ausfall von IT-SystemenG 1.8Staub, VerschmutzungOrganisatorische Mängel\n"
			+ "G 2.7Unerlaubte Ausübung von RechtenG 2.9Mangelhafte Anpassung an Veränderungen beim IT-EinsatzG 2.15Vertraulichkeitsverlust schutzbedürftiger"
			+ " Daten im Unix-SystemMenschliche Fehlhandlungen\n"
			+ "G 3.2Fahrlässige Zerstörung von Gerät oder DatenG 3.3Nichtbeachtung von SicherheitsmaßnahmenG 3.6Gefährdung durch Reinigungs- oder Fremdpersonal"
			+ "G 3.8Fehlerhafte Nutzung von IT-SystemenG 3.9Fehlerhafte Administration von IT-SystemenTechnisches Versagen\n"
			+ "G 4.8Bekanntwerden von SoftwareschwachstellenG 4.11Fehlende Authentisierungsmöglichkeit zwischen NIS-Server und NIS-ClientG 4.12Fehlende"
			+ " Authentisierungsmöglichkeit zwischen X-Server und X-ClientVorsätzliche Handlungen\n"
			+ "G 5.1Manipulation oder Zerstörung von Geräten oder ZubehörG 5.2Manipulation an Informationen oder SoftwareG 5.4DiebstahlG 5.7Abhören von"
			+ " LeitungenG 5.8Manipulation an LeitungenG 5.9Unberechtigte IT-NutzungG 5.18Systematisches Ausprobieren von PasswörternG 5.19Missbrauch von"
			+ " BenutzerrechtenG 5.20Missbrauch von AdministratorrechtenG 5.21Trojanische PferdeG 5.23SchadprogrammeG 5.41Mißbräuchliche Nutzung eines"
			+ " Unix-Systems mit Hilfe von uucpG 5.89Hijacking von Netz-VerbindungenMaßnahmenempfehlungen\n\n"
			
			+ "Um den betrachteten Informationsverbund abzusichern, müssen zusätzlich zu diesem Baustein noch weitere Bausteine umgesetzt werden, gemäß den"
			+ " Ergebnissen der Modellierung nach IT-Grundschutz.\n\n"
			
			+ "Für Clients unter Unix sind eine Reihe von Maßnahmen umzusetzen, beginnend mit der Planung des Einsatzes über den Betrieb bis zur"
			+ " Notfallvorsorge. Die Schritte, die dabei durchlaufen werden sollten, sowie die Maßnahmen, die in den jeweiligen Schritten beachtet werden"
			+ " sollten, sind im folgenden aufgeführt.\n"
			+ "Planung und Konzeption\n\n"
			
			+ "Schon vor dem erstmaligen Einsatz eines Unix-Systems, gleichgültig ob es als Client, als Terminal- oder Anwendungsserver oder als Einzelplatz-"
			+ "System eingesetzt werden soll, sind eine Reihe von Festlegungen zu treffen, die die Grundlage eines geordneten, sicheren Betriebs bilden. Werden"
			+ " hier Fehler gemacht, so lassen sich diese im Nachhinein oft nur mit sehr hohem Aufwand korrigieren.\n\n"
			
			+ "Es ist ein Verfahren für die Vergabe von User-IDs festzulegen, durch das gewährleistet wird, dass privilegierte und unprivilegierte"
			+ " Benutzerkennungen klar getrennt sind. Weiterhin ist sicherzustellen, dass kein unkontrollierter Zugang zum Single-User-Modus möglich ist, da"
			+ " sonst alle für die Laufzeit des Systems festgelegten Sicherheitsmaßnahmen unterlaufen werden können.\n"
			+ "Umsetzung\n\n"
			
			+ "Bei der Einrichtung eines Unix-Systems sind eine Reihe von Maßnahmen (siehe vor allem dazu die Maßnahme, M 4.105 Erste Maßnahmen nach einer Unix"
			+ "-Standardinstallation zu treffen, die die Sicherheit dieses Systems \"härten\", also Lücken schließen, die nach einer Standardinstallation in"
			+ " der Regel vorhanden sind. Dazu gehört auch, dass nur die wirklich benötigten Netzdienste aktiviert werden (siehe Maßnahme M 5.72 Deaktivieren"
			+ " nicht benötigter Netzdienste ) und dass die Systemprotokollierung aktiviert wird.\n\n"
			
			+ "Ferner sind die Zugriffsrechte auf Benutzer- und Systemdateien und -verzeichnisse so nach einem übergreifenden Schema zu vergeben, dass nur"
			+ " diejenigen Benutzer und Prozesse Zugriff erhalten, die diesen wirklich benötigen, wobei insbesondere auf die durch setuid und setgid bestimmten"
			+ " Rechte zu achten ist (siehe dazu die Maßnahme M 4.19 Restriktive Attributvergabe bei Unix-Systemdateien und -verzeichnissen ).\n"
			+ "Betrieb\n\n"
			
			+ "Um den Überblick über die Sicherheit eines Unix-Systems zu behalten, ist es unabdingbar, die vorhandenen Benutzerprofile und ihre Rechte zeitnah"
			+ " zu dokumentieren, diese Dokumentation immer auf dem aktuellen Stand zu halten und durch regelmäßige Überprüfungen mit der Realität abzugleichen"
			+ ". Die Sicherheit des Systems ist regelmäßig zu überprüfen, wobei auch die vom System erzeugten Protokolle auf eventuelle Unregelmäßigkeiten hin"
			+ " zu betrachten sind.\n"
			+ "Notfallvorsorge\n\n"
			
			+ "Da Unix-Systeme aufgrund ihrer Komplexität nach einem erfolgreichen Angriff oft auf schwer durchschaubare Weise kompromittiert sind, ist es"
			+ " wichtig, schon im Vorfeld Regeln festzulegen, nach denen bei einem echten oder vermuteten Verlust der Systemintegrität zu verfahren ist.\n\n"
			
			+ "Nachfolgend wird das Maßnahmenbündel für den Bereich \"Client unter Unix\" vorgestellt.\n\n"
			
			+ "Für eventuell angeschlossene Rechner (z. B. Clients unter Windows) sind die in den entsprechenden Bausteinen beschriebenen Maßnahmen zu"
			+ " realisieren.\n\n"
			
			+ "Darüber hinaus sind folgende weitere Maßnahmen umzusetzen:\n"
			+ "Planung und Konzeption\n"
			+ "M 2.33(Z)Aufteilung der Administrationstätigkeiten unter UnixM 4.13(A)Sorgfältige Vergabe von IDsM 4.18(A)Administrative und technische"
			+ " Absicherung des Zugangs zum Monitor- und Single-User-ModusM 5.34(Z)Einsatz von EinmalpasswörternM 5.64(Z)Secure ShellUmsetzung\n"
			+ "M 2.32(Z)Einrichtung einer eingeschränkten BenutzerumgebungM 4.9(A)Einsatz der Sicherheitsmechanismen von X-WindowM 4.14(A)Obligatorischer"
			+ " Passwortschutz unter UnixM 4.16(C)Zugangsbeschränkungen für Accounts und / oder TerminalsM 4.17(A)Sperren und Löschen nicht benötigter"
			+ " Accounts und TerminalsM 4.19(A)Restriktive Attributvergabe bei Unix-Systemdateien und -verzeichnissenM 4.20(B)Restriktive Attributvergabe bei"
			+ " Unix-Benutzerdateien und -verzeichnissenM 4.21(A)Verhinderung des unautorisierten Erlangens von AdministratorrechtenM 4.22(Z)Verhinderung des"
			+ " Vertraulichkeitsverlusts schutzbedürftiger Daten im Unix-SystemM 4.23(B)Sicherer Aufruf ausführbarer DateienM 4.105(A)Erste Maßnahmen nach"
			+ " einer Unix-StandardinstallationM 4.106(A)Aktivieren der SystemprotokollierungM 5.17(A)Einsatz der Sicherheitsmechanismen von NFSM 5.18(A)"
			+ "Einsatz der Sicherheitsmechanismen von NISM 5.19(A)Einsatz der Sicherheitsmechanismen von sendmailM 5.20(A)Einsatz der Sicherheitsmechanismen"
			+ " von rlogin, rsh und rcpM 5.21(A)Sicherer Einsatz von telnet, ftp, tftp und rexecM 5.35(A)Einsatz der Sicherheitsmechanismen von UUCPM 5.72(A)"
			+ "Deaktivieren nicht benötigter NetzdiensteBetrieb\n"
			+ "M 4.25(A)Einsatz der Protokollierung im Unix-SystemM 4.26(C)Regelmäßiger Sicherheitscheck des Unix-SystemsNotfallvorsorge\n"
			+ "M 6.31(A)Verhaltensregeln nach Verlust der Systemintegrität\n";
	}
	
}
