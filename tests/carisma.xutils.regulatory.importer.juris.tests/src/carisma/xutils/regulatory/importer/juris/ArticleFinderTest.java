package carisma.xutils.regulatory.importer.juris;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;

public class ArticleFinderTest {

	@Test
	public void testRegexParagraph() {
		Pattern p = Pattern.compile(ArticleFinder.PARAGRAHP);
		Matcher m = p.matcher("Absatz 24");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("Absatz 24", m.group());
			}
		}
		m = p.matcher("Absatz 2222");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("Absatz 2222", m.group());
			}
		}
		m = p.matcher("Abs 2222");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("Abs 2222", m.group());
			}
		}
		m = p.matcher("Abs. 2222");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("Abs. 2222", m.group());
			}
		}
	}
	
	@Test
	public void testRegexSentenceNumber() {
		Pattern p = Pattern.compile(ArticleFinder.SENTENCENUMBER);
		Matcher m = p.matcher("Nr. 4");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("Nr. 4", m.group());
			}
		}
		m = p.matcher("Nummer 172");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("Nummer 172", m.group());
			}
		}
		m = p.matcher("Nr 777");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("Nr 777", m.group());
			}
		}
	}
	
	@Test
	public void testRegexLaw() {
		Pattern p = Pattern.compile(ArticleFinder.LAW);
		Matcher m = p.matcher("Ordnung ");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("Ordnung ", m.group());
			}
		}
		m = p.matcher("gesetzes");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("gesetzes", m.group());
			}
		}
		m = p.matcher("Gesetz");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("Gesetz", m.group());
			}
		}
		m = p.matcher("ordnung ");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("ordnung ", m.group());
			}
		}
		m = p.matcher("Artikel");
		while(m.find()) { 
			if (!m.group().equals("")) {
				assertEquals("Artikel", m.group());
			}
		}
	}
	
	@Test
	public void textGetLawCrossConnections() {
		String law1 =  	"(1) Die Vorschriften dieses Abschnittes gelten für öffentliche Stellen des Bundes, soweit sie nicht als öffentlich-rechtliche Unternehmen am Wettbewerb teilnehmen."
						+ "(2) Soweit der Datenschutz nicht durch Landesgesetz geregelt ist, gelten die §§ 12 bis 16, 19 bis 20 auch für die öffentlichen Stellen der Länder, soweit sie"
						+ "1. Bundesrecht ausführen und nicht als öffentlich-rechtliche Unternehmen am Wettbewerb teilnehmen oder"
						+ "2. als Organe der Rechtspflege tätig werden und es sich nicht um Verwaltungsangelegenheiten handelt."
						+ "(3) Für Landesbeauftragte für den Datenschutz gilt § 23 Abs. 4 entsprechend."
						+ "(4) Werden personenbezogene Daten für frühere, bestehende oder zukünftige Beschäftigungsverhältnisse erhoben, verarbeitet oder genutzt, gelten § 28 Absatz 2 Nummer 2 und die §§ 32 bis 35 anstelle der §§ 13 bis 16 und 19 bis 20.";
		ArticleFinder finder = new ArticleFinder();
			// only 13 because the text contains duplicated entries
		assertEquals(13, finder.getLawCrossConnections(law1).size());
		String law2 = 	"(1) Die Aufsichtsbehörde kontrolliert die Ausführung dieses Gesetzes sowie anderer Vorschriften über den Datenschutz, soweit diese die automatisierte Verarbeitung personenbezogener Daten oder die Verarbeitung oder Nutzung personenbezogener Daten in oder aus nicht automatisierten Dateien regeln einschließlich des Rechts der Mitgliedstaaten in den Fällen des § 1 Abs. 5. Sie berät und unterstützt die Beauftragten für den Datenschutz und die verantwortlichen Stellen mit Rücksicht auf deren typische Bedürfnisse. Die Aufsichtsbehörde darf die von ihr gespeicherten Daten nur für Zwecke der Aufsicht verarbeiten und nutzen; § 14 Abs. 2 Nr. 1 bis 3, 6 und 7 gilt entsprechend. Insbesondere darf die Aufsichtsbehörde zum Zweck der Aufsicht Daten an andere Aufsichtsbehörden übermitteln. Sie leistet den Aufsichtsbehörden anderer Mitgliedstaaten der Europäischen Union auf Ersuchen ergänzende Hilfe (Amtshilfe). Stellt die Aufsichtsbehörde einen Verstoß gegen dieses Gesetz oder andere Vorschriften über den Datenschutz fest, so ist sie befugt, die Betroffenen hierüber zu unterrichten, den Verstoß bei den für die Verfolgung oder Ahndung zuständigen Stellen anzuzeigen sowie bei schwerwiegenden Verstößen die Gewerbeaufsichtsbehörde zur Durchführung gewerberechtlicher Maßnahmen zu unterrichten. Sie veröffentlicht regelmäßig, spätestens alle zwei Jahre, einen Tätigkeitsbericht. § 21 Satz 1 und § 23 Abs. 5 Satz 4 bis 7 gelten entsprechend."
						+ "(2) Die Aufsichtsbehörde führt ein Register der nach § 4d meldepflichtigen automatisierten Verarbeitungen mit den Angaben nach § 4e Satz 1. Das Register kann von jedem eingesehen werden. Das Einsichtsrecht erstreckt sich nicht auf die Angaben nach § 4e Satz 1 Nr. 9 sowie auf die Angabe der zugriffsberechtigten Personen."
						+ "(3) Die der Kontrolle unterliegenden Stellen sowie die mit deren Leitung beauftragten Personen haben der Aufsichtsbehörde auf Verlangen die für die Erfüllung ihrer Aufgaben erforderlichen Auskünfte unverzüglich zu erteilen. Der Auskunftspflichtige kann die Auskunft auf solche Fragen verweigern, deren Beantwortung ihn selbst oder einen der in § 383 Abs. 1 Nr. 1 bis 3 der Zivilprozessordnung bezeichneten Angehörigen der Gefahr strafgerichtlicher Verfolgung oder eines Verfahrens nach dem Gesetz über Ordnungswidrigkeiten aussetzen würde. Der Auskunftspflichtige ist darauf hinzuweisen."
						+ "(4) Die von der Aufsichtsbehörde mit der Kontrolle beauftragten Personen sind befugt, soweit es zur Erfüllung der der Aufsichtsbehörde übertragenen Aufgaben erforderlich ist, während der Betriebs- und Geschäftszeiten Grundstücke und Geschäftsräume der Stelle zu betreten und dort Prüfungen und Besichtigungen vorzunehmen. Sie können geschäftliche Unterlagen, insbesondere die Übersicht nach § 4g Abs. 2 Satz 1 sowie die gespeicherten personenbezogenen Daten und die Datenverarbeitungsprogramme, einsehen. § 24 Abs. 6 gilt entsprechend. Der Auskunftspflichtige hat diese Maßnahmen zu dulden."
						+ "(5) Zur Gewährleistung der Einhaltung dieses Gesetzes und anderer Vorschriften über den Datenschutz kann die Aufsichtsbehörde Maßnahmen zur Beseitigung festgestellter Verstöße bei der Erhebung, Verarbeitung oder Nutzung personenbezogener Daten oder technischer oder organisatorischer Mängel anordnen. Bei schwerwiegenden Verstößen oder Mängeln, insbesondere solchen, die mit einer besonderen Gefährdung des Persönlichkeitsrechts verbunden sind, kann sie die Erhebung, Verarbeitung oder Nutzung oder den Einsatz einzelner Verfahren untersagen, wenn die Verstöße oder Mängel entgegen der Anordnung nach Satz 1 und trotz der Verhängung eines Zwangsgeldes nicht in angemessener Zeit beseitigt werden. Sie kann die Abberufung des Beauftragten für den Datenschutz verlangen, wenn er die zur Erfüllung seiner Aufgaben erforderliche Fachkunde und Zuverlässigkeit nicht besitzt."
						+ "(6) Die Landesregierungen oder die von ihnen ermächtigten Stellen bestimmen die für die Kontrolle der Durchführung des Datenschutzes im Anwendungsbereich dieses Abschnittes zuständigen Aufsichtsbehörden."
						+ "(7) Die Anwendung der Gewerbeordnung auf die den Vorschriften dieses Abschnittes unterliegenden Gewerbebetriebe bleibt unberührt.";
//		for(LawCrossConnection lcc : finder.getLawCrossConnections(law2)) {
//			lcc.printLawCrossConnection();
//		}
			// only 9 because the sentences have been ignored
		assertEquals(9, finder.getLawCrossConnections(law2).size());
		
	}
}
