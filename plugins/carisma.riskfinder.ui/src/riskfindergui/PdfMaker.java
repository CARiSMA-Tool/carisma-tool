package riskfindergui;

import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import carisma.check.riskfinder.AnalyserResult;

import com.itextpdf.text.Chunk;
import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Font;
import com.itextpdf.text.ListItem;
import com.itextpdf.text.PageSize;
import com.itextpdf.text.Paragraph;
import com.itextpdf.text.pdf.PdfWriter;

/**
 * This class creates a pdf report from a given riskfinder analysis.
 * @author Tobias Meier
 *
 */
public final class PdfMaker {

	/**
	 * the model that has been analyzed will be stored here.
	 */
	private static List<RiskActivity> riskModel = new ArrayList<RiskActivity>();
	
	/**
	 * the result of the riskfinder will be stored here.
	 */
	private static List<AnalyserResult> resultAr = new ArrayList<AnalyserResult>();
	
	/**
	 * path to the folder where the pdf should be saved.
	 */
	private static String fILEpATH = System.getProperty("user.home") + System.getProperty("file.separator") + "report";
//	TODO Klaus R.: anderen Ort finden zum speichern, Nutzer soll Ort angeben koennen.
	
	/**
	 * path to save the pdf to.
	 */
	private static String fILE = fILEpATH + System.getProperty("file.separator") + "report.pdf";
//	TODO Klaus R.: Namen abhaengig vom analisierten Model machen oder durch Benutzer festlegen lassen.
	
	/**
	 * font for the headings.
	 */
	private static Font font1 = new Font(Font.FontFamily.TIMES_ROMAN, 24, Font.BOLD);
	
	/**
	 * font for the paragrap-headings.
	 */
	private static Font font2 = new Font(Font.FontFamily.TIMES_ROMAN, 15, Font.BOLD);
	
	/**
	 * font for the text.
	 */
	private static Font font3 = new Font(Font.FontFamily.TIMES_ROMAN, 13, Font.BOLD);
	
	/**
	 * separator to separate the heading from the results.
	 */
	private static String separator = "--------------------------------------------------"
						+ "-------------------------------------------------------------------------";

	/**
	 * private constructor.
	 * @param args arguments that could be given
	 */
	private PdfMaker(final String[] args) {
		
	}
	
	/**
	 * initial method to create the pdf-report.
	 * @param riskModel the model that was analyzed
	 * @param resultAr the results of the riskfinder
	 */
	public static void makePdf(final List<RiskActivity> riskModel, final List<AnalyserResult> resultAr) {
		initFile();
		PdfMaker.resultAr = resultAr;
		PdfMaker.riskModel = riskModel;
		try {
			Document document = new Document(PageSize.A4, 25, 25, 25, 25);
			PdfWriter.getInstance(document, new FileOutputStream(fILE));
			document.open();
			addTitlePage(document);
			addResult(document);
			document.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * this method creates the title of the result.
	 * @param document document to write the title in
	 * @throws DocumentException DocumentException
	 */
//	TODO Klaus R.: wann koennte die DocumentException rein theoretisch geworfen werden?
	private static void addTitlePage(final Document document) throws DocumentException {
		Paragraph title = new Paragraph();
		title.add(new Paragraph("Riskfinder Security Analysis Report", font1));
		addEmptyLine(title, 1);
		title.add(new Paragraph("Report generated on: " + new Date(), font3));
		addEmptyLine(title, 2);
		title.add(new Paragraph(separator));
		title.add(new Paragraph("Parameters:", font3));
		title.add(new Paragraph("Append: " + CheckController.getAppend()));
		title.add(new Paragraph("Synonyms: " + CheckController.getSynonyms()));
		title.add(new Paragraph("Stopwords: "
				+ CheckController.getStopwords().getPath()));
		title.add(new Paragraph("Ontology: "
				+ CheckController.getOntology().getPath()));
		title.add(new Paragraph("Diagram: "
				+ CheckController.getCurrentModel().getURI().toFileString()));

		title.add(new Paragraph(separator));
		title.add(new Paragraph("Entities checked: " + resultAr.size()));
		title.add(new Paragraph("Entities found: " + riskModel.size()));
		com.itextpdf.text.List list = new com.itextpdf.text.List(false);
		list.setIndentationLeft(15);
		Integer entry = 0;
		for (RiskActivity riskAktivity : riskModel) {
			list.add(new ListItem(new Chunk(riskAktivity.getName() + " (" + riskAktivity.getScore()
					+ ")").setLocalGoto(entry.toString())));
			entry++;
		}
		title.add(list);
		document.add(title);
		document.newPage();
	}

	/**
	 * this method creates the result entries.
	 * @param document document to write the result in
	 * @throws DocumentException DocumentException
	 */
//	TODO Klaus R.: wann koennte die DocumentException rein theoretisch geworfen werden?
	private static void addResult(final Document document) throws DocumentException {
		Paragraph result = new Paragraph();
		result.add(new Paragraph("Result", font1));
		Integer entry = 0;
		for (RiskActivity riskActivity : riskModel) {
			com.itextpdf.text.List textList = new com.itextpdf.text.List(false);
			textList.setIndentationLeft(30);
			addEmptyLine(result, 1);
			result.add(new Paragraph(new Chunk("[" + riskActivity.getName() + "]", font2)
					.setLocalDestination(entry.toString())));
			Paragraph newPara = new Paragraph("Relevant words: "
					+ riskActivity.getRelWords().get(0).getTreeSet());
			newPara.setIndentationLeft(15);
			result.add(newPara);
			entry++;
			for (RiskPattern riskPattern : riskActivity.getRelWords().get(0).getPatterns()) {
				textList.add(new ListItem(riskPattern.getPatternString()));
			}
			result.add(textList);
		}
		document.add(result);
	}

	/**
	 * adds n empty lines to the paragraph where n is the given number.
	 * @param paragraph paragraph to add empty lines
	 * @param number amount of empty lines
	 */
	private static void addEmptyLine(final Paragraph paragraph, final int number) {
		for (int i = 0; i < number; i++) {
			paragraph.add(new Paragraph(" "));
		}
	}
	
	/**
	 * creates the folder where the pdf should be stored if this folder does'nt exist.
	 */
	private static void initFile() {
		File file = new File(fILEpATH);
		if (!file.exists()) {
			file.mkdir();
		}	
	}
}
