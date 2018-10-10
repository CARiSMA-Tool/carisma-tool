package carisma.check.riskfinder;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.TreeSet;

import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.io.OutputFormat;

import carisma.core.analysis.AnalysisHost;
import carisma.processanalysis.textmodel.ProcessEntity;
import carisma.processanalysis.textmodel.Word;

/**
 * The Class HTMLWriter.
 * @author dbuerger
 */
public class HTMLReport {

	/** The path. */
	private String path = "";

	/** The document. */
	private Document doc = null;

	/** The host. */
	private AnalysisHost host = null;

	/** The body element. */
	private Element body = null;
	
	private int yellowThreshold = 250;
	private int redThreshold = 500;
	private String redColor = "#FF0000";
	private String yellowColor = "#FFFF00";
	private String greenColor = "#00FF00";
	/**
	 * Instantiates a new hTML writer.
	 *
	 * @param path the path
	 * @param host the host
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public HTMLReport(final String path, final AnalysisHost host)
			throws IOException {
		this.host = host;
		this.path = path;
		checkPath(path);
		doc = DocumentHelper.createDocument();
			// add the doctype
		doc.addDocType("html", "-//W3C//DTD HTML 4.01 Transitional//EN",
				"http://www.w3.org/TR/html4/loose.dtd");
		Element html = doc.addElement("html");
		Element head = html.addElement("head");
			// add the charset
		head.addElement("meta")
			.addAttribute("http-equiv", "Content-type")
			.addAttribute("content", "text/html; charset=UTF-8");
		Element title = head.addElement("title");
		title.setText("Riskfinder Analysis Result");
		body = html.addElement("body");
	}

	/**
	 * Appends an entry in the html file.
	 *
	 * @param curEntity the cur entity
	 * @param result the result
	 */
	public final void appendEntry(final ProcessEntity curEntity,
			final AnalyserResult result) {
		String name = "";
		int activityScore = 0;
		ArrayList<String> comments = null;
		name = getNameFromEntity(curEntity);
		activityScore = (int) result.getScore_method1(3);
		comments = getCommentsFromEntity(curEntity);
			// create the header
		Element h2 = body.addElement("h2");
		if (activityScore >= redThreshold) {
			h2.setText(name + " (" + activityScore + ")");
			h2.addAttribute("style", "background-color:" + redColor);
//			Element font = h2.addElement("font");
//			font.addAttribute("color", redColor);
//			font.setText(name + " (" + activityScore + ")");
		} else if (activityScore >= yellowThreshold) {
			h2.setText(name + " (" + activityScore + ")");
			h2.addAttribute("style", "background-color:" + yellowColor);
//			Element font = h2.addElement("font");
//			font.addAttribute("color", yellowColor);
//			font.setText(name + " (" + activityScore + ")");
		} else {
			h2.setText(name + " (" + activityScore + ")");
			h2.addAttribute("style", "background-color:" + greenColor);
		}
			// create the comments
		if (comments.size() != 0) {
			createComments(comments, h2);
		}
			// create the relevant words
		TreeSet<Word> relevantWords = result
				.getAllRelevantWordsSorted();
		if (relevantWords.size() > 0) {
			createRelevantWords(relevantWords);
		}
			// create the patterns
		createRelevantPattern(result);
	}

	/**
	 * Stores the html file. Encoding is 'UTF-8'.
	 */
	public final void finish() {
		try {
			OutputStream out = new FileOutputStream(path);
		    OutputFormat format = OutputFormat.createPrettyPrint();
		    format.setEncoding("UTF-8");
		    format.setXHTML(true);
		    org.dom4j.io.HTMLWriter writer = new org.dom4j.io.HTMLWriter(out,
		    		format);
		    writer.write(doc);
		    writer.flush();
		    writer.close();
		} catch (IOException e) {
			host.displayError("Failed to store the HTML document. "
					+ e.getMessage());
		}
	}

	/**
	 * Creates the relevant pattern.
	 *
	 * @param result the result
	 */
	private void createRelevantPattern(final AnalyserResult result) {
		Element h4 = body.addElement("h4");
		h4.setText("Relevante Pattern");
			// create a new table and the headers
		Element table = body.addElement("table");
		Element tr = table.addElement("tr");
		Element th1 = tr.addElement("th");
		Element th2 = tr.addElement("th");
		Element th3 = tr.addElement("th");
		th1.setText("ID");
		th2.setText("Beschreibung");
		th3.setText("Wert");
			// create several entries
		for (AnalyserResultEntry entry : result.asSortedList()) {
			tr = table.addElement("tr");
				// create the first column entry
			Element td1 = tr.addElement("td");
			td1.setText(entry.getPattern().getName().getEntityText());
				// create the second column entry
			Element td2 = tr.addElement("td");
			td2.setText(entry.getPattern().getTitle().getEntityText());
				// create the third column entry
			Element td3 = tr.addElement("td");
			td3.setText("" + entry.getScore());
		}
	}

	/**
	 * Creates the relevant words.
	 *
	 * @param relevantWords the relevant words
	 */
	private void createRelevantWords(final TreeSet<Word> relevantWords) {
		Element h4 = body.addElement("h4");
		h4.setText("Relevante Worte");
		for (Word word : relevantWords) {
			Element a = body.addElement("a");
			a.setText(word.getContent());
				// set the score italic
			Element i = a.addElement("i");
			i.setText("" + word.getScore() + "; ");
		}

	}

	/**
	 * Creates the comments.
	 *
	 * @param comments a list of comments
	 */
//	private void createComments(final ArrayList<String> comments) {
//		Element h4 = body.addElement("h4");
//		h4.setText("Kommentar");
//			// create an unordered list and add the comments
//		Element unorderedList = body.addElement("ul");
//		for (String comment : comments) {
//			Element listElement = unorderedList.addElement("li");
//			listElement.setText(comment);
//		}
//	}
	
	private void createComments(final ArrayList<String> comments,
			final Element h2) {
			// create an unordered list and add the comments
		StringBuffer buffer = new StringBuffer();
		for (String comment : comments) {
			buffer.append(comment + "; ");
		}
		Element comment = h2.addElement("font");
		comment.addAttribute("size", "-0.5");
		comment.setText(buffer.toString());
	}

	/**
	 * Gets the comments from an entity.
	 *
	 * @param curEntity the current entity
	 * @return the comments from entity
	 */
	private ArrayList<String> getCommentsFromEntity(final ProcessEntity curEntity) {
		ArrayList<String> comments = new ArrayList<String>();
		String[] entities = curEntity.getAllEntityTexts()
				.split("(Name: \")|(\" Comment: \")|(\" )");
		for (int i = 2; i < entities.length; i++) {
			comments.add(entities[i]);
		}
		return comments;
	}

	/**
	 * Gets the name from an entity.
	 *
	 * @param curEntity the current entity
	 * @return the name from entity
	 */
	private String getNameFromEntity(final ProcessEntity curEntity) {
		String name = "";
		String[] entities = curEntity.getAllEntityTexts()
				.split("(Name: \")|(\" Comment: \")|(\" )");
		if (entities.length > 0) {
			name = entities[1];
		}
		return name;
	}

	/**
	 * Check if the file in the given path exists.
	 * If the file does not exist, a new file will be created.
	 *
	 * @param path the path
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private void checkPath(final String path) throws IOException {
		File file = new File(path);
		if (!file.exists()) {
			if (!file.createNewFile()) {
				host.displayError("(HTMLWriter) Failed to create a new file.");
			}
		}
	}
}
