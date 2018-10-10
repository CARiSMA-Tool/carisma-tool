package carisma.check.riskfinder;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;

/**
 * This class stores the XMLResult in a file.
 *
 * @author dbuerger
 */
public final class XMLReport {

	/**
	 * Do not instantiate a new xML logger.
	 */
	private XMLReport(){
	}

	/**
	 * Stores the given {@link XMLResult} in the given file.
	 * The encoding is set to 'utf-8'.
	 *
	 * @param result the result
	 * @param xmlResultFile the xml result file
	 */
	public static void createReport(final XMLResult result, final File xmlResultFile) {
		try {
			FileOutputStream out = new FileOutputStream(xmlResultFile);
			OutputStreamWriter writer = new OutputStreamWriter(out, "UTF-8");
//			 writer.write(result.getXmlResultComplete());
			writer.write(result.getXmlSimpleResult());
			writer.close();
		} catch (IOException e) {
			System.err.println("Konnte Datei nicht erstellen");
		}
	}
}
