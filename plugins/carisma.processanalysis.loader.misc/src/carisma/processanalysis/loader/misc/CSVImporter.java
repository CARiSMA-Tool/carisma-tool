package carisma.processanalysis.loader.misc;

import java.io.IOException;

import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;
import carisma.processanalysis.textmodel.Text;
import carisma.processanalysis.textmodel.TextKind;

import com.csvreader.CsvReader;

// TODO: Sch�ner w�re es, von TextFileImporter abzuleiten, aber dann meckert checkstyle

/**
 * A class to read models from csv-files. Each line contains one activity per line: First the label, then 0 or more comments.
 * @author thumberg
 *
 */
public class CSVImporter extends FileImporter {
	/**
	 * The delimiter that seperates the columns in the file.
	 */
	private char delimiter;

	/**
	 * Coonstructor.
	 * @param filename The file that contains the model.
	 * @param delimiter The delimiter of the input file.
	 */
	public CSVImporter(final String filename, final char delimiter) {
		super(filename);
		this.delimiter = delimiter;
	}

	@Override
	public final ProcessDescription doImport() {
		// super. ...
		ProcessDescription ret = new TextFileImporter(this.getFilename()).doImport();
		System.out.println("CSV Import: Es wurden " + ret.getEntities().size()	+ " Entities importiert.");

		try {
			for (ProcessEntity curEntity : ret.getEntities()) {
				assert (curEntity.getTexts().size() == 0);
				Text oldText = curEntity.getTexts().get(0);
				curEntity.getTexts().clear();

				CsvReader csvReader = CsvReader.parse(oldText.getEntityText());
				csvReader.setDelimiter(this.delimiter);
				csvReader.readRecord();
				for (int colIdx = 0; colIdx < csvReader.getValues().length; colIdx++) {
					curEntity.addText(csvReader.getValues()[colIdx],
							colIdx == 0 ? TextKind.PROCESSNAME : TextKind.PROCESSCOMMENT);
				}
				assert (!csvReader.readRecord());
			}
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}

		return ret;
	}

}
