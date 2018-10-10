package carisma.processanalysis.loader.misc;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import org.eclipse.emf.ecore.EObject;

import carisma.processanalysis.textmodel.ProcessDescription;
import carisma.processanalysis.textmodel.ProcessEntity;


/**
 * Importer for a textfile that contains one activity label per line (and nothing else).
 * @author thumberg
 *
 */
public class TextFileImporter extends FileImporter {

	/**
	 * Constructor.
	 * @param filename The file that is used for import.
	 */
	public TextFileImporter(final String filename) {
		super(filename);
	}
	
	@Override
	public final ProcessDescription doImport() {
		ProcessDescription processDescription = new ProcessDescription();
		assert (!this.getFilename().equals(""));

		InputStreamReader reader = null;
		BufferedReader br = null;
		String curLine = null;

		try {
			reader = new InputStreamReader(new FileInputStream(this.getFilename()), "UTF-8");
			br = new BufferedReader(reader);

			while ((curLine = br.readLine()) != null) {
				//TODO: Type, id und object noch sinnvoll ergaenzen
				if(curLine.startsWith("#"))
					continue;
				EObject eObj = null;
				processDescription.addEntity(new ProcessEntity("", "", eObj, curLine));
			}
			
			br.close();
			reader.close();
		} catch (IOException e1) {
			e1.printStackTrace();
			return null;
		} finally {
			try {
				br.close();
				reader.close();
			} catch (IOException e) {				
				e.printStackTrace();
			}			
		}		
		
//		try {
//			BufferedReader in = new BufferedReader(new FileReader(this.filename));
//			String curLine = null;
//			while ((curLine = in.readLine()) != null) {
//				System.out.println("Gelesene Zeile: " + curLine);
//				ModelEntity newEntity=new ModelEntity(curLine,TextKind.Label);
//				ret.addEntity(newEntity);
//			}
//		} catch (IOException e) {
//			e.printStackTrace();
//			return null;
//		}
		
		//TODO: Modell in die Registry speichern
		//try {
		//	host.putToRegister(ProcessDescription.CARISMA_REGISTRY_KEY, processDescription);
		//} catch (RegisterInUseException e) {
		//	host.displayError("There is already a model to be analysed. Have you used two model importer plugins?");
		//	return false;
		//}
		
		return processDescription;
	}
}
