package carisma.processanalysis.texttools;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import com.csvreader.CsvReader;
import com.csvreader.CsvWriter;

public class DomainSpecificKeywordTaxonomy extends TreeMap<String, KeywordRelations> {// ggf. kann man den Namen noch aendern

	// TH: Besser Maps dynamisch erzeugen, nur Nodes speichern? -> erstmal ja
//	
//	/**
//	 * Verweis von einem Node der Taxonomie auf alle(!) Nachfolger.
//	 * Richtung: allgemein -> spezifisch
//	 */
//	TreeMap<String, TreeSet<String>> mapForward;
//	
//	/**
//	 * Umgekehrt zu mapForward.
//	 * Verweis von einem Begriff auf alle(!) Vorgaenger.
//	 * Richtung: spezifisch -> allgemein
//	 */
//	TreeMap<String, TreeSet<String>> mapBackward;
	
	TreeMap<String, TreeSet<String>> antiSynonyms;
	
	
	public DomainSpecificKeywordTaxonomy() {
		// super();
	}


	/** 
	 * @param parentNode
	 * @param childNotes
	 */
	public void addSet(KeywordRelations newEntry){
		if(this.containsKey(newEntry.getKeyword())){
			this.get(newEntry.getKeyword()).getChildren().addAll(newEntry.getChildren());			
		}
		else{
			this.put(newEntry.getKeyword(), newEntry);			
		}
		
		// neues keyword bei direkten Vorgaengern eintragen:
		for(String curPre : this.getAllDirectPredecessors(newEntry.getKeyword())){
			this.get(curPre).getChildren().add(newEntry.getKeyword());
		}
		
		// neues keyword bei direkten Nachfolgern eintragen:
		for(String curChild : newEntry.getChildren()){	
			// reicht das so? oder sicherstellen, dass Eintrag wirklich vorhanden? -> anscheinend nicht :(
			if(!this.containsKey(curChild))		
				this.put(curChild, new KeywordRelations(curChild));
			this.get(curChild).getParents().add(newEntry.getKeyword());
		}		
		
		// Synonyme eintragen:
		this.get(newEntry.getKeyword()).getSynomyms().addAll(newEntry.getSynomyms());
		// neues keyword bei Synonymen eintragen:
		for(String curSynonym : newEntry.getSynomyms()){
			if(!this.containsKey(curSynonym))		
				this.put(curSynonym, new KeywordRelations(curSynonym));
			this.get(curSynonym).getSynomyms().add(newEntry.getKeyword());			
			this.get(curSynonym).getChildren().addAll(this.get(newEntry.getKeyword()).getChildren());
//			for(String curChild : this.get(curSynonym).getChildren())
//				this.get(curChild).getParents().add(curSynonym);
			this.get(curSynonym).getParents().addAll(this.get(newEntry.getKeyword()).getParents());			
//			for(String curParent: this.get(curSynonym).getParents())
//				this.get(curParent).getChildren().add(curSynonym);
		}
	}

	// rekursiv, nicht bloss direkte Nachfolger
	// Liefert keine Synonyme auf gleicher Ebene wie keyword!
	public TreeSet<String> getAllSuccessors(String keyword){
		TreeSet<String> ret = new TreeSet<String>();
		
		if(!this.containsKey(keyword))
			return ret;
		
		// Abbruchkriterium: -> implizit in Schleife unten
//		if(this.get(keyword).getChildren().size() == 0)
//			return ret;
		
		// Rekursion:
		for(String curChild: this.get(keyword).getChildren()){
			ret.add(curChild);
			ret.addAll(this.getAllSuccessors(curChild));
			for(String curSynonym : this.get(curChild).getSynomyms()){
				ret.add(curSynonym);
				if(ret.contains(keyword))
					continue;
				ret.addAll(this.getAllSuccessors(curSynonym));
			}
		}
		
		// !! so nicht, führt zu Endlosrekursion! -> in obere Schleife verschoben
//		for(String curSynonym : this.get(keyword).getSynomyms()){
//			ret.add(curSynonym);
//			ret.addAll(this.getAllSuccessors(curSynonym));
//		}
		
		return ret;
	}

	// TODO TH: Synonyme beachten!
	public TreeSet<String> getAllPredecessors(String keyword){
		TreeSet<String> ret = new TreeSet<String>();
		
		if(!this.containsKey(keyword))
			return ret;
		
		// Abbruchkriterium:
		if(this.get(keyword).getParents().size() == 0 && this.get(keyword).getSynomyms().size() == 0)
			return ret;
		
		// TODO: das wird wohl noch nicht ausreichen, da in Synonyme nicht rekursiv verzweigt wird
		ret.addAll(this.get(keyword).getSynomyms());		
		
		// Rekursion:
		for(String curParent: this.get(keyword).getParents()){
			ret.add(curParent);
			ret.addAll(this.getAllPredecessors(curParent));
			for(String curSynonym : this.get(curParent).getSynomyms()){
				ret.add(curSynonym);
				if(ret.contains(keyword))
					continue;
				ret.addAll(this.getAllPredecessors(curSynonym));
			}

		}


		// s. vorherige Funktion
//		for(String curSynonym : this.get(keyword).getSynomyms()){
//			ret.add(curSynonym);
//			ret.addAll(this.getAllPredecessors(curSynonym));
//		}

		return ret;
	}

	public TreeSet<String> getAllDirectPredecessors(String keyword){
		TreeSet<String> ret = new TreeSet<String>();
		
		if(!this.containsKey(keyword))
			return ret;
		
		for(String curWord : this.keySet()){
			if(this.get(curWord).getChildren().contains(keyword))
				ret.add(curWord);
		}
		
		// hier auch Synonyme? -> geht das so?
//		for(String curSynonym : this.get(keyword).getSynomyms()){
//			ret.add(curSynonym);
//		}
		
		return ret;
	}

	
	// Import aus CSV
	public boolean loadFromCSV(String filename, boolean keepOld){
		if(!keepOld)
			this.clear();
		
		try {
//			CsvReader reader = new CsvReader(filename);
//			CsvReader reader=new CsvReader(new StreamReader(new FileInputStream("d:\\a.csv"), "UTF-8"));
			CsvReader reader=new CsvReader(new InputStreamReader(new FileInputStream(filename), "UTF-8"));
//			CsvReader reader=new CsvReader(new FileInputStream("d:\\a.csv"));
			while(reader.readRecord()){
				String[] values = reader.getValues();
				LinkedList<String> stringList = new LinkedList<String>(Arrays.asList(values)); // geht das so?
				String id = stringList.removeFirst();
				KeywordRelations newRelations = new KeywordRelations(id, stringList);
//				this.put(id, newRelations);
				this.addSet(newRelations);
			}
		} catch (Exception e) {
//			e.printStackTrace();
			System.out.println("Datei " + filename + " existiert nicht oder kann nicht gelesen werden. Taxonomie bleibt leer.");
			return false;
		}
		
		return true;
	}

	public boolean loadSynonymsFromCSV(String filename){		
		try {
			CsvReader reader=new CsvReader(new InputStreamReader(new FileInputStream(filename), "UTF-8"));
			while(reader.readRecord()){
				String[] values = reader.getValues();
				LinkedList<String> stringList = new LinkedList<String>(Arrays.asList(values)); // geht das so?
				String id = stringList.removeFirst();
				
				KeywordRelations newRelations = KeywordRelations.getSynonymsRelation(id, stringList);
//				this.put(id, newRelations);
				this.addSet(newRelations);
			}
		} catch (Exception e) {
			e.printStackTrace();
//			System.out.println("Datei " + filename + " existiert nicht oder kann nicht gelesen werden. Taxonomie bleibt leer.");
			return false;
		}
		
		return true;
	}
	
	/**
	 * Anti-Synonyme laden.
	 * Etwas improvisiert, im Gegensatz zu den richtigen Synonymen direkt als Map, um späteres Filtern zu erleichtern.
	 * @param filename
	 * @return
	 */
	public boolean loadAntiSynonymsFromCSV(String filename){
		this.antiSynonyms = new TreeMap<String, TreeSet<String>>();
		try {
			CsvReader reader=new CsvReader(new InputStreamReader(new FileInputStream(filename), "UTF-8"));
			while(reader.readRecord()){
				String[] values = reader.getValues();				
				
				LinkedList<String> stringList = new LinkedList<String>(Arrays.asList(values)); // geht das so?
				String key = stringList.removeFirst();
				
				// Existenz des Keys sicherstellen:
				if(!this.antiSynonyms.keySet().contains(key))
					this.antiSynonyms.put(key, new TreeSet<String>());
				
				// anti-synonym hinzufügen, doppelte Einträge werden hoffentlich vom Treeset abgefangen
				this.antiSynonyms.get(key).addAll(stringList);
				
//				this.put(id, newRelations);
//				this.addSet(newRelations);
			}
		} catch (Exception e) {
			e.printStackTrace();
//			System.out.println("Datei " + filename + " existiert nicht oder kann nicht gelesen werden. Taxonomie bleibt leer.");
			return false;
		}
		
		return true;
	}
	
	/**
	 * Gibt an, ob eine bestimmte Erweiterung auf Grund der Antisynonym-Beziehungen verboten ist.
	 * @param base
	 * @param extension
	 * @return
	 */
	public boolean forbidsExtension(String base, String extension){
		// wenn base nicht vorhanden, kann auch die Erweiterung nicht verboten sein
		if(!this.antiSynonyms.containsKey(base))
			return false;
		
		// base hat die abgefragte extension explizit als AS
		if(this.antiSynonyms.get(base).contains(extension))
			return true;
		
		// base hat ASs, aber nicht die abgefragte extension
		return false;
	}
	
	// Export nach CSV
	public boolean exportToCSV(String filename) {
		CsvWriter writer;
		try {
			writer = new CsvWriter(new OutputStreamWriter(new FileOutputStream(
					filename), "UTF-8"), ',');

			for (KeywordRelations curRelation : this.values()) {
				writer.write(curRelation.getKeyword());
				for (String curChild : curRelation.getChildren())
					writer.write(curChild);
				writer.endRecord();

			}
			writer.close();
			return true;
		} catch (UnsupportedEncodingException e1) {
			e1.printStackTrace();
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
		return false;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		DomainSpecificKeywordTaxonomy dskt = new DomainSpecificKeywordTaxonomy();
		
		dskt.addSet(new KeywordRelations("D", "H"));
		dskt.addSet(new KeywordRelations("E", "F", "G"));
		dskt.addSet(new KeywordRelations("F", "H"));
		dskt.addSet(new KeywordRelations("A", "B"));
		dskt.addSet(new KeywordRelations("A", "C"));
		dskt.addSet(new KeywordRelations("C", "D", "E"));
		dskt.addSet(new KeywordRelations("I", "J", "K"));
//		dskt.addSet(new KeywordRelations("H", "B")); // warum kein Overflow?

		dskt.addSet(KeywordRelations.getSynonymsRelation("B", "L"));
		dskt.addSet(KeywordRelations.getSynonymsRelation("C", "M"));
		dskt.addSet(new KeywordRelations("M", "G"));

		for(String curWord : dskt.keySet()){
			System.out.println(curWord + " -> " + dskt.getAllSuccessors(curWord));
			System.out.println(curWord + " <- " + dskt.getAllPredecessors(curWord));
		}
		for(KeywordRelations curRelation : dskt.values())
			System.out.println(curRelation);
		dskt.exportToCSV("testfileOut.csv");

		// neu einlesen
		dskt.loadFromCSV("testfileOut.csv", false);
		dskt.loadSynonymsFromCSV("testSynonyms.csv");

		for(String curWord : dskt.keySet()){
			System.out.println(curWord + " -> " + dskt.getAllSuccessors(curWord));
			System.out.println(curWord + " <- " + dskt.getAllPredecessors(curWord));
		}
		for(KeywordRelations curRelation : dskt.values())
			System.out.println(curRelation);
		
	}

}
