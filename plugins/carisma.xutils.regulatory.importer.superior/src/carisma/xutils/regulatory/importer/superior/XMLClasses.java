package carisma.xutils.regulatory.importer.superior;

//import java.io.File;
//import java.io.FileNotFoundException;
//import java.io.FileOutputStream;
//import java.io.OutputStreamWriter;
//import java.io.UnsupportedEncodingException;
//import java.io.Writer;
import java.util.ArrayList;

/**
 * this class is a wrapper class for the XML-Nodes
 * 
 * @author dbuerger
 *
 */
public class XMLClasses {
		/** the file to store the xml-file */ 
	
	@SuppressWarnings("unused") // useful for xstream, without error while encoding
	private final String fileName = "resources" + 
			System.getProperty("file.separator") + "ElementEntry.xml";
	
	/*
	 * auskommentiert, da nur zum speichern der xml-Datei noetig, dbuerger
	 */
//	
//
//	/**
//	 * for test only
//	 */
//	public static void main(String[] args) {
//		XMLClasses stream = new XMLClasses();
//		stream.store();
//	}
//
//	/**
//	 * stores the mentioned Strings in the related ElementEntries
//	 */
//	public void store(){
//		XStream xs = new XStream();
//		Entry entry = new Entry();
//			// create the activities 
//		Activity activity = new Activity();
//		activity.add("Administration", "Administration");
//		activity.add("Archivierung", "Archivierung");
//		entry.add(activity);
//		
//			// create the objects
//		Artifact object = new Artifact();
//		object.add("Anwendung", "Anwendung");
//		object.add("Netz", "Netz");
//		object.add("VPN", "VPN");
//		object.add("Software", "Software");
//		object.add("Gebäude", "Gebäude");
//		object.add("Kabelverbindung", "Kabelverbindung");
//		entry.add(object);
//		
//			// create the processes
//		Process process = new Process();
//		process.add("Notfallmanagment", "Notfallmanagment");
//		process.add("Risikomanagment", "Risikomanagment");
//		entry.add(process);
//		
//			// create the properties
//		Property property = new Property();
//		property.add("Funktionstrennung", "Funktionstrennung");
//		property.add("Vertraulichkeit", "Vertraulichkeit");
//		entry.add(property);
//		
//			// create the roles
//		Role role = new Role();
//		role.add("Wartungspersonal", "Wartungspersonal");
//		role.add("Telearbeiter", "Telearbeiter");
//		entry.add(role);
//		
////		System.out.println(xs.toXML(entry));
//		
//		
//		try {
//				// store the entries, encoding 'UTF-8'
//			FileOutputStream outputStream = new FileOutputStream(new File(fileName));
//			Writer writer = new OutputStreamWriter(outputStream, "UTF-8");
//			xs.toXML(entry, writer);
//		} catch (UnsupportedEncodingException e) {
//			e.printStackTrace();
//		} catch (FileNotFoundException e) {
//			System.err.println("File was not found!");
//		}
//
//	}
//	
	/**
	 * a class to represent the activity within the xml structure
	 *
	 */
	public class Activity implements iEntry{
		private ArrayList<String[]> list = null;
		private String name = "Activity";
		
		public Activity(){
			list = new ArrayList<String[]>();
		}
		
		@Override
		public String getName(){
			return this.name;
		}
		
		@Override
		public void add(String name, String caption){
			this.list.add(new String[] {name, caption} );
		}
		@Override
		public ArrayList<String[]> getList(){
			return this.list;
		}		
	}

	/**
	 * a class to represent the object within the xml structure
	 *
	 */
	public class Artifact implements iEntry{
		private ArrayList<String[]> list = null;
		private String name = "Artifact";
		
		public Artifact(){
			list = new ArrayList<String[]>();
		}
		
		@Override
		public String getName(){
			return this.name;
		}
		
		@Override
		public void add(String name, String caption){
			this.list.add(new String[] {name, caption} );
		}
		@Override
		public ArrayList<String[]> getList(){
			return this.list;
		}		
	}

	/**
	 * a class to represent the process within the xml structure
	 *
	 */
	public class Process implements iEntry{
		private ArrayList<String[]> list = null;
		private String name = "Process";
		
		public Process(){
			list = new ArrayList<String[]>();
		}
		
		@Override
		public String getName(){
			return this.name;
		}
		
		@Override
		public void add(String name, String caption){
			this.list.add(new String[] {name, caption} );
		}
		@Override
		public ArrayList<String[]> getList(){
			return this.list;
		}		
	}

	/**
	 * a class to represent the properties within the xml structure
	 *
	 */
	public class Property implements iEntry{
		private ArrayList<String[]> list = null;
		private String name = "Property";
		
		public Property(){
			list = new ArrayList<String[]>();
		}
		
		@Override
		public String getName(){
			return this.name;
		}
		
		@Override
		public void add(String name, String caption){
			this.list.add(new String[] {name, caption} );
		}
		@Override
		public ArrayList<String[]> getList(){
			return this.list;
		}		
	}
	
	/**
	 * a class to represent the role within the xml structure
	 *
	 */
	public class Role implements iEntry{
		private ArrayList<String[]> list = null;
		private String name = "Role";
		
		public Role(){
			this.list = new ArrayList<String[]>();
		}
		
		@Override
		public String getName(){ return this.name; }
		
		@Override
		public void add(String name, String caption){
			this.list.add(new String[] {name, caption} );
		}
		@Override
		public ArrayList<String[]> getList(){
			return this.list;
		}		
	}
	
	/**
	 * class to store all Entrys in one xml-file
	 *
	 */
	public class Entry{
		private ArrayList<iEntry> list = null;
		public Entry(){ list = new ArrayList<iEntry>(); }
		public void add(iEntry entry){ this.list.add(entry); }
		public ArrayList<iEntry> getEntrys(){ return this.list; }
	}
	
	/**
	 * the interface to iterate over all childs
	 */
	public interface iEntry {
			/** returns the name of the Entry */
		public String getName();
			/** returns the list of captions */
		public ArrayList<String[]> getList();
			/** adds a new caption to the list */
		public void add(String name, String caption);
	}

}

