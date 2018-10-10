package carisma.processanalysis.textmodel;

import java.util.ArrayList;

import org.eclipse.emf.ecore.EObject;


/**
 * A ModelEntity contains the text of an activity, and defines the type of the
 * text.
 * 
 * @author Christian Wessel
 * 
 */
public class ProcessEntity {
	
	/**
	 * Contains the whole text of an activity. This may be the label of the
	 * activity or any attached text from comments of the activity.
	 */
	private ArrayList<Text> texts = new ArrayList<Text>();
	
	private String type = null;
	private String id = null;
	private EObject object = null;
	private String name = null;

	/**
	 * The default constructor.
	 */
	public ProcessEntity(final String type, final String id, final EObject object) {
		this.type = type;
		this.id = id;
		this.object = object;
	}

	public ProcessEntity(final String type, final String id, final EObject object, final String name) {
		this(type, id, object);
		this.name = name;
		this.addText(name, TextKind.PROCESSNAME);
	}
	
	public String getType() {
		return type;
	}

	public String getId() {
		return id;
	}

	public EObject getObject() {
		return object;
	}

	public String getName() {
		return name;
	}

	/**
	 * Adds one text to the activity.
	 * @param text The text to be added.
	 * @param kind The type of the nex text.
	 */
	public final void addText(final String text, final TextKind kind) {
		if (kind == TextKind.PROCESSNAME) {
			this.name = text;
		}
		Text t = new Text(text, kind);
		this.texts.add(t);
	}

	/**
	 * Gets all stored texts.
	 * @return A list of stored texts.
	 */
	public final ArrayList<Text> getTexts() {
		return texts;
	}
	

	/**
	 * Fuer Report
	 * @return
	 */
	public final String getAllEntityTexts() {
		String ret = "";
		
		for(Text curText : this.texts){
			ret += curText.toString() + " ";
		}
		
		return ret;
	}


}
