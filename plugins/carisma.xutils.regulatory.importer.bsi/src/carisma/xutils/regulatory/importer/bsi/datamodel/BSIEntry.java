package carisma.xutils.regulatory.importer.bsi.datamodel;

import java.util.LinkedList;

public class BSIEntry {
	public enum EntryStatus {
		valid, deprecated, unknown
	}
	
	public enum EntryType {
		element,
		threat,
		measure,
		unknown
	}

	private String title;
	private String id;
	private EntryStatus status;
	private String text;
	private String url;
	private LinkedList<String> refs; // Liste von Verweisen auf andere Entries, als String der Form "B_5.3"

	public BSIEntry(final String title, final String id, final EntryStatus status, final String text,
			final String uRL) {
		super();
		this.title = title;
		this.id = id;
		this.status = status;
		this.url = uRL;
		this.text = text;
		this.refs = new LinkedList<String>();
	}

	public final String getTitle() {
		return title;
	}

	public final void setTitle(final String title) {
		this.title = title;
	}

	public final String getId() {
		return id;
	}

	public final void setId(final String id) {
		this.id = id;
	}

	public final EntryStatus getStatus() {
		return status;
	}

	public final void setStatus(final EntryStatus status) {
		this.status = status;
	}

	public final String getText() {
		return this.text;
	}

	public final void setText(final String text) {
		this.text = text;
	}

	public final void appendText(final String text) {
		this.text += text;
	}

	public final String getURL() {
		return url;
	}

	public final void setURL(final String uRL) {
		url = uRL;
	}
	
	public final LinkedList<String> getRefs(){
		return this.refs;
	}
	
	public static final EntryType getTypeByIDPrefix(String prefix) {
		char firstChar = prefix.charAt(0);
		switch (firstChar) {
		case 'B':
			return EntryType.element;
		case 'G':
			return EntryType.threat;
		case 'M':
			return EntryType.measure;
		default:
			return EntryType.unknown;
		}	
	}

	public final EntryType getTypeByIDPrefix() {
		return BSIEntry.getTypeByIDPrefix(this.id);				
	}

}
