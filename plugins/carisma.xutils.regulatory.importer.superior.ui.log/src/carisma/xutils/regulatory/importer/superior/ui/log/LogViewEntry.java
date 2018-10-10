package carisma.xutils.regulatory.importer.superior.ui.log;


import java.text.SimpleDateFormat;
import java.util.Date;

public class LogViewEntry {
	private String timestamp;
	private String resource;
	private String message;
	private int level;
	
	public LogViewEntry(String importerName, String message, int level) {
		SimpleDateFormat dateFormat = new SimpleDateFormat("HH:mm:ss.S");
		Date date = new Date();
		this.timestamp = dateFormat.format(date);
		this.resource = importerName;
		this.message = message;
		this.level = level;
	}
	
	public LogViewEntry(String importerName, String message) {
		this(importerName, message, 0);
	}

	
	public String getTimestamp() {
		return this.timestamp;
	}
	
	public String getResource() {
		return this.resource;
	}
	
	public String getMessage() {
		return this.message;
	}

	public int getLevel() {
		return level;
	}

	public void setLevel(int level) {
		this.level = level;
	}
}
