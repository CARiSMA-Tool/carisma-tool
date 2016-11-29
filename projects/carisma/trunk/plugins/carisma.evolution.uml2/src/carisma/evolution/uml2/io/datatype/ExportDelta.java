package carisma.evolution.uml2.io.datatype;

import java.util.ArrayList;
import java.util.List;

public class ExportDelta {

	private List<ExportDeltaElement> deltaElements;
	private List<String> usedChanges;
	
	public ExportDelta(List<String> newusedChanges, List<ExportDeltaElement> newContent) {
		this.usedChanges = new ArrayList<>();
		this.deltaElements = new ArrayList<>();
		if (newusedChanges != null) {
			this.usedChanges.addAll(newusedChanges);
		}
		if (newContent != null) { 
			this.deltaElements.addAll(newContent);
		}
	}
	
	public List<ExportDeltaElement> getContent() {
		return this.deltaElements;
	}
	
	public void addExpElementDescription(ExportDeltaElement element) {
		this.deltaElements.add(element);
	}
	
	/** Returns a List with the ID's of the Changes used in this Delta.
	 * @return List of Strings Change-ID(Alt#)
	 */
	public final List<String> getUsedChangesID() {
		return this.usedChanges;
	}
}
