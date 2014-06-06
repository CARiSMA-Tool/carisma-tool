package carisma.evolution.uml2.io.datatype;

import java.util.ArrayList;
import java.util.List;

public class ExportDelta {

	private List<ExportDeltaElement> deltaElements;
	private List<String> usedChanges;
	
	public ExportDelta(List<String> newusedChanges, List<ExportDeltaElement> newContent) {
		usedChanges = new ArrayList<String>();
		deltaElements = new ArrayList<ExportDeltaElement>();
		if (newusedChanges != null) {
			usedChanges.addAll(newusedChanges);
		}
		if (newContent != null) { 
			deltaElements.addAll(newContent);
		}
	}
	
	public List<ExportDeltaElement> getContent() {
		return deltaElements;
	}
	
	public void addExpElementDescription(ExportDeltaElement element) {
		deltaElements.add(element);
	}
	
	/** Returns a List with the ID's of the Changes used in this Delta.
	 * @return List of Strings Change-ID(Alt#)
	 */
	public final List<String> getUsedChangesID() {
		return usedChanges;
	}
}
