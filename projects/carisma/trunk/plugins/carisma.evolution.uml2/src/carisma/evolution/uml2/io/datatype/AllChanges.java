package carisma.evolution.uml2.io.datatype;

import java.util.ArrayList;
import java.util.List;

/** @deprecated Use a list with OChange elements instead 
 * 
 * @author bberghoff
 *
 */
public class AllChanges {
	
	private List<ExportChange> changes;
	
	public AllChanges(){
		this.changes = new ArrayList<ExportChange>();
	}
	
	
	public void addChange(ExportChange change){
		this.changes.add(change);
	}
	
	public void addChanges(List<ExportChange> changes){
		this.changes.addAll(changes);
	}
	
	public ExportChange getChange(int i){
		return this.changes.get(i);
	}
	
	public List<ExportChange> getChanges(){
		return this.changes;
	}
	

}
