package carisma.evolution;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class DeltaList {
	private int highestChangeCountAllTime = 0;
	
	private int highestChangeCountNow = 0;
	
	private List<Delta> allDeltas = null;
	
	private List<Delta> unsuccessfulDeltas = null;
	
	public DeltaList(final List<Delta> containedDeltas) {
		init();
		allDeltas.addAll(containedDeltas);
		highestChangeCountAllTime = calculateHighestChangeCount();
		highestChangeCountNow = highestChangeCountAllTime;
	}
	
	public void init() {
		if (allDeltas == null) {
			allDeltas = new ArrayList<Delta>();			
		} else {
			allDeltas.clear();
		}
		if (unsuccessfulDeltas == null) {
			unsuccessfulDeltas = new ArrayList<Delta>();
		} else {
			unsuccessfulDeltas.clear();
		}
		
	}
	
	public int getHighestChangeCountAllTime() {
		return highestChangeCountAllTime;
	}
	
	public int getHighestChangeCountNow() {
		return highestChangeCountNow;
	}
	
	private int calculateHighestChangeCount() {
		int maxChangeCount = 0;
		
		for (Delta d : allDeltas) {
			if (!(unsuccessfulDeltas.contains(d))) {
				maxChangeCount = Math.max(maxChangeCount, d.getNumberOfUsedChanges());				
			}
		}
		return maxChangeCount;
	}
	
	public List<Delta> getAllDeltas() {
		return Collections.unmodifiableList(allDeltas);
	}
	
	public List<Delta> getUnsuccessfulDeltas() {
		return Collections.unmodifiableList(unsuccessfulDeltas);		
	}
	
	public List<Delta> getRemainingDeltas() {
		List<Delta> remainingDeltas = new ArrayList<Delta>();
		remainingDeltas.addAll(allDeltas);
		remainingDeltas.removeAll(unsuccessfulDeltas);
		return Collections.unmodifiableList(remainingDeltas);
	}
	
	public boolean isEmpty() {
		if (allDeltas.size() - unsuccessfulDeltas.size() == 0) {
			return true;
		}
		return false;
	}
	
	public int allSize() {
		return allDeltas.size();
	}
	
	public int remainingSize() {
		return allDeltas.size() - unsuccessfulDeltas.size();
	}
	
	public boolean removeAll(final List<Delta> oldDeltas) {
		boolean error = false;
		for (Delta d : oldDeltas) {
			if (!remove(d)) {
				error = true;
			}
		}
		return error;
	}
	
	public boolean remove(final Delta d) {
		if (allDeltas.contains(d)) {
			unsuccessfulDeltas.add(d); 
			if (d.getNumberOfUsedChanges() == highestChangeCountNow) {
				highestChangeCountNow = calculateHighestChangeCount();
			}
			return true;
		} else {
			return false;
		}
	}
}
