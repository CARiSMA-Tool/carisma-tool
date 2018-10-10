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
		this.allDeltas.addAll(containedDeltas);
		this.highestChangeCountAllTime = calculateHighestChangeCount();
		this.highestChangeCountNow = this.highestChangeCountAllTime;
	}
	
	public void init() {
		if (this.allDeltas == null) {
			this.allDeltas = new ArrayList<>();			
		} else {
			this.allDeltas.clear();
		}
		if (this.unsuccessfulDeltas == null) {
			this.unsuccessfulDeltas = new ArrayList<>();
		} else {
			this.unsuccessfulDeltas.clear();
		}
		
	}
	
	public int getHighestChangeCountAllTime() {
		return this.highestChangeCountAllTime;
	}
	
	public int getHighestChangeCountNow() {
		return this.highestChangeCountNow;
	}
	
	private int calculateHighestChangeCount() {
		int maxChangeCount = 0;
		
		for (Delta d : this.allDeltas) {
			if (!(this.unsuccessfulDeltas.contains(d))) {
				maxChangeCount = Math.max(maxChangeCount, d.getNumberOfUsedChanges());				
			}
		}
		return maxChangeCount;
	}
	
	public List<Delta> getAllDeltas() {
		return Collections.unmodifiableList(this.allDeltas);
	}
	
	public List<Delta> getUnsuccessfulDeltas() {
		return Collections.unmodifiableList(this.unsuccessfulDeltas);		
	}
	
	public List<Delta> getRemainingDeltas() {
		List<Delta> remainingDeltas = new ArrayList<>();
		remainingDeltas.addAll(this.allDeltas);
		remainingDeltas.removeAll(this.unsuccessfulDeltas);
		return Collections.unmodifiableList(remainingDeltas);
	}
	
	public boolean isEmpty() {
		if (this.allDeltas.size() - this.unsuccessfulDeltas.size() == 0) {
			return true;
		}
		return false;
	}
	
	public int allSize() {
		return this.allDeltas.size();
	}
	
	public int remainingSize() {
		return this.allDeltas.size() - this.unsuccessfulDeltas.size();
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
		if (this.allDeltas.contains(d)) {
			this.unsuccessfulDeltas.add(d); 
			if (d.getNumberOfUsedChanges() == this.highestChangeCountNow) {
				this.highestChangeCountNow = calculateHighestChangeCount();
			}
			return true;
		}
		return false;
	}
}
