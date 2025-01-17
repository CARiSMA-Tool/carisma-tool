package carisma.check.idscheck.dto;

import org.eclipse.uml2.uml.Lifeline;

public class StartEndLifelineDto {
	private Lifeline startLifeline;
	private Lifeline endLifeline;
	
	public StartEndLifelineDto(Lifeline startLifeline, Lifeline endLifeline) {
		this.startLifeline = startLifeline;
		this.endLifeline = endLifeline;
	}
	
	public Lifeline getStartLifeline() {
		return startLifeline;
	}
	public void setStartLifeline(Lifeline startLifeline) {
		this.startLifeline = startLifeline;
	}
	public Lifeline getEndLifeline() {
		return endLifeline;
	}
	public void setEndLifeline(Lifeline endLifeline) {
		this.endLifeline = endLifeline;
	}
	
	
	
	

}
