package carisma.bpmn2.marisk.util;

import java.util.Collection;

import org.eclipse.bpmn2.Activity;

public class IncompleteMappingExeption extends Exception {

	/**
	 * Generated serial 
	 */
	private static final long serialVersionUID = 8611402388871899119L;
	
	private final Collection<String> notFoundActivities;
	private final Collection<Activity> mappedActivities;

	public IncompleteMappingExeption(Collection<String> notfound, Collection<Activity> mapped) {
		this.notFoundActivities = notfound;
		this.mappedActivities = mapped;
	}

	public Collection<Activity> getMappedActivities() {
		return this.mappedActivities;
	}

	public Collection<String> getNotFoundActivities() {
		return this.notFoundActivities;
	}

}
