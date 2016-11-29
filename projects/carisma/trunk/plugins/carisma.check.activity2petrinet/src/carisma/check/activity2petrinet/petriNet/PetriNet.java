/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.check.activity2petrinet.petriNet;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

/**
 * This class models a petri net.
 * @author Kubi Mensah
 *
 */
public class PetriNet {
   
	/**
	 * The ID of the petri net object.
	 */
	private String petriNetObjectID;
	/**
	 * The type of this object.
	 */
	private String type;
	/**
	 * List of arcs contained by the petri net.
	 */
	private List<Arc> arcs;
	/**
	 * List of places contained by the petri net.
	 */
	private List<Place> places;
	/**
	 * List of transitions contained by the petri net.
	 */
	private List<Transition> transitions;
	/**
	 * Number of places.
	 */
	private int placeCount; // TODO: braucht man das wirklich? Kann das überhaupt != places.size werden?
	/**
	 * Number of transitions.
	 */
	private int transCount; // TODO: s.o.

	/**
	 * Constructs a new petri net object.
	 * @param id the ID of the Petri Net
	 * @param type the type of the Petri net
	 */
	public PetriNet(final String id, final String type) {
		this.setId(id);
		this.setType(type);
		this.arcs = new ArrayList<>();
		this.places = new ArrayList<>();
		this.transitions = new ArrayList<>();
		this.placeCount = 0;
		this.transCount = 0;
	}

	/**
	 * Sets the id.
	 * @param id the id of the petri net
	 */
	public final void setId(final String id) {
		this.petriNetObjectID = id;
	}

	/**
	 * Returns the id.
	 * @return the id.
	 */
	public final String getId() {
		return this.petriNetObjectID;
	}

	/**
	 * Sets the type.
	 * @param type the petri net type
	 */
	public final void setType(final String type) {
		this.type = type;
	}
	
	/**
	 * Returns a list of arcs contained by the petri net.
	 * @return list of arc objects
	 */
	public final List<Arc> getArcs() {
		return this.arcs;
	}
	
	/**
	 * Returns a list of places contained by the petri net.
	 * @return list of place objects
	 */
	public final List<Place> getPlaces() {
		return this.places;
	}

	/**
	 * Returns a list of transitions contained by the petri net.
	 * @return list of transition objects
	 */
	public final List<Transition> getTransitions() {
		return this.transitions;
	}
	/**
	 * Returns the type of this object.
	 * @return type of this object.
	 */
	public final String getType() {
		return this.type;
	}
	/**
	 * Adds a place to the petri net.
	 * @param p place
	 * @return returns true if the place was successfuly added.
	 */
	public final boolean addPlace(final Place p) {
		if (!containsPlace(p.getId())) {
			if (p.getName().trim().equals("")) {
				p.setName("p" + this.placeCount);
				this.placeCount++;
			}
			this.places.add(p);
			return true;
		}
		return false;
	}

	public final boolean addPlaces(final Place... p) {
		boolean ret = true;
		for(Place curPlace : p)
			ret &= this.addPlace(curPlace);
		return ret;
	}

	
	/**
	 * Adds a transition to the petri net.
	 * @param t transition
	 * @return returns true if the transition was successfuly added.
	 */

	public final boolean addTransition(final Transition t) {
		if (!containsTransition(t.getId())) {
			if (t.getName().trim().equals("")) {
				t.setName("t" + this.transCount);
				this.transCount++;
			}
			this.transitions.add(t);
			return true;
		}
		return false;
	}
	

	public final boolean addTransitions(final Transition... t) {
		boolean ret = true;
		for(Transition curTransition : t){
			ret &= this.addTransition(curTransition);
		}
		return ret;
	}

	
	/**
	 * Adds an arc to the petri net.
	 * @param a arc
	 * @return returns true if the a was successfuly added.
	 */
	public final boolean addArc(final Arc a) {
		if (!containsArc(a.getId())) {
			this.arcs.add(a);
			return true;
		}
		return false;
	}

	public final boolean addArcs(final Arc... a) {
		boolean ret = true;
		for(Arc curArc : a){
			ret &= this.addArc(curArc);
		}
		return ret;
	}

	/**
	 * Returns true if the petri net contains an arc with the given id.
	 * @param id ID of the arc
	 * @return truth value
	 */
	public final boolean containsArc(final String id) {
		for (Arc arc : this.arcs) {
			if (arc.getId().equals(id)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Returns true if the petri net contains a place with the given id.
	 * @param id ID of the place
	 * @return truth value
	 */
	public final boolean containsPlace(final String id) {
		for (Place place : this.places) {
			if (place.getId().equals(id)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Returns true if the petri net contains a transtion with the given id.
	 * @param id ID of the transition
	 * @return truth value
	 */
	public final boolean containsTransition(final String id) {
		for (Transition trans : this.transitions) {
			if (trans.getId().equals(id)) {
				return true;
			}
		}
		return false;
	}

	public final Place getPlaceByID(final String id){
		for (Place curPlace : this.places) {
			if (curPlace.getId().equals(id)) {
				return curPlace;
			}
		}
		return null;
	}

	public final Place getPlaceByName(final String name){
		for (Place curPlace : this.places) {
			if (curPlace.getName().equals(name)) {
				return curPlace;
			}
		}
		return null;
	}

	public final Transition getTransitionByID(final String id){
		for (Transition curTransition: this.transitions) {
			if (curTransition.getId().equals(id)) {
				return curTransition;
			}
		}
		return null;
	}

	public final Transition getTransitionByName(final String name){
		for (Transition curTransition : this.transitions) {
			if (curTransition.getName().equals(name)) {
				return curTransition;
			}
		}
		return null;
	}
	
	public final Collection<String> getInputPlaces(final String transitionName){
		LinkedList<String> ret = new LinkedList<>();

		for (Arc curArc : this.getArcs()) {
			if (curArc.getTarget().equals(transitionName)) {
				ret.add(curArc.getSource());
			}
		}		
		return ret;
	}

	public final Collection<String> getOutputPlaces(final String transitionName){
		LinkedList<String> ret = new LinkedList<>();

		for (Arc curArc : this.getArcs()) {
			if (curArc.getSource().equals(transitionName)) {
				ret.add(curArc.getTarget());
			}
		}		
		return ret;
	}
}
