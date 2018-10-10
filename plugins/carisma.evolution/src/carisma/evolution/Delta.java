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
package carisma.evolution;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
/**
 * A delta contains a set of delta elements.
 * This class provides access methods for the delta. 
 * @author Daniel Warzecha
 *
 */
public class Delta {
	
	private List<String> changeIDs = new ArrayList<>();
	
	private List<DeltaElement> deltaContent = null;
	
	public Delta(final List<DeltaElement> content) {
		this(new ArrayList<String>(), content);
	}

	public Delta(final List<String> changeIDList, final List<DeltaElement> content) {
		if (changeIDList == null) {
			throw new IllegalArgumentException("Parameter 'chaneIDList' is null");
		}
		this.changeIDs.addAll(changeIDList);
		this.deltaContent = new ArrayList<>();
		this.deltaContent.addAll(content);
	}
	
	public int getNumberOfUsedChanges() {
		return this.changeIDs.size();
	}
	
	public List<String> getChangeIDs() {
		return Collections.unmodifiableList(this.changeIDs);
	}
	
	public List<DeltaElement> getContent() {
		return this.deltaContent;
	}
	
	/**
	 * Returns all SubstElements in the Delta.
	 * @return - set of SubstElements
	 */
	public List<SubstElement> getAllSubstitutions() {
		return getAllOfType(SubstElement.class);
	}
	/**
	 * Returns all DelElements in the Delta.
	 * @return - set of DelElements
	 */
	public List<DelElement> getAllDeletions() {
		return getAllOfType(DelElement.class);
	}
	/**
	 * Returns all AddElements in the Delta.
	 * @return - set of AddElements
	 */
	public final List<AddElement> getAllAdditions() {
		return getAllOfType(AddElement.class);
	}
	/**
	 * Returns all EditElements in the Delta.
	 * @return - set of EditElements
	 */
	public final List<EditElement> getAllEdits() {
		return getAllOfType(EditElement.class);
	}

	/**
	 * Returns all CopyElements in the Delta.
	 * @return - set of CopyElements
	 */
	public final List<CopyElement> getAllCopies() {
		return getAllOfType(CopyElement.class);
	}
	
	/**
	 * Checks if the delta removes the given model element from the model, either by deleting or substituting the element. 
	 * @param match - the element to check
	 * @return - true if the element is somehow removed from the model
	 */
	public final boolean removes(final EObject match) {
		for (DelElement del : getAllDeletions()) {
			if (del.getTarget().equals(match) || del.getAccompanyingDeletions().contains(match)) {
				return true;
			}
		}
		for (SubstElement subst : getAllSubstitutions()) {
			if (subst.getTarget().equals(match) || subst.getAccompanyingDeletions().contains(match)) {
				return true;
			}
		}
		return false;
	}
	/**
	 * Checks if the delta edits the given model element.
	 * @param match - the element to check
	 * @return - true if the element is edited, i.e. an EditElement with the model element as target exists
	 */
	public final boolean edits(final EObject match) {
		for (EditElement edit : getAllEdits()) {
			if (edit.getTarget().equals(match)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Checks if a model element defined by the pattern described by the given AddElement
	 * is added by the delta.
	 * @param pattern - the pattern to find
	 * @return - true if a model element matching the given pattern is added to the delta
	 */
	public final boolean adds(final AddElement pattern) {
		for (AddElement addShallow : getAllAdditions()) {
			for (AddElement addDeep : addShallow.getAllAddedElements()) {
				if (matchesAddedElement(addDeep, pattern)) {
					return true;
				}
			}
		}
		for (SubstElement subst : getAllSubstitutions()) {
			for (AddElement addedComponent : subst.getAllAddedElements()) {
				if (matchesAddedElement(addedComponent, pattern)) {
					return true;
				}
			}
		}
		return false;
	}
	
	public final List<EditElement> getEdits(final EObject target) {
		List<EditElement> edits = new ArrayList<>();
		for (EditElement edit : getAllEdits()) {
			if (target.equals(edit.getTarget())) {
				edits.add(edit);
			}
		}
		return edits;
	}
	
	/**
	 * Returns all AddElements matching the given pattern.
	 * @param pattern - the pattern to match
	 * @return - the AddElements matching the pattern
	 */
	public final List<AddElement> getMatchingAdditions(final AddElement pattern) {
		List<AddElement> matchingAdditions = new ArrayList<>();
		for (AddElement addShallow : getAllAdditions()) {
			for (AddElement addDeep : addShallow.getAllAddedElements()) {
				if (matchesAddedElement(addDeep, pattern)) {
					matchingAdditions.add(addDeep);
				}
			}
		}
		for (SubstElement subst : getAllSubstitutions()) {
			for (AddElement addedComponent : subst.getAllAddedElements()) {
				if (matchesAddedElement(addedComponent, pattern)) {
					matchingAdditions.add(addedComponent);
				}
			}
		}
		return matchingAdditions;		
	}
	
	/**
	 * Checks if the pattern AddElement constraints are fulfilled by the possibleMatch AddElement.
	 * @param possibleMatch - the AddElement to check
	 * @param pattern - the pattern to be fulfilled by matching AddElements
	 * @return - true if the pattern is matched by the possible match
	 */
	private final static boolean matchesAddedElement(final AddElement possibleMatch, final AddElement pattern) {
		boolean matchesTarget = false;
		boolean matchesMetaclass = false;
		boolean matchesKeyValues = true;
		EObject target = pattern.getTarget();
		EClass metaclass = pattern.getMetaClass();
		
		if (target == null || target.equals(possibleMatch.getTarget())) {
			matchesTarget = true;
		}
		if (metaclass == null || metaclass.equals(possibleMatch.getMetaClass())) {
			matchesMetaclass = true;
		}
		if (pattern.getValues().isEmpty()) {
			matchesKeyValues = true;
		} else {
			Map<String,Object> values = pattern.getValues();
			for (Entry<String,Object> keyValuePair : values.entrySet()) {
				if (!possibleMatch.getValues().entrySet().contains(keyValuePair)) {
					matchesKeyValues = false;
					break;
				}
			}
		}
		if (matchesTarget && matchesMetaclass && matchesKeyValues) {
			return true;
		}
		return false;
	}
	
	/**
	 * Searches the Delta for DeltaElements of the given type.
	 * @param <T> - a DeltaElement class
	 * @param type - the class
	 * @return - the set of DeltaElements of the given type in the delta
	 */
	private <T extends DeltaElement> List<T> getAllOfType(final Class<T> type) {
		List<T> allOfType = new ArrayList<>();
		for (DeltaElement de : this.deltaContent) {
			if (type.isInstance(de)) {
				allOfType.add(type.cast(de));
			}
		}
		return allOfType;		
	}
	
	/**
	 * Checks if the delta contains elements.
	 * @return - true, if delta is empty
	 */
	public boolean isEmpty() {
		return this.deltaContent.isEmpty();
	}
	
}
