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
package carisma.core.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;

/**
 * Utility class for working with objects.
 * @author wenzel
 */
public final class EObjectUtil {

	/**
	 * Prohibits instantiation of this class.
	 */
	private EObjectUtil() {
	}
	
	/**
	 * Returns the name of an object.
	 * @param obj object
	 * @return the name of the object
	 */
	public static String getName(final EObject obj) {
		if (obj != null) {
			EStructuralFeature sf = obj.eClass().getEStructuralFeature("name");
			if (sf == null) {
				sf = obj.eClass().getEStructuralFeature("Name");
			}
			if (sf == null) {
				sf = obj.eClass().getEStructuralFeature("NAME");
			}
			if (sf != null) {
				return String.valueOf(obj.eGet(sf));
			}
			return "[unnamed " + obj.eClass().getName() + "]";
		}
		return "[no element]";
	}

	/**
	 * Return the a string of form "type 'name'" of an object.
	 * @param obj object
	 * @return a string of form "type 'name'" of the object
	 */
	public static String getTypeAndName(final EObject obj) {
		if (obj != null) {
			return obj.eClass().getName() + " '" + getName(obj) + "'";
		}
		return "[no element]";
	}
	
	/**
	 * Returns the value of the given single-feature of the given object.
	 * @param object the object that is inspected
	 * @param featureName the name of the feature which is evaluated
	 * @param type the expected type of the feature value
	 * @param <T> type
	 * @return the value of the given feature of the given object
	 */
	@SuppressWarnings("unchecked")
	public static <T> T getSingleFeature(final EObject object, final String featureName, final Class<T> type) {
		EStructuralFeature sf = object.eClass().getEStructuralFeature(featureName);
		if (sf == null) {
			throw new IllegalArgumentException("No such feature for " + object.eClass().getName() + ": " + featureName);
		}
		if (sf.isMany()) {
			throw new IllegalArgumentException("Feature of " + object.eClass().getName() + " has multiplicity higher than 1: " + featureName);
		}
		return (T) object.eGet(sf);
	}
	
	/**
	 * Returns the values of the given many-feature of the given object.
	 * @param object the object that is inspected
	 * @param featureName the name of the feature which is evaluated
	 * @param type the expected type of the feature value
	 * @param <T> type
	 * @return the values of the given feature of the given object
	 */
	@SuppressWarnings("unchecked")
	public static <T> List<T> getManyFeature(final EObject object, final String featureName, final Class<T> type) {
		EStructuralFeature sf = object.eClass().getEStructuralFeature(featureName);
		if (sf == null) {
			throw new IllegalArgumentException("No such feature for " + object.eClass().getName() + ": " + featureName);
		}
		if (!sf.isMany()) {
			throw new IllegalArgumentException("Feature of " + object.eClass().getName() + " has multiplicity higher than 1: " + featureName);
		}
		return (List<T>) object.eGet(sf);
	}
	
	/**
	 * Returns the values of the given single-feature of the given objects.
	 * @param objects the objects that are inspected
	 * @param featureName the name of the feature which is evaluated
	 * @param type the expected type of the feature value
	 * @param <T> type
	 * @return the values of the given feature of the given object
	 */
	public static <T> List<T> getAllSingleFeatures(final Collection<EObject> objects, final String featureName, final Class<T> type) {
		List<T> result = new ArrayList<>();
		for (EObject obj : objects) {
			result.add(getSingleFeature(obj, featureName, type));
		}
		return result;
	}
	
	
}
