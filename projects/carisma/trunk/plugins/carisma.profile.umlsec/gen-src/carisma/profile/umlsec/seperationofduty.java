/**
 */
package carisma.profile.umlsec;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Action;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>seperationofduty</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * DA Milen Ivanov
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.seperationofduty#getBase_Action <em>Base Action</em>}</li>
 *   <li>{@link carisma.profile.umlsec.seperationofduty#getActivity <em>Activity</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.UmlsecPackage#getseperationofduty()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='seperation of duty'"
 * @generated
 */
public interface seperationofduty extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Action</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Action</em>' reference.
	 * @see #setBase_Action(Action)
	 * @see carisma.profile.umlsec.UmlsecPackage#getseperationofduty_Base_Action()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Action getBase_Action();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.seperationofduty#getBase_Action <em>Base Action</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Action</em>' reference.
	 * @see #getBase_Action()
	 * @generated
	 */
	void setBase_Action(Action value);

	/**
	 * Returns the value of the '<em><b>Activity</b></em>' reference list.
	 * The list contents are of type {@link org.eclipse.uml2.uml.Action}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Activity</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Activity</em>' reference list.
	 * @see carisma.profile.umlsec.UmlsecPackage#getseperationofduty_Activity()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Action> getActivity();

	/**
	 * Retrieves the first {@link org.eclipse.uml2.uml.Action} with the specified '<em><b>Name</b></em>' from the '<em><b>Activity</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param name The '<em><b>Name</b></em>' of the {@link org.eclipse.uml2.uml.Action} to retrieve, or <code>null</code>.
	 * @return The first {@link org.eclipse.uml2.uml.Action} with the specified '<em><b>Name</b></em>', or <code>null</code>.
	 * @see #getActivity()
	 * @generated
	 */
	Action getActivity(String name);

	/**
	 * Retrieves the first {@link org.eclipse.uml2.uml.Action} with the specified '<em><b>Name</b></em>' from the '<em><b>Activity</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param name The '<em><b>Name</b></em>' of the {@link org.eclipse.uml2.uml.Action} to retrieve, or <code>null</code>.
	 * @param ignoreCase Whether to ignore case in {@link java.lang.String} comparisons.
	 * @param eClass The Ecore class of the {@link org.eclipse.uml2.uml.Action} to retrieve, or <code>null</code>.
	 * @return The first {@link org.eclipse.uml2.uml.Action} with the specified '<em><b>Name</b></em>', or <code>null</code>.
	 * @see #getActivity()
	 * @generated
	 */
	Action getActivity(String name, boolean ignoreCase, EClass eClass);

} // seperationofduty
