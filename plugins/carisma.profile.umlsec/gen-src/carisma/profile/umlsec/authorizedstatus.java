/**
 */
package carisma.profile.umlsec;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.State;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>authorizedstatus</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.authorizedstatus#getPermission <em>Permission</em>}</li>
 *   <li>{@link carisma.profile.umlsec.authorizedstatus#getBase_State <em>Base State</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.UmlsecPackage#getauthorizedstatus()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='authorized-status'"
 * @generated
 */
public interface authorizedstatus extends EObject {
	/**
	 * Returns the value of the '<em><b>Permission</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Permission</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Permission</em>' attribute.
	 * @see #setPermission(String)
	 * @see carisma.profile.umlsec.UmlsecPackage#getauthorizedstatus_Permission()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getPermission();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.authorizedstatus#getPermission <em>Permission</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Permission</em>' attribute.
	 * @see #getPermission()
	 * @generated
	 */
	void setPermission(String value);

	/**
	 * Returns the value of the '<em><b>Base State</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base State</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base State</em>' reference.
	 * @see #setBase_State(State)
	 * @see carisma.profile.umlsec.UmlsecPackage#getauthorizedstatus_Base_State()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	State getBase_State();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.authorizedstatus#getBase_State <em>Base State</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base State</em>' reference.
	 * @see #getBase_State()
	 * @generated
	 */
	void setBase_State(State value);

} // authorizedstatus
