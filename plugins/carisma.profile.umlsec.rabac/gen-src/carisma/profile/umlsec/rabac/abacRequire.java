/**
 */
package carisma.profile.umlsec.rabac;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Transition;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>abac Require</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.rabac.abacRequire#getRight <em>Right</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.abacRequire#getFilters <em>Filters</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.abacRequire#getBase_Transition <em>Base Transition</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.abacRequire#getBase_Operation <em>Base Operation</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.rabac.RabacPackage#getabacRequire()
 * @model
 * @generated
 */
public interface abacRequire extends EObject {
	/**
	 * Returns the value of the '<em><b>Right</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Right</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Right</em>' attribute.
	 * @see #setRight(String)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getabacRequire_Right()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getRight();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.abacRequire#getRight <em>Right</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Right</em>' attribute.
	 * @see #getRight()
	 * @generated
	 */
	void setRight(String value);

	/**
	 * Returns the value of the '<em><b>Filters</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Filters</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Filters</em>' attribute.
	 * @see #setFilters(String)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getabacRequire_Filters()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getFilters();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.abacRequire#getFilters <em>Filters</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Filters</em>' attribute.
	 * @see #getFilters()
	 * @generated
	 */
	void setFilters(String value);

	/**
	 * Returns the value of the '<em><b>Base Transition</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Transition</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Transition</em>' reference.
	 * @see #setBase_Transition(Transition)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getabacRequire_Base_Transition()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Transition getBase_Transition();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.abacRequire#getBase_Transition <em>Base Transition</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Transition</em>' reference.
	 * @see #getBase_Transition()
	 * @generated
	 */
	void setBase_Transition(Transition value);

	/**
	 * Returns the value of the '<em><b>Base Operation</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Operation</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Operation</em>' reference.
	 * @see #setBase_Operation(Operation)
	 * @see carisma.profile.umlsec.rabac.RabacPackage#getabacRequire_Base_Operation()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Operation getBase_Operation();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.rabac.abacRequire#getBase_Operation <em>Base Operation</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Operation</em>' reference.
	 * @see #getBase_Operation()
	 * @generated
	 */
	void setBase_Operation(Operation value);

} // abacRequire
