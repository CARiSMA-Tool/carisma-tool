/**
 */
package carisma.profile.umlsec.extension4ids;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Lifeline;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>consumer</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.extension4ids.consumer#getBase_Lifeline <em>Base Lifeline</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getconsumer()
 * @model
 * @generated
 */
public interface consumer extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Lifeline</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Lifeline</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Lifeline</em>' reference.
	 * @see #setBase_Lifeline(Lifeline)
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getconsumer_Base_Lifeline()
	 * @model ordered="false"
	 * @generated
	 */
	Lifeline getBase_Lifeline();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.extension4ids.consumer#getBase_Lifeline <em>Base Lifeline</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Lifeline</em>' reference.
	 * @see #getBase_Lifeline()
	 * @generated
	 */
	void setBase_Lifeline(Lifeline value);

} // consumer
