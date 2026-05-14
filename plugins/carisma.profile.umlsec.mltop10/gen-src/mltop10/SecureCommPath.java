/**
 */
package mltop10;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.CommunicationPath;

/**
 * <!-- begin-user-doc --> A representation of the model object '<em><b>Secure
 * Comm Path</b></em>'. <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link mltop10.SecureCommPath#getBase_CommunicationPath <em>Base
 * Communication Path</em>}</li>
 * <li>{@link mltop10.SecureCommPath#isConfidelityPreserving <em>Confidelity
 * Preserving</em>}</li>
 * <li>{@link mltop10.SecureCommPath#isIntegrityPreserving <em>Integrity
 * Preserving</em>}</li>
 * </ul>
 *
 * @see mltop10.Mltop10Package#getSecureCommPath()
 * @model
 * @generated
 */
public interface SecureCommPath extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Communication Path</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Communication Path</em>' reference isn't
	 * clear, there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>Base Communication Path</em>' reference.
	 * @see #setBase_CommunicationPath(CommunicationPath)
	 * @see mltop10.Mltop10Package#getSecureCommPath_Base_CommunicationPath()
	 * @model ordered="false"
	 * @generated
	 */
	CommunicationPath getBase_CommunicationPath();

	/**
	 * Sets the value of the
	 * '{@link mltop10.SecureCommPath#getBase_CommunicationPath <em>Base
	 * Communication Path</em>}' reference. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @param value the new value of the '<em>Base Communication Path</em>'
	 *              reference.
	 * @see #getBase_CommunicationPath()
	 * @generated
	 */
	void setBase_CommunicationPath(CommunicationPath value);

	/**
	 * Returns the value of the '<em><b>Confidelity Preserving</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Confidelity Preserving</em>' attribute isn't
	 * clear, there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>Confidelity Preserving</em>' attribute.
	 * @see #setConfidelityPreserving(boolean)
	 * @see mltop10.Mltop10Package#getSecureCommPath_ConfidelityPreserving()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true"
	 *        ordered="false"
	 * @generated
	 */
	boolean isConfidelityPreserving();

	/**
	 * Sets the value of the '{@link mltop10.SecureCommPath#isConfidelityPreserving
	 * <em>Confidelity Preserving</em>}' attribute. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @param value the new value of the '<em>Confidelity Preserving</em>'
	 *              attribute.
	 * @see #isConfidelityPreserving()
	 * @generated
	 */
	void setConfidelityPreserving(boolean value);

	/**
	 * Returns the value of the '<em><b>Integrity Preserving</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Integrity Preserving</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>Integrity Preserving</em>' attribute.
	 * @see #setIntegrityPreserving(boolean)
	 * @see mltop10.Mltop10Package#getSecureCommPath_IntegrityPreserving()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true"
	 *        ordered="false"
	 * @generated
	 */
	boolean isIntegrityPreserving();

	/**
	 * Sets the value of the '{@link mltop10.SecureCommPath#isIntegrityPreserving
	 * <em>Integrity Preserving</em>}' attribute. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @param value the new value of the '<em>Integrity Preserving</em>' attribute.
	 * @see #isIntegrityPreserving()
	 * @generated
	 */
	void setIntegrityPreserving(boolean value);

} // SecureCommPath
