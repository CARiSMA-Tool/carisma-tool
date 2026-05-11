/**
 */
package mltop10;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Node;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Training Data Server</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link mltop10.TrainingDataServer#getBase_Node <em>Base Node</em>}</li>
 *   <li>{@link mltop10.TrainingDataServer#isSecureDataStorage <em>Secure Data Storage</em>}</li>
 * </ul>
 *
 * @see mltop10.Mltop10Package#getTrainingDataServer()
 * @model
 * @generated
 */
public interface TrainingDataServer extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Node</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Node</em>' reference.
	 * @see #setBase_Node(Node)
	 * @see mltop10.Mltop10Package#getTrainingDataServer_Base_Node()
	 * @model ordered="false"
	 * @generated
	 */
	Node getBase_Node();

	/**
	 * Sets the value of the '{@link mltop10.TrainingDataServer#getBase_Node <em>Base Node</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Node</em>' reference.
	 * @see #getBase_Node()
	 * @generated
	 */
	void setBase_Node(Node value);

	/**
	 * Returns the value of the '<em><b>Secure Data Storage</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Secure Data Storage</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Secure Data Storage</em>' attribute.
	 * @see #setSecureDataStorage(boolean)
	 * @see mltop10.Mltop10Package#getTrainingDataServer_SecureDataStorage()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isSecureDataStorage();

	/**
	 * Sets the value of the '{@link mltop10.TrainingDataServer#isSecureDataStorage <em>Secure Data Storage</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Secure Data Storage</em>' attribute.
	 * @see #isSecureDataStorage()
	 * @generated
	 */
	void setSecureDataStorage(boolean value);

} // TrainingDataServer
