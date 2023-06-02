/**
 */
package carisma.profile.umlsec.umlsec4ids;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Node;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>X509</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.X509#getBase_Node <em>Base Node</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getX509()
 * @model
 * @generated
 */
public interface X509 extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Node</em>' reference.
	 * @see #setBase_Node(Node)
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getX509_Base_Node()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Node getBase_Node();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.umlsec4ids.X509#getBase_Node <em>Base Node</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Node</em>' reference.
	 * @see #getBase_Node()
	 * @generated
	 */
	void setBase_Node(Node value);

} // X509
