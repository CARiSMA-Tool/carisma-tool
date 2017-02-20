/**
 */
package carisma.profile.umlsec;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Node;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>smartcard</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.smartcard#getBase_Node <em>Base Node</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.UmlsecPackage#getsmartcard()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='smart card'"
 * @generated
 */
public interface smartcard extends EObject {
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
	 * @see carisma.profile.umlsec.UmlsecPackage#getsmartcard_Base_Node()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Node getBase_Node();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.smartcard#getBase_Node <em>Base Node</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Node</em>' reference.
	 * @see #getBase_Node()
	 * @generated
	 */
	void setBase_Node(Node value);

} // smartcard
