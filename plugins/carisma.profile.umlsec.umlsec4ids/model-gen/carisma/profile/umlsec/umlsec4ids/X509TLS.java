/**
 */
package carisma.profile.umlsec.umlsec4ids;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Node;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>X509TLS</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.X509TLS#getBase_Node <em>Base Node</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.X509TLS#getExpiration_date_yyyy_mm_dd <em>Expiration date yyyy mm dd</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getX509TLS()
 * @model
 * @generated
 */
public interface X509TLS extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Node</em>' reference.
	 * @see #setBase_Node(Node)
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getX509TLS_Base_Node()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Node getBase_Node();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.umlsec4ids.X509TLS#getBase_Node <em>Base Node</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Node</em>' reference.
	 * @see #getBase_Node()
	 * @generated
	 */
	void setBase_Node(Node value);

	/**
	 * Returns the value of the '<em><b>Expiration date yyyy mm dd</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Expiration date yyyy mm dd</em>' attribute.
	 * @see #setExpiration_date_yyyy_mm_dd(int)
	 * @see carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage#getX509TLS_Expiration_date_yyyy_mm_dd()
	 * @model dataType="org.eclipse.uml2.types.Integer" required="true" ordered="false"
	 * @generated
	 */
	int getExpiration_date_yyyy_mm_dd();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.umlsec4ids.X509TLS#getExpiration_date_yyyy_mm_dd <em>Expiration date yyyy mm dd</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Expiration date yyyy mm dd</em>' attribute.
	 * @see #getExpiration_date_yyyy_mm_dd()
	 * @generated
	 */
	void setExpiration_date_yyyy_mm_dd(int value);

} // X509TLS
