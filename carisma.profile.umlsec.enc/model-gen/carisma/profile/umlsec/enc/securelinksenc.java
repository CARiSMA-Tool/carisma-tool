/**
 */
package carisma.profile.umlsec.enc;

import carisma.profile.umlsec.securelinks;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>securelinksenc</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.enc.securelinksenc#getEncAdversary <em>Enc Adversary</em>}</li>
 *   <li>{@link carisma.profile.umlsec.enc.securelinksenc#getEncModel <em>Enc Model</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.enc.EncPackage#getsecurelinksenc()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='secure links enc'"
 * @generated
 */
public interface securelinksenc extends securelinks {
	/**
	 * Returns the value of the '<em><b>Enc Adversary</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Enc Adversary</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Enc Adversary</em>' attribute.
	 * @see #setEncAdversary(String)
	 * @see carisma.profile.umlsec.enc.EncPackage#getsecurelinksenc_EncAdversary()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getEncAdversary();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.enc.securelinksenc#getEncAdversary <em>Enc Adversary</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Enc Adversary</em>' attribute.
	 * @see #getEncAdversary()
	 * @generated
	 */
	void setEncAdversary(String value);

	/**
	 * Returns the value of the '<em><b>Enc Model</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Enc Model</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Enc Model</em>' attribute.
	 * @see #setEncModel(String)
	 * @see carisma.profile.umlsec.enc.EncPackage#getsecurelinksenc_EncModel()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getEncModel();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.enc.securelinksenc#getEncModel <em>Enc Model</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Enc Model</em>' attribute.
	 * @see #getEncModel()
	 * @generated
	 */
	void setEncModel(String value);

} // securelinksenc
