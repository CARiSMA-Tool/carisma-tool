/**
 */
package carisma.profile.umlsec.enc;

import carisma.profile.umlsec.encrypted;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>encryptedenc</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.enc.encryptedenc#getAlg <em>Alg</em>}</li>
 *   <li>{@link carisma.profile.umlsec.enc.encryptedenc#getKeylength <em>Keylength</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.enc.EncPackage#getencryptedenc()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='encrypted enc'"
 * @generated
 */
public interface encryptedenc extends encrypted {
	/**
	 * Returns the value of the '<em><b>Alg</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Alg</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Alg</em>' attribute.
	 * @see #setAlg(String)
	 * @see carisma.profile.umlsec.enc.EncPackage#getencryptedenc_Alg()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getAlg();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.enc.encryptedenc#getAlg <em>Alg</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Alg</em>' attribute.
	 * @see #getAlg()
	 * @generated
	 */
	void setAlg(String value);

	/**
	 * Returns the value of the '<em><b>Keylength</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Keylength</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Keylength</em>' attribute.
	 * @see #setKeylength(String)
	 * @see carisma.profile.umlsec.enc.EncPackage#getencryptedenc_Keylength()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getKeylength();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.enc.encryptedenc#getKeylength <em>Keylength</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Keylength</em>' attribute.
	 * @see #getKeylength()
	 * @generated
	 */
	void setKeylength(String value);

} // encryptedenc
