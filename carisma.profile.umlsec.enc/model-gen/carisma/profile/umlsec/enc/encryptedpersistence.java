/**
 */
package carisma.profile.umlsec.enc;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>encryptedpersistence</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.enc.encryptedpersistence#getBase_Class <em>Base Class</em>}</li>
 *   <li>{@link carisma.profile.umlsec.enc.encryptedpersistence#getAlg <em>Alg</em>}</li>
 *   <li>{@link carisma.profile.umlsec.enc.encryptedpersistence#getKeylength <em>Keylength</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.enc.EncPackage#getencryptedpersistence()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='encrypted persistence'"
 * @generated
 */
public interface encryptedpersistence extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Class</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Class</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Class</em>' reference.
	 * @see #setBase_Class(org.eclipse.uml2.uml.Class)
	 * @see carisma.profile.umlsec.enc.EncPackage#getencryptedpersistence_Base_Class()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Class getBase_Class();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.enc.encryptedpersistence#getBase_Class <em>Base Class</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Class</em>' reference.
	 * @see #getBase_Class()
	 * @generated
	 */
	void setBase_Class(org.eclipse.uml2.uml.Class value);

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
	 * @see carisma.profile.umlsec.enc.EncPackage#getencryptedpersistence_Alg()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getAlg();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.enc.encryptedpersistence#getAlg <em>Alg</em>}' attribute.
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
	 * @see carisma.profile.umlsec.enc.EncPackage#getencryptedpersistence_Keylength()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getKeylength();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.enc.encryptedpersistence#getKeylength <em>Keylength</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Keylength</em>' attribute.
	 * @see #getKeylength()
	 * @generated
	 */
	void setKeylength(String value);

} // encryptedpersistence
