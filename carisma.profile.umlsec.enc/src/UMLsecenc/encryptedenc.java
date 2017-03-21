/**
 */
package UMLsecenc;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.CommunicationPath;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>encryptedenc</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link UMLsecenc.encryptedenc#getAlg <em>Alg</em>}</li>
 *   <li>{@link UMLsecenc.encryptedenc#getKeylength <em>Keylength</em>}</li>
 *   <li>{@link UMLsecenc.encryptedenc#getBase_CommunicationPath <em>Base Communication Path</em>}</li>
 * </ul>
 *
 * @see UMLsecenc.UMLsecencPackage#getencryptedenc()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='encrypted enc'"
 * @generated
 */
public interface encryptedenc extends EObject {
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
	 * @see UMLsecenc.UMLsecencPackage#getencryptedenc_Alg()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getAlg();

	/**
	 * Sets the value of the '{@link UMLsecenc.encryptedenc#getAlg <em>Alg</em>}' attribute.
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
	 * @see UMLsecenc.UMLsecencPackage#getencryptedenc_Keylength()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getKeylength();

	/**
	 * Sets the value of the '{@link UMLsecenc.encryptedenc#getKeylength <em>Keylength</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Keylength</em>' attribute.
	 * @see #getKeylength()
	 * @generated
	 */
	void setKeylength(String value);

	/**
	 * Returns the value of the '<em><b>Base Communication Path</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Communication Path</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Communication Path</em>' reference.
	 * @see #setBase_CommunicationPath(CommunicationPath)
	 * @see UMLsecenc.UMLsecencPackage#getencryptedenc_Base_CommunicationPath()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	CommunicationPath getBase_CommunicationPath();

	/**
	 * Sets the value of the '{@link UMLsecenc.encryptedenc#getBase_CommunicationPath <em>Base Communication Path</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Communication Path</em>' reference.
	 * @see #getBase_CommunicationPath()
	 * @generated
	 */
	void setBase_CommunicationPath(CommunicationPath value);

} // encryptedenc
