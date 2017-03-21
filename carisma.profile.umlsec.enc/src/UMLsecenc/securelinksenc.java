/**
 */
package UMLsecenc;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>securelinksenc</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link UMLsecenc.securelinksenc#getAdversary <em>Adversary</em>}</li>
 *   <li>{@link UMLsecenc.securelinksenc#getEncAdversary <em>Enc Adversary</em>}</li>
 *   <li>{@link UMLsecenc.securelinksenc#getEncModel <em>Enc Model</em>}</li>
 *   <li>{@link UMLsecenc.securelinksenc#getBase_Package <em>Base Package</em>}</li>
 * </ul>
 *
 * @see UMLsecenc.UMLsecencPackage#getsecurelinksenc()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='secure links enc'"
 * @generated
 */
public interface securelinksenc extends EObject {
	/**
	 * Returns the value of the '<em><b>Adversary</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Adversary</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Adversary</em>' attribute.
	 * @see #setAdversary(String)
	 * @see UMLsecenc.UMLsecencPackage#getsecurelinksenc_Adversary()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getAdversary();

	/**
	 * Sets the value of the '{@link UMLsecenc.securelinksenc#getAdversary <em>Adversary</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Adversary</em>' attribute.
	 * @see #getAdversary()
	 * @generated
	 */
	void setAdversary(String value);

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
	 * @see UMLsecenc.UMLsecencPackage#getsecurelinksenc_EncAdversary()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getEncAdversary();

	/**
	 * Sets the value of the '{@link UMLsecenc.securelinksenc#getEncAdversary <em>Enc Adversary</em>}' attribute.
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
	 * @see UMLsecenc.UMLsecencPackage#getsecurelinksenc_EncModel()
	 * @model dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getEncModel();

	/**
	 * Sets the value of the '{@link UMLsecenc.securelinksenc#getEncModel <em>Enc Model</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Enc Model</em>' attribute.
	 * @see #getEncModel()
	 * @generated
	 */
	void setEncModel(String value);

	/**
	 * Returns the value of the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Package</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Package</em>' reference.
	 * @see #setBase_Package(org.eclipse.uml2.uml.Package)
	 * @see UMLsecenc.UMLsecencPackage#getsecurelinksenc_Base_Package()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Package getBase_Package();

	/**
	 * Sets the value of the '{@link UMLsecenc.securelinksenc#getBase_Package <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Package</em>' reference.
	 * @see #getBase_Package()
	 * @generated
	 */
	void setBase_Package(org.eclipse.uml2.uml.Package value);

} // securelinksenc
