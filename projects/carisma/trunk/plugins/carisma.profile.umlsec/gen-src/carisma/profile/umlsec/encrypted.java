/**
 */
package carisma.profile.umlsec;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.CommunicationPath;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>encrypted</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.encrypted#getBase_CommunicationPath <em>Base Communication Path</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.UmlsecPackage#getencrypted()
 * @model
 * @generated
 */
public interface encrypted extends EObject {
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
	 * @see carisma.profile.umlsec.UmlsecPackage#getencrypted_Base_CommunicationPath()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	CommunicationPath getBase_CommunicationPath();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.encrypted#getBase_CommunicationPath <em>Base Communication Path</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Communication Path</em>' reference.
	 * @see #getBase_CommunicationPath()
	 * @generated
	 */
	void setBase_CommunicationPath(CommunicationPath value);

} // encrypted
