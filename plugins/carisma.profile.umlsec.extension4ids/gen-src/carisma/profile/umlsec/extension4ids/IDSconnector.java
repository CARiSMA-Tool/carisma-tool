/**
 */
package carisma.profile.umlsec.extension4ids;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>ID Sconnector</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.extension4ids.IDSconnector#getBase_Artifact <em>Base Artifact</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getIDSconnector()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='IDS connector'"
 * @generated
 */
public interface IDSconnector extends EObject {
	/**
	 * Returns the value of the '<em><b>Base Artifact</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Artifact</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Artifact</em>' reference.
	 * @see #setBase_Artifact(Artifact)
	 * @see carisma.profile.umlsec.extension4ids.Extension4idsPackage#getIDSconnector_Base_Artifact()
	 * @model ordered="false"
	 * @generated
	 */
	Artifact getBase_Artifact();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.extension4ids.IDSconnector#getBase_Artifact <em>Base Artifact</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Artifact</em>' reference.
	 * @see #getBase_Artifact()
	 * @generated
	 */
	void setBase_Artifact(Artifact value);

} // IDSconnector
