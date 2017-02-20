/**
 */
package carisma.profile.umlsec;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>securelinks</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.securelinks#getAdversary <em>Adversary</em>}</li>
 *   <li>{@link carisma.profile.umlsec.securelinks#getBase_Package <em>Base Package</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.UmlsecPackage#getsecurelinks()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='secure links'"
 * @generated
 */
public interface securelinks extends EObject {
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
	 * @see carisma.profile.umlsec.UmlsecPackage#getsecurelinks_Adversary()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getAdversary();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.securelinks#getAdversary <em>Adversary</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Adversary</em>' attribute.
	 * @see #getAdversary()
	 * @generated
	 */
	void setAdversary(String value);

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
	 * @see carisma.profile.umlsec.UmlsecPackage#getsecurelinks_Base_Package()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Package getBase_Package();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.securelinks#getBase_Package <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Package</em>' reference.
	 * @see #getBase_Package()
	 * @generated
	 */
	void setBase_Package(org.eclipse.uml2.uml.Package value);

} // securelinks
