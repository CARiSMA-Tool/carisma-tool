/**
 */
package carisma.profile.umlsec;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>datasecurity</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.datasecurity#getAdversary <em>Adversary</em>}</li>
 *   <li>{@link carisma.profile.umlsec.datasecurity#getBase_Package <em>Base Package</em>}</li>
 *   <li>{@link carisma.profile.umlsec.datasecurity#getAuthenticity <em>Authenticity</em>}</li>
 *   <li>{@link carisma.profile.umlsec.datasecurity#getIntegrity <em>Integrity</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.UmlsecPackage#getdatasecurity()
 * @model annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='data security'"
 * @generated
 */
public interface datasecurity extends EObject {
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
	 * @see carisma.profile.umlsec.UmlsecPackage#getdatasecurity_Adversary()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getAdversary();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.datasecurity#getAdversary <em>Adversary</em>}' attribute.
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
	 * @see carisma.profile.umlsec.UmlsecPackage#getdatasecurity_Base_Package()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Package getBase_Package();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.datasecurity#getBase_Package <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Package</em>' reference.
	 * @see #getBase_Package()
	 * @generated
	 */
	void setBase_Package(org.eclipse.uml2.uml.Package value);

	/**
	 * Returns the value of the '<em><b>Authenticity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Authenticity</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Authenticity</em>' attribute.
	 * @see #setAuthenticity(String)
	 * @see carisma.profile.umlsec.UmlsecPackage#getdatasecurity_Authenticity()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getAuthenticity();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.datasecurity#getAuthenticity <em>Authenticity</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Authenticity</em>' attribute.
	 * @see #getAuthenticity()
	 * @generated
	 */
	void setAuthenticity(String value);

	/**
	 * Returns the value of the '<em><b>Integrity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Integrity</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Integrity</em>' attribute.
	 * @see #setIntegrity(String)
	 * @see carisma.profile.umlsec.UmlsecPackage#getdatasecurity_Integrity()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" required="true" ordered="false"
	 * @generated
	 */
	String getIntegrity();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.datasecurity#getIntegrity <em>Integrity</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Integrity</em>' attribute.
	 * @see #getIntegrity()
	 * @generated
	 */
	void setIntegrity(String value);

} // datasecurity
