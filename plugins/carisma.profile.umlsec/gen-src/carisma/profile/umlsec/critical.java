/**
 */
package carisma.profile.umlsec;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.Component;
import org.eclipse.uml2.uml.InstanceSpecification;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>critical</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.critical#getSecrecy <em>Secrecy</em>}</li>
 *   <li>{@link carisma.profile.umlsec.critical#getIntegrity <em>Integrity</em>}</li>
 *   <li>{@link carisma.profile.umlsec.critical#getHigh <em>High</em>}</li>
 *   <li>{@link carisma.profile.umlsec.critical#getBase_Class <em>Base Class</em>}</li>
 *   <li>{@link carisma.profile.umlsec.critical#getBase_Component <em>Base Component</em>}</li>
 *   <li>{@link carisma.profile.umlsec.critical#getFresh <em>Fresh</em>}</li>
 *   <li>{@link carisma.profile.umlsec.critical#getAuthenticity <em>Authenticity</em>}</li>
 *   <li>{@link carisma.profile.umlsec.critical#getBase_InstanceSpecification <em>Base Instance Specification</em>}</li>
 *   <li>{@link carisma.profile.umlsec.critical#getBase_Classifier <em>Base Classifier</em>}</li>
 *   <li>{@link carisma.profile.umlsec.critical#getPrivacy <em>Privacy</em>}</li>
 * </ul>
 *
 * @see carisma.profile.umlsec.UmlsecPackage#getcritical()
 * @model
 * @generated
 */
public interface critical extends EObject {
	/**
	 * Returns the value of the '<em><b>Secrecy</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Secrecy</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Secrecy</em>' attribute list.
	 * @see carisma.profile.umlsec.UmlsecPackage#getcritical_Secrecy()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getSecrecy();

	/**
	 * Returns the value of the '<em><b>Integrity</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Integrity</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Integrity</em>' attribute list.
	 * @see carisma.profile.umlsec.UmlsecPackage#getcritical_Integrity()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getIntegrity();

	/**
	 * Returns the value of the '<em><b>High</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>High</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>High</em>' attribute list.
	 * @see carisma.profile.umlsec.UmlsecPackage#getcritical_High()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getHigh();

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
	 * @see carisma.profile.umlsec.UmlsecPackage#getcritical_Base_Class()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Class getBase_Class();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.critical#getBase_Class <em>Base Class</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Class</em>' reference.
	 * @see #getBase_Class()
	 * @generated
	 */
	void setBase_Class(org.eclipse.uml2.uml.Class value);

	/**
	 * Returns the value of the '<em><b>Base Component</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Component</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Component</em>' reference.
	 * @see #setBase_Component(Component)
	 * @see carisma.profile.umlsec.UmlsecPackage#getcritical_Base_Component()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Component getBase_Component();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.critical#getBase_Component <em>Base Component</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Component</em>' reference.
	 * @see #getBase_Component()
	 * @generated
	 */
	void setBase_Component(Component value);

	/**
	 * Returns the value of the '<em><b>Fresh</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Fresh</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Fresh</em>' attribute list.
	 * @see carisma.profile.umlsec.UmlsecPackage#getcritical_Fresh()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getFresh();

	/**
	 * Returns the value of the '<em><b>Authenticity</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Authenticity</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Authenticity</em>' attribute list.
	 * @see carisma.profile.umlsec.UmlsecPackage#getcritical_Authenticity()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getAuthenticity();

	/**
	 * Returns the value of the '<em><b>Base Instance Specification</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Instance Specification</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Instance Specification</em>' reference.
	 * @see #setBase_InstanceSpecification(InstanceSpecification)
	 * @see carisma.profile.umlsec.UmlsecPackage#getcritical_Base_InstanceSpecification()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	InstanceSpecification getBase_InstanceSpecification();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.critical#getBase_InstanceSpecification <em>Base Instance Specification</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Instance Specification</em>' reference.
	 * @see #getBase_InstanceSpecification()
	 * @generated
	 */
	void setBase_InstanceSpecification(InstanceSpecification value);

	/**
	 * Returns the value of the '<em><b>Base Classifier</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Classifier</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Classifier</em>' reference.
	 * @see #setBase_Classifier(Classifier)
	 * @see carisma.profile.umlsec.UmlsecPackage#getcritical_Base_Classifier()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Classifier getBase_Classifier();

	/**
	 * Sets the value of the '{@link carisma.profile.umlsec.critical#getBase_Classifier <em>Base Classifier</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Classifier</em>' reference.
	 * @see #getBase_Classifier()
	 * @generated
	 */
	void setBase_Classifier(Classifier value);

	/**
	 * Returns the value of the '<em><b>Privacy</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Privacy</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Privacy</em>' attribute list.
	 * @see carisma.profile.umlsec.UmlsecPackage#getcritical_Privacy()
	 * @model unique="false" dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getPrivacy();

} // critical
