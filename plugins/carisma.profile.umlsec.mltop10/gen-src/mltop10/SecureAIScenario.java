/**
 */
package mltop10;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Model;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Secure AI Scenario</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link mltop10.SecureAIScenario#isPackageIntegrityVerified <em>Package Integrity Verified</em>}</li>
 *   <li>{@link mltop10.SecureAIScenario#isPackagesFromSecureSources <em>Packages From Secure Sources</em>}</li>
 *   <li>{@link mltop10.SecureAIScenario#isRegularSecurityAudits <em>Regular Security Audits</em>}</li>
 *   <li>{@link mltop10.SecureAIScenario#isRegularPackageUpdates <em>Regular Package Updates</em>}</li>
 *   <li>{@link mltop10.SecureAIScenario#isSecureDeployment <em>Secure Deployment</em>}</li>
 *   <li>{@link mltop10.SecureAIScenario#getBase_Package <em>Base Package</em>}</li>
 *   <li>{@link mltop10.SecureAIScenario#getBase_Model <em>Base Model</em>}</li>
 * </ul>
 *
 * @see mltop10.Mltop10Package#getSecureAIScenario()
 * @model
 * @generated
 */
public interface SecureAIScenario extends EObject {
	/**
	 * Returns the value of the '<em><b>Package Integrity Verified</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Package Integrity Verified</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Package Integrity Verified</em>' attribute.
	 * @see #setPackageIntegrityVerified(boolean)
	 * @see mltop10.Mltop10Package#getSecureAIScenario_PackageIntegrityVerified()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isPackageIntegrityVerified();

	/**
	 * Sets the value of the '{@link mltop10.SecureAIScenario#isPackageIntegrityVerified <em>Package Integrity Verified</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Package Integrity Verified</em>' attribute.
	 * @see #isPackageIntegrityVerified()
	 * @generated
	 */
	void setPackageIntegrityVerified(boolean value);

	/**
	 * Returns the value of the '<em><b>Packages From Secure Sources</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Packages From Secure Sources</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Packages From Secure Sources</em>' attribute.
	 * @see #setPackagesFromSecureSources(boolean)
	 * @see mltop10.Mltop10Package#getSecureAIScenario_PackagesFromSecureSources()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isPackagesFromSecureSources();

	/**
	 * Sets the value of the '{@link mltop10.SecureAIScenario#isPackagesFromSecureSources <em>Packages From Secure Sources</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Packages From Secure Sources</em>' attribute.
	 * @see #isPackagesFromSecureSources()
	 * @generated
	 */
	void setPackagesFromSecureSources(boolean value);

	/**
	 * Returns the value of the '<em><b>Regular Security Audits</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Regular Security Audits</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Regular Security Audits</em>' attribute.
	 * @see #setRegularSecurityAudits(boolean)
	 * @see mltop10.Mltop10Package#getSecureAIScenario_RegularSecurityAudits()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularSecurityAudits();

	/**
	 * Sets the value of the '{@link mltop10.SecureAIScenario#isRegularSecurityAudits <em>Regular Security Audits</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Security Audits</em>' attribute.
	 * @see #isRegularSecurityAudits()
	 * @generated
	 */
	void setRegularSecurityAudits(boolean value);

	/**
	 * Returns the value of the '<em><b>Regular Package Updates</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Regular Package Updates</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Regular Package Updates</em>' attribute.
	 * @see #setRegularPackageUpdates(boolean)
	 * @see mltop10.Mltop10Package#getSecureAIScenario_RegularPackageUpdates()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularPackageUpdates();

	/**
	 * Sets the value of the '{@link mltop10.SecureAIScenario#isRegularPackageUpdates <em>Regular Package Updates</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Package Updates</em>' attribute.
	 * @see #isRegularPackageUpdates()
	 * @generated
	 */
	void setRegularPackageUpdates(boolean value);

	/**
	 * Returns the value of the '<em><b>Secure Deployment</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Secure Deployment</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Secure Deployment</em>' attribute.
	 * @see #setSecureDeployment(boolean)
	 * @see mltop10.Mltop10Package#getSecureAIScenario_SecureDeployment()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isSecureDeployment();

	/**
	 * Sets the value of the '{@link mltop10.SecureAIScenario#isSecureDeployment <em>Secure Deployment</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Secure Deployment</em>' attribute.
	 * @see #isSecureDeployment()
	 * @generated
	 */
	void setSecureDeployment(boolean value);

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
	 * @see mltop10.Mltop10Package#getSecureAIScenario_Base_Package()
	 * @model ordered="false"
	 * @generated
	 */
	org.eclipse.uml2.uml.Package getBase_Package();

	/**
	 * Sets the value of the '{@link mltop10.SecureAIScenario#getBase_Package <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Package</em>' reference.
	 * @see #getBase_Package()
	 * @generated
	 */
	void setBase_Package(org.eclipse.uml2.uml.Package value);

	/**
	 * Returns the value of the '<em><b>Base Model</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Base Model</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Base Model</em>' reference.
	 * @see #setBase_Model(Model)
	 * @see mltop10.Mltop10Package#getSecureAIScenario_Base_Model()
	 * @model ordered="false"
	 * @generated
	 */
	Model getBase_Model();

	/**
	 * Sets the value of the '{@link mltop10.SecureAIScenario#getBase_Model <em>Base Model</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Model</em>' reference.
	 * @see #getBase_Model()
	 * @generated
	 */
	void setBase_Model(Model value);

} // SecureAIScenario
