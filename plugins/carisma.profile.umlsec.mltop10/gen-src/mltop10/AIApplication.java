/**
 */
package mltop10;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>AI Application</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link mltop10.AIApplication#getBase_Artifact <em>Base Artifact</em>}</li>
 *   <li>{@link mltop10.AIApplication#isCheckModelResultAuthenticity <em>Check Model Result Authenticity</em>}</li>
 *   <li>{@link mltop10.AIApplication#isInputValidation <em>Input Validation</em>}</li>
 *   <li>{@link mltop10.AIApplication#isTamperEvidentLogging <em>Tamper Evident Logging</em>}</li>
 *   <li>{@link mltop10.AIApplication#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}</li>
 * </ul>
 *
 * @see mltop10.Mltop10Package#getAIApplication()
 * @model
 * @generated
 */
public interface AIApplication extends EObject {
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
	 * @see mltop10.Mltop10Package#getAIApplication_Base_Artifact()
	 * @model ordered="false"
	 * @generated
	 */
	Artifact getBase_Artifact();

	/**
	 * Sets the value of the '{@link mltop10.AIApplication#getBase_Artifact <em>Base Artifact</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Artifact</em>' reference.
	 * @see #getBase_Artifact()
	 * @generated
	 */
	void setBase_Artifact(Artifact value);

	/**
	 * Returns the value of the '<em><b>Check Model Result Authenticity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Check Model Result Authenticity</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Check Model Result Authenticity</em>' attribute.
	 * @see #setCheckModelResultAuthenticity(boolean)
	 * @see mltop10.Mltop10Package#getAIApplication_CheckModelResultAuthenticity()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isCheckModelResultAuthenticity();

	/**
	 * Sets the value of the '{@link mltop10.AIApplication#isCheckModelResultAuthenticity <em>Check Model Result Authenticity</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Check Model Result Authenticity</em>' attribute.
	 * @see #isCheckModelResultAuthenticity()
	 * @generated
	 */
	void setCheckModelResultAuthenticity(boolean value);

	/**
	 * Returns the value of the '<em><b>Input Validation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Input Validation</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Input Validation</em>' attribute.
	 * @see #setInputValidation(boolean)
	 * @see mltop10.Mltop10Package#getAIApplication_InputValidation()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isInputValidation();

	/**
	 * Sets the value of the '{@link mltop10.AIApplication#isInputValidation <em>Input Validation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Input Validation</em>' attribute.
	 * @see #isInputValidation()
	 * @generated
	 */
	void setInputValidation(boolean value);

	/**
	 * Returns the value of the '<em><b>Tamper Evident Logging</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Tamper Evident Logging</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Tamper Evident Logging</em>' attribute.
	 * @see #setTamperEvidentLogging(boolean)
	 * @see mltop10.Mltop10Package#getAIApplication_TamperEvidentLogging()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isTamperEvidentLogging();

	/**
	 * Sets the value of the '{@link mltop10.AIApplication#isTamperEvidentLogging <em>Tamper Evident Logging</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Tamper Evident Logging</em>' attribute.
	 * @see #isTamperEvidentLogging()
	 * @generated
	 */
	void setTamperEvidentLogging(boolean value);

	/**
	 * Returns the value of the '<em><b>Regular Audit And Monitoring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Regular Audit And Monitoring</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Regular Audit And Monitoring</em>' attribute.
	 * @see #setRegularAuditAndMonitoring(boolean)
	 * @see mltop10.Mltop10Package#getAIApplication_RegularAuditAndMonitoring()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularAuditAndMonitoring();

	/**
	 * Sets the value of the '{@link mltop10.AIApplication#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Audit And Monitoring</em>' attribute.
	 * @see #isRegularAuditAndMonitoring()
	 * @generated
	 */
	void setRegularAuditAndMonitoring(boolean value);

} // AIApplication
