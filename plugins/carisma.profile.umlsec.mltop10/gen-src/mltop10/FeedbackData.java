/**
 */
package mltop10;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Feedback Data</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link mltop10.FeedbackData#getBase_Artifact <em>Base Artifact</em>}</li>
 *   <li>{@link mltop10.FeedbackData#isAccessControl <em>Access Control</em>}</li>
 *   <li>{@link mltop10.FeedbackData#isAnomalyDetection <em>Anomaly Detection</em>}</li>
 *   <li>{@link mltop10.FeedbackData#isAuthenticityVerified <em>Authenticity Verified</em>}</li>
 *   <li>{@link mltop10.FeedbackData#isCleaning <em>Cleaning</em>}</li>
 *   <li>{@link mltop10.FeedbackData#isValidation <em>Validation</em>}</li>
 * </ul>
 *
 * @see mltop10.Mltop10Package#getFeedbackData()
 * @model
 * @generated
 */
public interface FeedbackData extends EObject {
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
	 * @see mltop10.Mltop10Package#getFeedbackData_Base_Artifact()
	 * @model ordered="false"
	 * @generated
	 */
	Artifact getBase_Artifact();

	/**
	 * Sets the value of the '{@link mltop10.FeedbackData#getBase_Artifact <em>Base Artifact</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Artifact</em>' reference.
	 * @see #getBase_Artifact()
	 * @generated
	 */
	void setBase_Artifact(Artifact value);

	/**
	 * Returns the value of the '<em><b>Access Control</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Access Control</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Access Control</em>' attribute.
	 * @see #setAccessControl(boolean)
	 * @see mltop10.Mltop10Package#getFeedbackData_AccessControl()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isAccessControl();

	/**
	 * Sets the value of the '{@link mltop10.FeedbackData#isAccessControl <em>Access Control</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Access Control</em>' attribute.
	 * @see #isAccessControl()
	 * @generated
	 */
	void setAccessControl(boolean value);

	/**
	 * Returns the value of the '<em><b>Anomaly Detection</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Anomaly Detection</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Anomaly Detection</em>' attribute.
	 * @see #setAnomalyDetection(boolean)
	 * @see mltop10.Mltop10Package#getFeedbackData_AnomalyDetection()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isAnomalyDetection();

	/**
	 * Sets the value of the '{@link mltop10.FeedbackData#isAnomalyDetection <em>Anomaly Detection</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Anomaly Detection</em>' attribute.
	 * @see #isAnomalyDetection()
	 * @generated
	 */
	void setAnomalyDetection(boolean value);

	/**
	 * Returns the value of the '<em><b>Authenticity Verified</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Authenticity Verified</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Authenticity Verified</em>' attribute.
	 * @see #setAuthenticityVerified(boolean)
	 * @see mltop10.Mltop10Package#getFeedbackData_AuthenticityVerified()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isAuthenticityVerified();

	/**
	 * Sets the value of the '{@link mltop10.FeedbackData#isAuthenticityVerified <em>Authenticity Verified</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Authenticity Verified</em>' attribute.
	 * @see #isAuthenticityVerified()
	 * @generated
	 */
	void setAuthenticityVerified(boolean value);

	/**
	 * Returns the value of the '<em><b>Cleaning</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Cleaning</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Cleaning</em>' attribute.
	 * @see #setCleaning(boolean)
	 * @see mltop10.Mltop10Package#getFeedbackData_Cleaning()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isCleaning();

	/**
	 * Sets the value of the '{@link mltop10.FeedbackData#isCleaning <em>Cleaning</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Cleaning</em>' attribute.
	 * @see #isCleaning()
	 * @generated
	 */
	void setCleaning(boolean value);

	/**
	 * Returns the value of the '<em><b>Validation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Validation</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Validation</em>' attribute.
	 * @see #setValidation(boolean)
	 * @see mltop10.Mltop10Package#getFeedbackData_Validation()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isValidation();

	/**
	 * Sets the value of the '{@link mltop10.FeedbackData#isValidation <em>Validation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Validation</em>' attribute.
	 * @see #isValidation()
	 * @generated
	 */
	void setValidation(boolean value);

} // FeedbackData
