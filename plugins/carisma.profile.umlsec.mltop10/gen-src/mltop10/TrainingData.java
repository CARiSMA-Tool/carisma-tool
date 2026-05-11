/**
 */
package mltop10;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Training Data</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link mltop10.TrainingData#getBase_Artifact <em>Base Artifact</em>}</li>
 *   <li>{@link mltop10.TrainingData#isPublic <em>Public</em>}</li>
 *   <li>{@link mltop10.TrainingData#isAccessControl <em>Access Control</em>}</li>
 *   <li>{@link mltop10.TrainingData#isAnomalyDetection <em>Anomaly Detection</em>}</li>
 *   <li>{@link mltop10.TrainingData#isReduced <em>Reduced</em>}</li>
 *   <li>{@link mltop10.TrainingData#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}</li>
 *   <li>{@link mltop10.TrainingData#isRegularUpdatesAndTraining <em>Regular Updates And Training</em>}</li>
 *   <li>{@link mltop10.TrainingData#isTrusted <em>Trusted</em>}</li>
 *   <li>{@link mltop10.TrainingData#isValidation <em>Validation</em>}</li>
 *   <li>{@link mltop10.TrainingData#isVerification <em>Verification</em>}</li>
 *   <li>{@link mltop10.TrainingData#isWatermarking <em>Watermarking</em>}</li>
 *   <li>{@link mltop10.TrainingData#isRegularBackup <em>Regular Backup</em>}</li>
 * </ul>
 *
 * @see mltop10.Mltop10Package#getTrainingData()
 * @model
 * @generated
 */
public interface TrainingData extends EObject {
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
	 * @see mltop10.Mltop10Package#getTrainingData_Base_Artifact()
	 * @model ordered="false"
	 * @generated
	 */
	Artifact getBase_Artifact();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#getBase_Artifact <em>Base Artifact</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Base Artifact</em>' reference.
	 * @see #getBase_Artifact()
	 * @generated
	 */
	void setBase_Artifact(Artifact value);

	/**
	 * Returns the value of the '<em><b>Public</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Public</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Public</em>' attribute.
	 * @see #setPublic(boolean)
	 * @see mltop10.Mltop10Package#getTrainingData_Public()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isPublic();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isPublic <em>Public</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Public</em>' attribute.
	 * @see #isPublic()
	 * @generated
	 */
	void setPublic(boolean value);

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
	 * @see mltop10.Mltop10Package#getTrainingData_AccessControl()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isAccessControl();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isAccessControl <em>Access Control</em>}' attribute.
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
	 * @see mltop10.Mltop10Package#getTrainingData_AnomalyDetection()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isAnomalyDetection();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isAnomalyDetection <em>Anomaly Detection</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Anomaly Detection</em>' attribute.
	 * @see #isAnomalyDetection()
	 * @generated
	 */
	void setAnomalyDetection(boolean value);

	/**
	 * Returns the value of the '<em><b>Reduced</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Reduced</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Reduced</em>' attribute.
	 * @see #setReduced(boolean)
	 * @see mltop10.Mltop10Package#getTrainingData_Reduced()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isReduced();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isReduced <em>Reduced</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Reduced</em>' attribute.
	 * @see #isReduced()
	 * @generated
	 */
	void setReduced(boolean value);

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
	 * @see mltop10.Mltop10Package#getTrainingData_RegularAuditAndMonitoring()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularAuditAndMonitoring();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Audit And Monitoring</em>' attribute.
	 * @see #isRegularAuditAndMonitoring()
	 * @generated
	 */
	void setRegularAuditAndMonitoring(boolean value);

	/**
	 * Returns the value of the '<em><b>Regular Updates And Training</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Regular Updates And Training</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Regular Updates And Training</em>' attribute.
	 * @see #setRegularUpdatesAndTraining(boolean)
	 * @see mltop10.Mltop10Package#getTrainingData_RegularUpdatesAndTraining()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularUpdatesAndTraining();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isRegularUpdatesAndTraining <em>Regular Updates And Training</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Updates And Training</em>' attribute.
	 * @see #isRegularUpdatesAndTraining()
	 * @generated
	 */
	void setRegularUpdatesAndTraining(boolean value);

	/**
	 * Returns the value of the '<em><b>Trusted</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Trusted</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Trusted</em>' attribute.
	 * @see #setTrusted(boolean)
	 * @see mltop10.Mltop10Package#getTrainingData_Trusted()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isTrusted();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isTrusted <em>Trusted</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Trusted</em>' attribute.
	 * @see #isTrusted()
	 * @generated
	 */
	void setTrusted(boolean value);

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
	 * @see mltop10.Mltop10Package#getTrainingData_Validation()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isValidation();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isValidation <em>Validation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Validation</em>' attribute.
	 * @see #isValidation()
	 * @generated
	 */
	void setValidation(boolean value);

	/**
	 * Returns the value of the '<em><b>Verification</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Verification</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Verification</em>' attribute.
	 * @see #setVerification(boolean)
	 * @see mltop10.Mltop10Package#getTrainingData_Verification()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isVerification();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isVerification <em>Verification</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Verification</em>' attribute.
	 * @see #isVerification()
	 * @generated
	 */
	void setVerification(boolean value);

	/**
	 * Returns the value of the '<em><b>Watermarking</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Watermarking</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Watermarking</em>' attribute.
	 * @see #setWatermarking(boolean)
	 * @see mltop10.Mltop10Package#getTrainingData_Watermarking()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isWatermarking();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isWatermarking <em>Watermarking</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Watermarking</em>' attribute.
	 * @see #isWatermarking()
	 * @generated
	 */
	void setWatermarking(boolean value);

	/**
	 * Returns the value of the '<em><b>Regular Backup</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Regular Backup</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Regular Backup</em>' attribute.
	 * @see #setRegularBackup(boolean)
	 * @see mltop10.Mltop10Package#getTrainingData_RegularBackup()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularBackup();

	/**
	 * Sets the value of the '{@link mltop10.TrainingData#isRegularBackup <em>Regular Backup</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Backup</em>' attribute.
	 * @see #isRegularBackup()
	 * @generated
	 */
	void setRegularBackup(boolean value);

} // TrainingData
