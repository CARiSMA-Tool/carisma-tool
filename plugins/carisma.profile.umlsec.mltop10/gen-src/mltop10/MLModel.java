/**
 */
package mltop10;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>ML Model</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link mltop10.MLModel#getBase_Artifact <em>Base Artifact</em>}</li>
 *   <li>{@link mltop10.MLModel#isPublic <em>Public</em>}</li>
 *   <li>{@link mltop10.MLModel#isAccessControl <em>Access Control</em>}</li>
 *   <li>{@link mltop10.MLModel#isAdversarialTraining <em>Adversarial Training</em>}</li>
 *   <li>{@link mltop10.MLModel#isAnomalyDetection <em>Anomaly Detection</em>}</li>
 *   <li>{@link mltop10.MLModel#isCryptographicallySecured <em>Cryptographically Secured</em>}</li>
 *   <li>{@link mltop10.MLModel#isDefenseMechanism <em>Defense Mechanism</em>}</li>
 *   <li>{@link mltop10.MLModel#isDifferentialPrivacy <em>Differential Privacy</em>}</li>
 *   <li>{@link mltop10.MLModel#isEnsembleModel <em>Ensemble Model</em>}</li>
 *   <li>{@link mltop10.MLModel#isObfuscation <em>Obfuscation</em>}</li>
 *   <li>{@link mltop10.MLModel#isLegalProtection <em>Legal Protection</em>}</li>
 *   <li>{@link mltop10.MLModel#isRegularBackup <em>Regular Backup</em>}</li>
 *   <li>{@link mltop10.MLModel#isRegularRetraining <em>Regular Retraining</em>}</li>
 *   <li>{@link mltop10.MLModel#isRobustActivationFunction <em>Robust Activation Function</em>}</li>
 *   <li>{@link mltop10.MLModel#isRobustArchitecture <em>Robust Architecture</em>}</li>
 *   <li>{@link mltop10.MLModel#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}</li>
 *   <li>{@link mltop10.MLModel#isRegularPerformanceMonitoring <em>Regular Performance Monitoring</em>}</li>
 *   <li>{@link mltop10.MLModel#isRegularTestingAndMonitoring <em>Regular Testing And Monitoring</em>}</li>
 *   <li>{@link mltop10.MLModel#isTransparency <em>Transparency</em>}</li>
 *   <li>{@link mltop10.MLModel#isValidation <em>Validation</em>}</li>
 *   <li>{@link mltop10.MLModel#isWatermarking <em>Watermarking</em>}</li>
 * </ul>
 *
 * @see mltop10.Mltop10Package#getMLModel()
 * @model
 * @generated
 */
public interface MLModel extends EObject {
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
	 * @see mltop10.Mltop10Package#getMLModel_Base_Artifact()
	 * @model ordered="false"
	 * @generated
	 */
	Artifact getBase_Artifact();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#getBase_Artifact <em>Base Artifact</em>}' reference.
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
	 * @see mltop10.Mltop10Package#getMLModel_Public()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isPublic();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isPublic <em>Public</em>}' attribute.
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
	 * @see mltop10.Mltop10Package#getMLModel_AccessControl()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isAccessControl();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isAccessControl <em>Access Control</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Access Control</em>' attribute.
	 * @see #isAccessControl()
	 * @generated
	 */
	void setAccessControl(boolean value);

	/**
	 * Returns the value of the '<em><b>Adversarial Training</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Adversarial Training</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Adversarial Training</em>' attribute.
	 * @see #setAdversarialTraining(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_AdversarialTraining()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isAdversarialTraining();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isAdversarialTraining <em>Adversarial Training</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Adversarial Training</em>' attribute.
	 * @see #isAdversarialTraining()
	 * @generated
	 */
	void setAdversarialTraining(boolean value);

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
	 * @see mltop10.Mltop10Package#getMLModel_AnomalyDetection()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isAnomalyDetection();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isAnomalyDetection <em>Anomaly Detection</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Anomaly Detection</em>' attribute.
	 * @see #isAnomalyDetection()
	 * @generated
	 */
	void setAnomalyDetection(boolean value);

	/**
	 * Returns the value of the '<em><b>Cryptographically Secured</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Cryptographically Secured</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Cryptographically Secured</em>' attribute.
	 * @see #setCryptographicallySecured(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_CryptographicallySecured()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isCryptographicallySecured();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isCryptographicallySecured <em>Cryptographically Secured</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Cryptographically Secured</em>' attribute.
	 * @see #isCryptographicallySecured()
	 * @generated
	 */
	void setCryptographicallySecured(boolean value);

	/**
	 * Returns the value of the '<em><b>Defense Mechanism</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Defense Mechanism</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Defense Mechanism</em>' attribute.
	 * @see #setDefenseMechanism(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_DefenseMechanism()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isDefenseMechanism();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isDefenseMechanism <em>Defense Mechanism</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Defense Mechanism</em>' attribute.
	 * @see #isDefenseMechanism()
	 * @generated
	 */
	void setDefenseMechanism(boolean value);

	/**
	 * Returns the value of the '<em><b>Differential Privacy</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Differential Privacy</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Differential Privacy</em>' attribute.
	 * @see #setDifferentialPrivacy(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_DifferentialPrivacy()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isDifferentialPrivacy();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isDifferentialPrivacy <em>Differential Privacy</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Differential Privacy</em>' attribute.
	 * @see #isDifferentialPrivacy()
	 * @generated
	 */
	void setDifferentialPrivacy(boolean value);

	/**
	 * Returns the value of the '<em><b>Ensemble Model</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ensemble Model</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ensemble Model</em>' attribute.
	 * @see #setEnsembleModel(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_EnsembleModel()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isEnsembleModel();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isEnsembleModel <em>Ensemble Model</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ensemble Model</em>' attribute.
	 * @see #isEnsembleModel()
	 * @generated
	 */
	void setEnsembleModel(boolean value);

	/**
	 * Returns the value of the '<em><b>Obfuscation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Obfuscation</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Obfuscation</em>' attribute.
	 * @see #setObfuscation(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_Obfuscation()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isObfuscation();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isObfuscation <em>Obfuscation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Obfuscation</em>' attribute.
	 * @see #isObfuscation()
	 * @generated
	 */
	void setObfuscation(boolean value);

	/**
	 * Returns the value of the '<em><b>Legal Protection</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Legal Protection</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Legal Protection</em>' attribute.
	 * @see #setLegalProtection(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_LegalProtection()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isLegalProtection();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isLegalProtection <em>Legal Protection</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Legal Protection</em>' attribute.
	 * @see #isLegalProtection()
	 * @generated
	 */
	void setLegalProtection(boolean value);

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
	 * @see mltop10.Mltop10Package#getMLModel_RegularBackup()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularBackup();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isRegularBackup <em>Regular Backup</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Backup</em>' attribute.
	 * @see #isRegularBackup()
	 * @generated
	 */
	void setRegularBackup(boolean value);

	/**
	 * Returns the value of the '<em><b>Regular Retraining</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Regular Retraining</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Regular Retraining</em>' attribute.
	 * @see #setRegularRetraining(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_RegularRetraining()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularRetraining();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isRegularRetraining <em>Regular Retraining</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Retraining</em>' attribute.
	 * @see #isRegularRetraining()
	 * @generated
	 */
	void setRegularRetraining(boolean value);

	/**
	 * Returns the value of the '<em><b>Robust Activation Function</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Robust Activation Function</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Robust Activation Function</em>' attribute.
	 * @see #setRobustActivationFunction(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_RobustActivationFunction()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRobustActivationFunction();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isRobustActivationFunction <em>Robust Activation Function</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Robust Activation Function</em>' attribute.
	 * @see #isRobustActivationFunction()
	 * @generated
	 */
	void setRobustActivationFunction(boolean value);

	/**
	 * Returns the value of the '<em><b>Robust Architecture</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Robust Architecture</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Robust Architecture</em>' attribute.
	 * @see #setRobustArchitecture(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_RobustArchitecture()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRobustArchitecture();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isRobustArchitecture <em>Robust Architecture</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Robust Architecture</em>' attribute.
	 * @see #isRobustArchitecture()
	 * @generated
	 */
	void setRobustArchitecture(boolean value);

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
	 * @see mltop10.Mltop10Package#getMLModel_RegularAuditAndMonitoring()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularAuditAndMonitoring();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Audit And Monitoring</em>' attribute.
	 * @see #isRegularAuditAndMonitoring()
	 * @generated
	 */
	void setRegularAuditAndMonitoring(boolean value);

	/**
	 * Returns the value of the '<em><b>Regular Performance Monitoring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Regular Performance Monitoring</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Regular Performance Monitoring</em>' attribute.
	 * @see #setRegularPerformanceMonitoring(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_RegularPerformanceMonitoring()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularPerformanceMonitoring();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isRegularPerformanceMonitoring <em>Regular Performance Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Performance Monitoring</em>' attribute.
	 * @see #isRegularPerformanceMonitoring()
	 * @generated
	 */
	void setRegularPerformanceMonitoring(boolean value);

	/**
	 * Returns the value of the '<em><b>Regular Testing And Monitoring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Regular Testing And Monitoring</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Regular Testing And Monitoring</em>' attribute.
	 * @see #setRegularTestingAndMonitoring(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_RegularTestingAndMonitoring()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isRegularTestingAndMonitoring();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isRegularTestingAndMonitoring <em>Regular Testing And Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Regular Testing And Monitoring</em>' attribute.
	 * @see #isRegularTestingAndMonitoring()
	 * @generated
	 */
	void setRegularTestingAndMonitoring(boolean value);

	/**
	 * Returns the value of the '<em><b>Transparency</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Transparency</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Transparency</em>' attribute.
	 * @see #setTransparency(boolean)
	 * @see mltop10.Mltop10Package#getMLModel_Transparency()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isTransparency();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isTransparency <em>Transparency</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Transparency</em>' attribute.
	 * @see #isTransparency()
	 * @generated
	 */
	void setTransparency(boolean value);

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
	 * @see mltop10.Mltop10Package#getMLModel_Validation()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isValidation();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isValidation <em>Validation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Validation</em>' attribute.
	 * @see #isValidation()
	 * @generated
	 */
	void setValidation(boolean value);

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
	 * @see mltop10.Mltop10Package#getMLModel_Watermarking()
	 * @model dataType="org.eclipse.uml2.types.Boolean" required="true" ordered="false"
	 * @generated
	 */
	boolean isWatermarking();

	/**
	 * Sets the value of the '{@link mltop10.MLModel#isWatermarking <em>Watermarking</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Watermarking</em>' attribute.
	 * @see #isWatermarking()
	 * @generated
	 */
	void setWatermarking(boolean value);

} // MLModel
