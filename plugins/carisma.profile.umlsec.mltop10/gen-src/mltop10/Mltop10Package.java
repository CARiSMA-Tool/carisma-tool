/**
 */
package mltop10;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see mltop10.Mltop10Factory
 * @model kind="package"
 * @generated
 */
public interface Mltop10Package extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "mltop10";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http:///mltop10.ecore";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "mltop10";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	Mltop10Package eINSTANCE = mltop10.impl.Mltop10PackageImpl.init();

	/**
	 * The meta object id for the '{@link mltop10.impl.MLModelImpl <em>ML Model</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see mltop10.impl.MLModelImpl
	 * @see mltop10.impl.Mltop10PackageImpl#getMLModel()
	 * @generated
	 */
	int ML_MODEL = 0;

	/**
	 * The feature id for the '<em><b>Base Artifact</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__BASE_ARTIFACT = 0;

	/**
	 * The feature id for the '<em><b>Public</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__PUBLIC = 1;

	/**
	 * The feature id for the '<em><b>Access Control</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__ACCESS_CONTROL = 2;

	/**
	 * The feature id for the '<em><b>Adversarial Training</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__ADVERSARIAL_TRAINING = 3;

	/**
	 * The feature id for the '<em><b>Anomaly Detection</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__ANOMALY_DETECTION = 4;

	/**
	 * The feature id for the '<em><b>Cryptographically Secured</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__CRYPTOGRAPHICALLY_SECURED = 5;

	/**
	 * The feature id for the '<em><b>Defense Mechanism</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__DEFENSE_MECHANISM = 6;

	/**
	 * The feature id for the '<em><b>Differential Privacy</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__DIFFERENTIAL_PRIVACY = 7;

	/**
	 * The feature id for the '<em><b>Ensemble Model</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__ENSEMBLE_MODEL = 8;

	/**
	 * The feature id for the '<em><b>Obfuscation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__OBFUSCATION = 9;

	/**
	 * The feature id for the '<em><b>Legal Protection</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__LEGAL_PROTECTION = 10;

	/**
	 * The feature id for the '<em><b>Regular Backup</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__REGULAR_BACKUP = 11;

	/**
	 * The feature id for the '<em><b>Regular Retraining</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__REGULAR_RETRAINING = 12;

	/**
	 * The feature id for the '<em><b>Robust Activation Function</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__ROBUST_ACTIVATION_FUNCTION = 13;

	/**
	 * The feature id for the '<em><b>Robust Architecture</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__ROBUST_ARCHITECTURE = 14;

	/**
	 * The feature id for the '<em><b>Regular Audit And Monitoring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__REGULAR_AUDIT_AND_MONITORING = 15;

	/**
	 * The feature id for the '<em><b>Regular Performance Monitoring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__REGULAR_PERFORMANCE_MONITORING = 16;

	/**
	 * The feature id for the '<em><b>Regular Testing And Monitoring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__REGULAR_TESTING_AND_MONITORING = 17;

	/**
	 * The feature id for the '<em><b>Transparency</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__TRANSPARENCY = 18;

	/**
	 * The feature id for the '<em><b>Validation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__VALIDATION = 19;

	/**
	 * The feature id for the '<em><b>Watermarking</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL__WATERMARKING = 20;

	/**
	 * The number of structural features of the '<em>ML Model</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL_FEATURE_COUNT = 21;

	/**
	 * The number of operations of the '<em>ML Model</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ML_MODEL_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link mltop10.impl.AIAlgorithmImpl <em>AI Algorithm</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see mltop10.impl.AIAlgorithmImpl
	 * @see mltop10.impl.Mltop10PackageImpl#getAIAlgorithm()
	 * @generated
	 */
	int AI_ALGORITHM = 1;

	/**
	 * The feature id for the '<em><b>Base Artifact</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_ALGORITHM__BASE_ARTIFACT = 0;

	/**
	 * The feature id for the '<em><b>Public</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_ALGORITHM__PUBLIC = 1;

	/**
	 * The feature id for the '<em><b>Access Control</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_ALGORITHM__ACCESS_CONTROL = 2;

	/**
	 * The feature id for the '<em><b>Randomize</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_ALGORITHM__RANDOMIZE = 3;

	/**
	 * The feature id for the '<em><b>Regularisation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_ALGORITHM__REGULARISATION = 4;

	/**
	 * The number of structural features of the '<em>AI Algorithm</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_ALGORITHM_FEATURE_COUNT = 5;

	/**
	 * The number of operations of the '<em>AI Algorithm</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_ALGORITHM_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link mltop10.impl.TrainingDataImpl <em>Training Data</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see mltop10.impl.TrainingDataImpl
	 * @see mltop10.impl.Mltop10PackageImpl#getTrainingData()
	 * @generated
	 */
	int TRAINING_DATA = 2;

	/**
	 * The feature id for the '<em><b>Base Artifact</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__BASE_ARTIFACT = 0;

	/**
	 * The feature id for the '<em><b>Public</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__PUBLIC = 1;

	/**
	 * The feature id for the '<em><b>Access Control</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__ACCESS_CONTROL = 2;

	/**
	 * The feature id for the '<em><b>Anomaly Detection</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__ANOMALY_DETECTION = 3;

	/**
	 * The feature id for the '<em><b>Reduced</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__REDUCED = 4;

	/**
	 * The feature id for the '<em><b>Regular Audit And Monitoring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__REGULAR_AUDIT_AND_MONITORING = 5;

	/**
	 * The feature id for the '<em><b>Regular Updates And Training</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__REGULAR_UPDATES_AND_TRAINING = 6;

	/**
	 * The feature id for the '<em><b>Trusted</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__TRUSTED = 7;

	/**
	 * The feature id for the '<em><b>Validation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__VALIDATION = 8;

	/**
	 * The feature id for the '<em><b>Verification</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__VERIFICATION = 9;

	/**
	 * The feature id for the '<em><b>Watermarking</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__WATERMARKING = 10;

	/**
	 * The feature id for the '<em><b>Regular Backup</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA__REGULAR_BACKUP = 11;

	/**
	 * The number of structural features of the '<em>Training Data</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA_FEATURE_COUNT = 12;

	/**
	 * The number of operations of the '<em>Training Data</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link mltop10.impl.AIApplicationImpl <em>AI Application</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see mltop10.impl.AIApplicationImpl
	 * @see mltop10.impl.Mltop10PackageImpl#getAIApplication()
	 * @generated
	 */
	int AI_APPLICATION = 3;

	/**
	 * The feature id for the '<em><b>Base Artifact</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_APPLICATION__BASE_ARTIFACT = 0;

	/**
	 * The feature id for the '<em><b>Check Model Result Authenticity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_APPLICATION__CHECK_MODEL_RESULT_AUTHENTICITY = 1;

	/**
	 * The feature id for the '<em><b>Input Validation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_APPLICATION__INPUT_VALIDATION = 2;

	/**
	 * The feature id for the '<em><b>Tamper Evident Logging</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_APPLICATION__TAMPER_EVIDENT_LOGGING = 3;

	/**
	 * The feature id for the '<em><b>Regular Audit And Monitoring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_APPLICATION__REGULAR_AUDIT_AND_MONITORING = 4;

	/**
	 * The number of structural features of the '<em>AI Application</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_APPLICATION_FEATURE_COUNT = 5;

	/**
	 * The number of operations of the '<em>AI Application</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AI_APPLICATION_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link mltop10.impl.TrainingDataServerImpl <em>Training Data Server</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see mltop10.impl.TrainingDataServerImpl
	 * @see mltop10.impl.Mltop10PackageImpl#getTrainingDataServer()
	 * @generated
	 */
	int TRAINING_DATA_SERVER = 4;

	/**
	 * The feature id for the '<em><b>Base Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA_SERVER__BASE_NODE = 0;

	/**
	 * The feature id for the '<em><b>Secure Data Storage</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA_SERVER__SECURE_DATA_STORAGE = 1;

	/**
	 * The number of structural features of the '<em>Training Data Server</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA_SERVER_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>Training Data Server</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRAINING_DATA_SERVER_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link mltop10.impl.SecureAIScenarioImpl <em>Secure AI Scenario</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see mltop10.impl.SecureAIScenarioImpl
	 * @see mltop10.impl.Mltop10PackageImpl#getSecureAIScenario()
	 * @generated
	 */
	int SECURE_AI_SCENARIO = 5;

	/**
	 * The feature id for the '<em><b>Package Integrity Verified</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_AI_SCENARIO__PACKAGE_INTEGRITY_VERIFIED = 0;

	/**
	 * The feature id for the '<em><b>Packages From Secure Sources</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_AI_SCENARIO__PACKAGES_FROM_SECURE_SOURCES = 1;

	/**
	 * The feature id for the '<em><b>Regular Security Audits</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_AI_SCENARIO__REGULAR_SECURITY_AUDITS = 2;

	/**
	 * The feature id for the '<em><b>Regular Package Updates</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_AI_SCENARIO__REGULAR_PACKAGE_UPDATES = 3;

	/**
	 * The feature id for the '<em><b>Secure Deployment</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_AI_SCENARIO__SECURE_DEPLOYMENT = 4;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_AI_SCENARIO__BASE_PACKAGE = 5;

	/**
	 * The feature id for the '<em><b>Base Model</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_AI_SCENARIO__BASE_MODEL = 6;

	/**
	 * The number of structural features of the '<em>Secure AI Scenario</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_AI_SCENARIO_FEATURE_COUNT = 7;

	/**
	 * The number of operations of the '<em>Secure AI Scenario</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_AI_SCENARIO_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link mltop10.impl.FeedbackDataImpl <em>Feedback Data</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see mltop10.impl.FeedbackDataImpl
	 * @see mltop10.impl.Mltop10PackageImpl#getFeedbackData()
	 * @generated
	 */
	int FEEDBACK_DATA = 6;

	/**
	 * The feature id for the '<em><b>Base Artifact</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FEEDBACK_DATA__BASE_ARTIFACT = 0;

	/**
	 * The feature id for the '<em><b>Access Control</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FEEDBACK_DATA__ACCESS_CONTROL = 1;

	/**
	 * The feature id for the '<em><b>Anomaly Detection</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FEEDBACK_DATA__ANOMALY_DETECTION = 2;

	/**
	 * The feature id for the '<em><b>Authenticity Verified</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FEEDBACK_DATA__AUTHENTICITY_VERIFIED = 3;

	/**
	 * The feature id for the '<em><b>Cleaning</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FEEDBACK_DATA__CLEANING = 4;

	/**
	 * The feature id for the '<em><b>Validation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FEEDBACK_DATA__VALIDATION = 5;

	/**
	 * The number of structural features of the '<em>Feedback Data</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FEEDBACK_DATA_FEATURE_COUNT = 6;

	/**
	 * The number of operations of the '<em>Feedback Data</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FEEDBACK_DATA_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link mltop10.impl.SecureCommPathImpl <em>Secure Comm Path</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see mltop10.impl.SecureCommPathImpl
	 * @see mltop10.impl.Mltop10PackageImpl#getSecureCommPath()
	 * @generated
	 */
	int SECURE_COMM_PATH = 7;

	/**
	 * The feature id for the '<em><b>Base Communication Path</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_COMM_PATH__BASE_COMMUNICATION_PATH = 0;

	/**
	 * The feature id for the '<em><b>Confidelity Preserving</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_COMM_PATH__CONFIDELITY_PRESERVING = 1;

	/**
	 * The feature id for the '<em><b>Integrity Preserving</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_COMM_PATH__INTEGRITY_PRESERVING = 2;

	/**
	 * The number of structural features of the '<em>Secure Comm Path</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_COMM_PATH_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>Secure Comm Path</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURE_COMM_PATH_OPERATION_COUNT = 0;


	/**
	 * Returns the meta object for class '{@link mltop10.MLModel <em>ML Model</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>ML Model</em>'.
	 * @see mltop10.MLModel
	 * @generated
	 */
	EClass getMLModel();

	/**
	 * Returns the meta object for the reference '{@link mltop10.MLModel#getBase_Artifact <em>Base Artifact</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Artifact</em>'.
	 * @see mltop10.MLModel#getBase_Artifact()
	 * @see #getMLModel()
	 * @generated
	 */
	EReference getMLModel_Base_Artifact();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isPublic <em>Public</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Public</em>'.
	 * @see mltop10.MLModel#isPublic()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_Public();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isAccessControl <em>Access Control</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Access Control</em>'.
	 * @see mltop10.MLModel#isAccessControl()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_AccessControl();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isAdversarialTraining <em>Adversarial Training</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Adversarial Training</em>'.
	 * @see mltop10.MLModel#isAdversarialTraining()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_AdversarialTraining();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isAnomalyDetection <em>Anomaly Detection</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Anomaly Detection</em>'.
	 * @see mltop10.MLModel#isAnomalyDetection()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_AnomalyDetection();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isCryptographicallySecured <em>Cryptographically Secured</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cryptographically Secured</em>'.
	 * @see mltop10.MLModel#isCryptographicallySecured()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_CryptographicallySecured();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isDefenseMechanism <em>Defense Mechanism</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Defense Mechanism</em>'.
	 * @see mltop10.MLModel#isDefenseMechanism()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_DefenseMechanism();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isDifferentialPrivacy <em>Differential Privacy</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Differential Privacy</em>'.
	 * @see mltop10.MLModel#isDifferentialPrivacy()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_DifferentialPrivacy();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isEnsembleModel <em>Ensemble Model</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Ensemble Model</em>'.
	 * @see mltop10.MLModel#isEnsembleModel()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_EnsembleModel();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isObfuscation <em>Obfuscation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Obfuscation</em>'.
	 * @see mltop10.MLModel#isObfuscation()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_Obfuscation();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isLegalProtection <em>Legal Protection</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Legal Protection</em>'.
	 * @see mltop10.MLModel#isLegalProtection()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_LegalProtection();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isRegularBackup <em>Regular Backup</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Backup</em>'.
	 * @see mltop10.MLModel#isRegularBackup()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_RegularBackup();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isRegularRetraining <em>Regular Retraining</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Retraining</em>'.
	 * @see mltop10.MLModel#isRegularRetraining()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_RegularRetraining();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isRobustActivationFunction <em>Robust Activation Function</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Robust Activation Function</em>'.
	 * @see mltop10.MLModel#isRobustActivationFunction()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_RobustActivationFunction();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isRobustArchitecture <em>Robust Architecture</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Robust Architecture</em>'.
	 * @see mltop10.MLModel#isRobustArchitecture()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_RobustArchitecture();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Audit And Monitoring</em>'.
	 * @see mltop10.MLModel#isRegularAuditAndMonitoring()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_RegularAuditAndMonitoring();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isRegularPerformanceMonitoring <em>Regular Performance Monitoring</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Performance Monitoring</em>'.
	 * @see mltop10.MLModel#isRegularPerformanceMonitoring()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_RegularPerformanceMonitoring();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isRegularTestingAndMonitoring <em>Regular Testing And Monitoring</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Testing And Monitoring</em>'.
	 * @see mltop10.MLModel#isRegularTestingAndMonitoring()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_RegularTestingAndMonitoring();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isTransparency <em>Transparency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Transparency</em>'.
	 * @see mltop10.MLModel#isTransparency()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_Transparency();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isValidation <em>Validation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Validation</em>'.
	 * @see mltop10.MLModel#isValidation()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_Validation();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.MLModel#isWatermarking <em>Watermarking</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Watermarking</em>'.
	 * @see mltop10.MLModel#isWatermarking()
	 * @see #getMLModel()
	 * @generated
	 */
	EAttribute getMLModel_Watermarking();

	/**
	 * Returns the meta object for class '{@link mltop10.AIAlgorithm <em>AI Algorithm</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>AI Algorithm</em>'.
	 * @see mltop10.AIAlgorithm
	 * @generated
	 */
	EClass getAIAlgorithm();

	/**
	 * Returns the meta object for the reference '{@link mltop10.AIAlgorithm#getBase_Artifact <em>Base Artifact</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Artifact</em>'.
	 * @see mltop10.AIAlgorithm#getBase_Artifact()
	 * @see #getAIAlgorithm()
	 * @generated
	 */
	EReference getAIAlgorithm_Base_Artifact();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.AIAlgorithm#isPublic <em>Public</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Public</em>'.
	 * @see mltop10.AIAlgorithm#isPublic()
	 * @see #getAIAlgorithm()
	 * @generated
	 */
	EAttribute getAIAlgorithm_Public();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.AIAlgorithm#isAccessControl <em>Access Control</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Access Control</em>'.
	 * @see mltop10.AIAlgorithm#isAccessControl()
	 * @see #getAIAlgorithm()
	 * @generated
	 */
	EAttribute getAIAlgorithm_AccessControl();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.AIAlgorithm#isRandomize <em>Randomize</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Randomize</em>'.
	 * @see mltop10.AIAlgorithm#isRandomize()
	 * @see #getAIAlgorithm()
	 * @generated
	 */
	EAttribute getAIAlgorithm_Randomize();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.AIAlgorithm#isRegularisation <em>Regularisation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regularisation</em>'.
	 * @see mltop10.AIAlgorithm#isRegularisation()
	 * @see #getAIAlgorithm()
	 * @generated
	 */
	EAttribute getAIAlgorithm_Regularisation();

	/**
	 * Returns the meta object for class '{@link mltop10.TrainingData <em>Training Data</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Training Data</em>'.
	 * @see mltop10.TrainingData
	 * @generated
	 */
	EClass getTrainingData();

	/**
	 * Returns the meta object for the reference '{@link mltop10.TrainingData#getBase_Artifact <em>Base Artifact</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Artifact</em>'.
	 * @see mltop10.TrainingData#getBase_Artifact()
	 * @see #getTrainingData()
	 * @generated
	 */
	EReference getTrainingData_Base_Artifact();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isPublic <em>Public</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Public</em>'.
	 * @see mltop10.TrainingData#isPublic()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_Public();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isAccessControl <em>Access Control</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Access Control</em>'.
	 * @see mltop10.TrainingData#isAccessControl()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_AccessControl();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isAnomalyDetection <em>Anomaly Detection</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Anomaly Detection</em>'.
	 * @see mltop10.TrainingData#isAnomalyDetection()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_AnomalyDetection();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isReduced <em>Reduced</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Reduced</em>'.
	 * @see mltop10.TrainingData#isReduced()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_Reduced();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Audit And Monitoring</em>'.
	 * @see mltop10.TrainingData#isRegularAuditAndMonitoring()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_RegularAuditAndMonitoring();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isRegularUpdatesAndTraining <em>Regular Updates And Training</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Updates And Training</em>'.
	 * @see mltop10.TrainingData#isRegularUpdatesAndTraining()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_RegularUpdatesAndTraining();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isTrusted <em>Trusted</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Trusted</em>'.
	 * @see mltop10.TrainingData#isTrusted()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_Trusted();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isValidation <em>Validation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Validation</em>'.
	 * @see mltop10.TrainingData#isValidation()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_Validation();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isVerification <em>Verification</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Verification</em>'.
	 * @see mltop10.TrainingData#isVerification()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_Verification();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isWatermarking <em>Watermarking</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Watermarking</em>'.
	 * @see mltop10.TrainingData#isWatermarking()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_Watermarking();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingData#isRegularBackup <em>Regular Backup</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Backup</em>'.
	 * @see mltop10.TrainingData#isRegularBackup()
	 * @see #getTrainingData()
	 * @generated
	 */
	EAttribute getTrainingData_RegularBackup();

	/**
	 * Returns the meta object for class '{@link mltop10.AIApplication <em>AI Application</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>AI Application</em>'.
	 * @see mltop10.AIApplication
	 * @generated
	 */
	EClass getAIApplication();

	/**
	 * Returns the meta object for the reference '{@link mltop10.AIApplication#getBase_Artifact <em>Base Artifact</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Artifact</em>'.
	 * @see mltop10.AIApplication#getBase_Artifact()
	 * @see #getAIApplication()
	 * @generated
	 */
	EReference getAIApplication_Base_Artifact();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.AIApplication#isCheckModelResultAuthenticity <em>Check Model Result Authenticity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Check Model Result Authenticity</em>'.
	 * @see mltop10.AIApplication#isCheckModelResultAuthenticity()
	 * @see #getAIApplication()
	 * @generated
	 */
	EAttribute getAIApplication_CheckModelResultAuthenticity();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.AIApplication#isInputValidation <em>Input Validation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Input Validation</em>'.
	 * @see mltop10.AIApplication#isInputValidation()
	 * @see #getAIApplication()
	 * @generated
	 */
	EAttribute getAIApplication_InputValidation();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.AIApplication#isTamperEvidentLogging <em>Tamper Evident Logging</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Tamper Evident Logging</em>'.
	 * @see mltop10.AIApplication#isTamperEvidentLogging()
	 * @see #getAIApplication()
	 * @generated
	 */
	EAttribute getAIApplication_TamperEvidentLogging();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.AIApplication#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Audit And Monitoring</em>'.
	 * @see mltop10.AIApplication#isRegularAuditAndMonitoring()
	 * @see #getAIApplication()
	 * @generated
	 */
	EAttribute getAIApplication_RegularAuditAndMonitoring();

	/**
	 * Returns the meta object for class '{@link mltop10.TrainingDataServer <em>Training Data Server</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Training Data Server</em>'.
	 * @see mltop10.TrainingDataServer
	 * @generated
	 */
	EClass getTrainingDataServer();

	/**
	 * Returns the meta object for the reference '{@link mltop10.TrainingDataServer#getBase_Node <em>Base Node</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Node</em>'.
	 * @see mltop10.TrainingDataServer#getBase_Node()
	 * @see #getTrainingDataServer()
	 * @generated
	 */
	EReference getTrainingDataServer_Base_Node();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.TrainingDataServer#isSecureDataStorage <em>Secure Data Storage</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Secure Data Storage</em>'.
	 * @see mltop10.TrainingDataServer#isSecureDataStorage()
	 * @see #getTrainingDataServer()
	 * @generated
	 */
	EAttribute getTrainingDataServer_SecureDataStorage();

	/**
	 * Returns the meta object for class '{@link mltop10.SecureAIScenario <em>Secure AI Scenario</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Secure AI Scenario</em>'.
	 * @see mltop10.SecureAIScenario
	 * @generated
	 */
	EClass getSecureAIScenario();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.SecureAIScenario#isPackageIntegrityVerified <em>Package Integrity Verified</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Package Integrity Verified</em>'.
	 * @see mltop10.SecureAIScenario#isPackageIntegrityVerified()
	 * @see #getSecureAIScenario()
	 * @generated
	 */
	EAttribute getSecureAIScenario_PackageIntegrityVerified();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.SecureAIScenario#isPackagesFromSecureSources <em>Packages From Secure Sources</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Packages From Secure Sources</em>'.
	 * @see mltop10.SecureAIScenario#isPackagesFromSecureSources()
	 * @see #getSecureAIScenario()
	 * @generated
	 */
	EAttribute getSecureAIScenario_PackagesFromSecureSources();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.SecureAIScenario#isRegularSecurityAudits <em>Regular Security Audits</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Security Audits</em>'.
	 * @see mltop10.SecureAIScenario#isRegularSecurityAudits()
	 * @see #getSecureAIScenario()
	 * @generated
	 */
	EAttribute getSecureAIScenario_RegularSecurityAudits();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.SecureAIScenario#isRegularPackageUpdates <em>Regular Package Updates</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Regular Package Updates</em>'.
	 * @see mltop10.SecureAIScenario#isRegularPackageUpdates()
	 * @see #getSecureAIScenario()
	 * @generated
	 */
	EAttribute getSecureAIScenario_RegularPackageUpdates();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.SecureAIScenario#isSecureDeployment <em>Secure Deployment</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Secure Deployment</em>'.
	 * @see mltop10.SecureAIScenario#isSecureDeployment()
	 * @see #getSecureAIScenario()
	 * @generated
	 */
	EAttribute getSecureAIScenario_SecureDeployment();

	/**
	 * Returns the meta object for the reference '{@link mltop10.SecureAIScenario#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see mltop10.SecureAIScenario#getBase_Package()
	 * @see #getSecureAIScenario()
	 * @generated
	 */
	EReference getSecureAIScenario_Base_Package();

	/**
	 * Returns the meta object for the reference '{@link mltop10.SecureAIScenario#getBase_Model <em>Base Model</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Model</em>'.
	 * @see mltop10.SecureAIScenario#getBase_Model()
	 * @see #getSecureAIScenario()
	 * @generated
	 */
	EReference getSecureAIScenario_Base_Model();

	/**
	 * Returns the meta object for class '{@link mltop10.FeedbackData <em>Feedback Data</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Feedback Data</em>'.
	 * @see mltop10.FeedbackData
	 * @generated
	 */
	EClass getFeedbackData();

	/**
	 * Returns the meta object for the reference '{@link mltop10.FeedbackData#getBase_Artifact <em>Base Artifact</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Artifact</em>'.
	 * @see mltop10.FeedbackData#getBase_Artifact()
	 * @see #getFeedbackData()
	 * @generated
	 */
	EReference getFeedbackData_Base_Artifact();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.FeedbackData#isAccessControl <em>Access Control</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Access Control</em>'.
	 * @see mltop10.FeedbackData#isAccessControl()
	 * @see #getFeedbackData()
	 * @generated
	 */
	EAttribute getFeedbackData_AccessControl();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.FeedbackData#isAnomalyDetection <em>Anomaly Detection</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Anomaly Detection</em>'.
	 * @see mltop10.FeedbackData#isAnomalyDetection()
	 * @see #getFeedbackData()
	 * @generated
	 */
	EAttribute getFeedbackData_AnomalyDetection();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.FeedbackData#isAuthenticityVerified <em>Authenticity Verified</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Authenticity Verified</em>'.
	 * @see mltop10.FeedbackData#isAuthenticityVerified()
	 * @see #getFeedbackData()
	 * @generated
	 */
	EAttribute getFeedbackData_AuthenticityVerified();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.FeedbackData#isCleaning <em>Cleaning</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cleaning</em>'.
	 * @see mltop10.FeedbackData#isCleaning()
	 * @see #getFeedbackData()
	 * @generated
	 */
	EAttribute getFeedbackData_Cleaning();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.FeedbackData#isValidation <em>Validation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Validation</em>'.
	 * @see mltop10.FeedbackData#isValidation()
	 * @see #getFeedbackData()
	 * @generated
	 */
	EAttribute getFeedbackData_Validation();

	/**
	 * Returns the meta object for class '{@link mltop10.SecureCommPath <em>Secure Comm Path</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Secure Comm Path</em>'.
	 * @see mltop10.SecureCommPath
	 * @generated
	 */
	EClass getSecureCommPath();

	/**
	 * Returns the meta object for the reference '{@link mltop10.SecureCommPath#getBase_CommunicationPath <em>Base Communication Path</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Communication Path</em>'.
	 * @see mltop10.SecureCommPath#getBase_CommunicationPath()
	 * @see #getSecureCommPath()
	 * @generated
	 */
	EReference getSecureCommPath_Base_CommunicationPath();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.SecureCommPath#isConfidelityPreserving <em>Confidelity Preserving</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Confidelity Preserving</em>'.
	 * @see mltop10.SecureCommPath#isConfidelityPreserving()
	 * @see #getSecureCommPath()
	 * @generated
	 */
	EAttribute getSecureCommPath_ConfidelityPreserving();

	/**
	 * Returns the meta object for the attribute '{@link mltop10.SecureCommPath#isIntegrityPreserving <em>Integrity Preserving</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Integrity Preserving</em>'.
	 * @see mltop10.SecureCommPath#isIntegrityPreserving()
	 * @see #getSecureCommPath()
	 * @generated
	 */
	EAttribute getSecureCommPath_IntegrityPreserving();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	Mltop10Factory getMltop10Factory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each operation of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link mltop10.impl.MLModelImpl <em>ML Model</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see mltop10.impl.MLModelImpl
		 * @see mltop10.impl.Mltop10PackageImpl#getMLModel()
		 * @generated
		 */
		EClass ML_MODEL = eINSTANCE.getMLModel();

		/**
		 * The meta object literal for the '<em><b>Base Artifact</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ML_MODEL__BASE_ARTIFACT = eINSTANCE.getMLModel_Base_Artifact();

		/**
		 * The meta object literal for the '<em><b>Public</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__PUBLIC = eINSTANCE.getMLModel_Public();

		/**
		 * The meta object literal for the '<em><b>Access Control</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__ACCESS_CONTROL = eINSTANCE.getMLModel_AccessControl();

		/**
		 * The meta object literal for the '<em><b>Adversarial Training</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__ADVERSARIAL_TRAINING = eINSTANCE.getMLModel_AdversarialTraining();

		/**
		 * The meta object literal for the '<em><b>Anomaly Detection</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__ANOMALY_DETECTION = eINSTANCE.getMLModel_AnomalyDetection();

		/**
		 * The meta object literal for the '<em><b>Cryptographically Secured</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__CRYPTOGRAPHICALLY_SECURED = eINSTANCE.getMLModel_CryptographicallySecured();

		/**
		 * The meta object literal for the '<em><b>Defense Mechanism</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__DEFENSE_MECHANISM = eINSTANCE.getMLModel_DefenseMechanism();

		/**
		 * The meta object literal for the '<em><b>Differential Privacy</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__DIFFERENTIAL_PRIVACY = eINSTANCE.getMLModel_DifferentialPrivacy();

		/**
		 * The meta object literal for the '<em><b>Ensemble Model</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__ENSEMBLE_MODEL = eINSTANCE.getMLModel_EnsembleModel();

		/**
		 * The meta object literal for the '<em><b>Obfuscation</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__OBFUSCATION = eINSTANCE.getMLModel_Obfuscation();

		/**
		 * The meta object literal for the '<em><b>Legal Protection</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__LEGAL_PROTECTION = eINSTANCE.getMLModel_LegalProtection();

		/**
		 * The meta object literal for the '<em><b>Regular Backup</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__REGULAR_BACKUP = eINSTANCE.getMLModel_RegularBackup();

		/**
		 * The meta object literal for the '<em><b>Regular Retraining</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__REGULAR_RETRAINING = eINSTANCE.getMLModel_RegularRetraining();

		/**
		 * The meta object literal for the '<em><b>Robust Activation Function</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__ROBUST_ACTIVATION_FUNCTION = eINSTANCE.getMLModel_RobustActivationFunction();

		/**
		 * The meta object literal for the '<em><b>Robust Architecture</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__ROBUST_ARCHITECTURE = eINSTANCE.getMLModel_RobustArchitecture();

		/**
		 * The meta object literal for the '<em><b>Regular Audit And Monitoring</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__REGULAR_AUDIT_AND_MONITORING = eINSTANCE.getMLModel_RegularAuditAndMonitoring();

		/**
		 * The meta object literal for the '<em><b>Regular Performance Monitoring</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__REGULAR_PERFORMANCE_MONITORING = eINSTANCE.getMLModel_RegularPerformanceMonitoring();

		/**
		 * The meta object literal for the '<em><b>Regular Testing And Monitoring</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__REGULAR_TESTING_AND_MONITORING = eINSTANCE.getMLModel_RegularTestingAndMonitoring();

		/**
		 * The meta object literal for the '<em><b>Transparency</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__TRANSPARENCY = eINSTANCE.getMLModel_Transparency();

		/**
		 * The meta object literal for the '<em><b>Validation</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__VALIDATION = eINSTANCE.getMLModel_Validation();

		/**
		 * The meta object literal for the '<em><b>Watermarking</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ML_MODEL__WATERMARKING = eINSTANCE.getMLModel_Watermarking();

		/**
		 * The meta object literal for the '{@link mltop10.impl.AIAlgorithmImpl <em>AI Algorithm</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see mltop10.impl.AIAlgorithmImpl
		 * @see mltop10.impl.Mltop10PackageImpl#getAIAlgorithm()
		 * @generated
		 */
		EClass AI_ALGORITHM = eINSTANCE.getAIAlgorithm();

		/**
		 * The meta object literal for the '<em><b>Base Artifact</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference AI_ALGORITHM__BASE_ARTIFACT = eINSTANCE.getAIAlgorithm_Base_Artifact();

		/**
		 * The meta object literal for the '<em><b>Public</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute AI_ALGORITHM__PUBLIC = eINSTANCE.getAIAlgorithm_Public();

		/**
		 * The meta object literal for the '<em><b>Access Control</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute AI_ALGORITHM__ACCESS_CONTROL = eINSTANCE.getAIAlgorithm_AccessControl();

		/**
		 * The meta object literal for the '<em><b>Randomize</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute AI_ALGORITHM__RANDOMIZE = eINSTANCE.getAIAlgorithm_Randomize();

		/**
		 * The meta object literal for the '<em><b>Regularisation</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute AI_ALGORITHM__REGULARISATION = eINSTANCE.getAIAlgorithm_Regularisation();

		/**
		 * The meta object literal for the '{@link mltop10.impl.TrainingDataImpl <em>Training Data</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see mltop10.impl.TrainingDataImpl
		 * @see mltop10.impl.Mltop10PackageImpl#getTrainingData()
		 * @generated
		 */
		EClass TRAINING_DATA = eINSTANCE.getTrainingData();

		/**
		 * The meta object literal for the '<em><b>Base Artifact</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TRAINING_DATA__BASE_ARTIFACT = eINSTANCE.getTrainingData_Base_Artifact();

		/**
		 * The meta object literal for the '<em><b>Public</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__PUBLIC = eINSTANCE.getTrainingData_Public();

		/**
		 * The meta object literal for the '<em><b>Access Control</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__ACCESS_CONTROL = eINSTANCE.getTrainingData_AccessControl();

		/**
		 * The meta object literal for the '<em><b>Anomaly Detection</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__ANOMALY_DETECTION = eINSTANCE.getTrainingData_AnomalyDetection();

		/**
		 * The meta object literal for the '<em><b>Reduced</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__REDUCED = eINSTANCE.getTrainingData_Reduced();

		/**
		 * The meta object literal for the '<em><b>Regular Audit And Monitoring</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__REGULAR_AUDIT_AND_MONITORING = eINSTANCE.getTrainingData_RegularAuditAndMonitoring();

		/**
		 * The meta object literal for the '<em><b>Regular Updates And Training</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__REGULAR_UPDATES_AND_TRAINING = eINSTANCE.getTrainingData_RegularUpdatesAndTraining();

		/**
		 * The meta object literal for the '<em><b>Trusted</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__TRUSTED = eINSTANCE.getTrainingData_Trusted();

		/**
		 * The meta object literal for the '<em><b>Validation</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__VALIDATION = eINSTANCE.getTrainingData_Validation();

		/**
		 * The meta object literal for the '<em><b>Verification</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__VERIFICATION = eINSTANCE.getTrainingData_Verification();

		/**
		 * The meta object literal for the '<em><b>Watermarking</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__WATERMARKING = eINSTANCE.getTrainingData_Watermarking();

		/**
		 * The meta object literal for the '<em><b>Regular Backup</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA__REGULAR_BACKUP = eINSTANCE.getTrainingData_RegularBackup();

		/**
		 * The meta object literal for the '{@link mltop10.impl.AIApplicationImpl <em>AI Application</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see mltop10.impl.AIApplicationImpl
		 * @see mltop10.impl.Mltop10PackageImpl#getAIApplication()
		 * @generated
		 */
		EClass AI_APPLICATION = eINSTANCE.getAIApplication();

		/**
		 * The meta object literal for the '<em><b>Base Artifact</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference AI_APPLICATION__BASE_ARTIFACT = eINSTANCE.getAIApplication_Base_Artifact();

		/**
		 * The meta object literal for the '<em><b>Check Model Result Authenticity</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute AI_APPLICATION__CHECK_MODEL_RESULT_AUTHENTICITY = eINSTANCE.getAIApplication_CheckModelResultAuthenticity();

		/**
		 * The meta object literal for the '<em><b>Input Validation</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute AI_APPLICATION__INPUT_VALIDATION = eINSTANCE.getAIApplication_InputValidation();

		/**
		 * The meta object literal for the '<em><b>Tamper Evident Logging</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute AI_APPLICATION__TAMPER_EVIDENT_LOGGING = eINSTANCE.getAIApplication_TamperEvidentLogging();

		/**
		 * The meta object literal for the '<em><b>Regular Audit And Monitoring</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute AI_APPLICATION__REGULAR_AUDIT_AND_MONITORING = eINSTANCE.getAIApplication_RegularAuditAndMonitoring();

		/**
		 * The meta object literal for the '{@link mltop10.impl.TrainingDataServerImpl <em>Training Data Server</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see mltop10.impl.TrainingDataServerImpl
		 * @see mltop10.impl.Mltop10PackageImpl#getTrainingDataServer()
		 * @generated
		 */
		EClass TRAINING_DATA_SERVER = eINSTANCE.getTrainingDataServer();

		/**
		 * The meta object literal for the '<em><b>Base Node</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TRAINING_DATA_SERVER__BASE_NODE = eINSTANCE.getTrainingDataServer_Base_Node();

		/**
		 * The meta object literal for the '<em><b>Secure Data Storage</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRAINING_DATA_SERVER__SECURE_DATA_STORAGE = eINSTANCE.getTrainingDataServer_SecureDataStorage();

		/**
		 * The meta object literal for the '{@link mltop10.impl.SecureAIScenarioImpl <em>Secure AI Scenario</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see mltop10.impl.SecureAIScenarioImpl
		 * @see mltop10.impl.Mltop10PackageImpl#getSecureAIScenario()
		 * @generated
		 */
		EClass SECURE_AI_SCENARIO = eINSTANCE.getSecureAIScenario();

		/**
		 * The meta object literal for the '<em><b>Package Integrity Verified</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURE_AI_SCENARIO__PACKAGE_INTEGRITY_VERIFIED = eINSTANCE.getSecureAIScenario_PackageIntegrityVerified();

		/**
		 * The meta object literal for the '<em><b>Packages From Secure Sources</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURE_AI_SCENARIO__PACKAGES_FROM_SECURE_SOURCES = eINSTANCE.getSecureAIScenario_PackagesFromSecureSources();

		/**
		 * The meta object literal for the '<em><b>Regular Security Audits</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURE_AI_SCENARIO__REGULAR_SECURITY_AUDITS = eINSTANCE.getSecureAIScenario_RegularSecurityAudits();

		/**
		 * The meta object literal for the '<em><b>Regular Package Updates</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURE_AI_SCENARIO__REGULAR_PACKAGE_UPDATES = eINSTANCE.getSecureAIScenario_RegularPackageUpdates();

		/**
		 * The meta object literal for the '<em><b>Secure Deployment</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURE_AI_SCENARIO__SECURE_DEPLOYMENT = eINSTANCE.getSecureAIScenario_SecureDeployment();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SECURE_AI_SCENARIO__BASE_PACKAGE = eINSTANCE.getSecureAIScenario_Base_Package();

		/**
		 * The meta object literal for the '<em><b>Base Model</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SECURE_AI_SCENARIO__BASE_MODEL = eINSTANCE.getSecureAIScenario_Base_Model();

		/**
		 * The meta object literal for the '{@link mltop10.impl.FeedbackDataImpl <em>Feedback Data</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see mltop10.impl.FeedbackDataImpl
		 * @see mltop10.impl.Mltop10PackageImpl#getFeedbackData()
		 * @generated
		 */
		EClass FEEDBACK_DATA = eINSTANCE.getFeedbackData();

		/**
		 * The meta object literal for the '<em><b>Base Artifact</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference FEEDBACK_DATA__BASE_ARTIFACT = eINSTANCE.getFeedbackData_Base_Artifact();

		/**
		 * The meta object literal for the '<em><b>Access Control</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FEEDBACK_DATA__ACCESS_CONTROL = eINSTANCE.getFeedbackData_AccessControl();

		/**
		 * The meta object literal for the '<em><b>Anomaly Detection</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FEEDBACK_DATA__ANOMALY_DETECTION = eINSTANCE.getFeedbackData_AnomalyDetection();

		/**
		 * The meta object literal for the '<em><b>Authenticity Verified</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FEEDBACK_DATA__AUTHENTICITY_VERIFIED = eINSTANCE.getFeedbackData_AuthenticityVerified();

		/**
		 * The meta object literal for the '<em><b>Cleaning</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FEEDBACK_DATA__CLEANING = eINSTANCE.getFeedbackData_Cleaning();

		/**
		 * The meta object literal for the '<em><b>Validation</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FEEDBACK_DATA__VALIDATION = eINSTANCE.getFeedbackData_Validation();

		/**
		 * The meta object literal for the '{@link mltop10.impl.SecureCommPathImpl <em>Secure Comm Path</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see mltop10.impl.SecureCommPathImpl
		 * @see mltop10.impl.Mltop10PackageImpl#getSecureCommPath()
		 * @generated
		 */
		EClass SECURE_COMM_PATH = eINSTANCE.getSecureCommPath();

		/**
		 * The meta object literal for the '<em><b>Base Communication Path</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SECURE_COMM_PATH__BASE_COMMUNICATION_PATH = eINSTANCE.getSecureCommPath_Base_CommunicationPath();

		/**
		 * The meta object literal for the '<em><b>Confidelity Preserving</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURE_COMM_PATH__CONFIDELITY_PRESERVING = eINSTANCE.getSecureCommPath_ConfidelityPreserving();

		/**
		 * The meta object literal for the '<em><b>Integrity Preserving</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURE_COMM_PATH__INTEGRITY_PRESERVING = eINSTANCE.getSecureCommPath_IntegrityPreserving();

	}

} //Mltop10Package
