/**
 */
package mltop10.impl;

import mltop10.AIAlgorithm;
import mltop10.AIApplication;
import mltop10.FeedbackData;
import mltop10.MLModel;
import mltop10.Mltop10Factory;
import mltop10.Mltop10Package;
import mltop10.SecureAIService;
import mltop10.SecureCommPath;
import mltop10.ThreatComments;
import mltop10.TrainingData;
import mltop10.TrainingDataServer;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcorePackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.uml2.types.TypesPackage;

import org.eclipse.uml2.uml.UMLPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class Mltop10PackageImpl extends EPackageImpl implements Mltop10Package {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass mlModelEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass aiAlgorithmEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass trainingDataEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass aiApplicationEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass trainingDataServerEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass secureAIServiceEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass feedbackDataEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass secureCommPathEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass threatCommentsEClass = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see mltop10.Mltop10Package#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private Mltop10PackageImpl() {
		super(eNS_URI, Mltop10Factory.eINSTANCE);
	}
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 *
	 * <p>This method is used to initialize {@link Mltop10Package#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static Mltop10Package init() {
		if (isInited) return (Mltop10Package)EPackage.Registry.INSTANCE.getEPackage(Mltop10Package.eNS_URI);

		// Obtain or create and register package
		Object registeredMltop10Package = EPackage.Registry.INSTANCE.get(eNS_URI);
		Mltop10PackageImpl theMltop10Package = registeredMltop10Package instanceof Mltop10PackageImpl ? (Mltop10PackageImpl)registeredMltop10Package : new Mltop10PackageImpl();

		isInited = true;

		// Initialize simple dependencies
		EcorePackage.eINSTANCE.eClass();
		TypesPackage.eINSTANCE.eClass();
		UMLPackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theMltop10Package.createPackageContents();

		// Initialize created meta-data
		theMltop10Package.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theMltop10Package.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(Mltop10Package.eNS_URI, theMltop10Package);
		return theMltop10Package;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getMLModel() {
		return mlModelEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getMLModel_Base_Artifact() {
		return (EReference)mlModelEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_Public() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_AccessControl() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_AdversarialTraining() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_AnomalyDetection() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_CryptographicallySecured() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_DefenseMechanism() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_DifferentialPrivacy() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_EnsembleModel() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(8);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_Obfuscation() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(9);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_LegalProtection() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(10);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_RegularBackup() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(11);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_RegularRetraining() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(12);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_RobustModelDesign() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(13);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_RobustArchitecture() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(14);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_RegularAuditAndMonitoring() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(15);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_RegularPerformanceMonitoring() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(16);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_RegularTestingAndMonitoring() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(17);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_Transparency() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(18);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_Validation() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(19);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getMLModel_Watermarking() {
		return (EAttribute)mlModelEClass.getEStructuralFeatures().get(20);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getAIAlgorithm() {
		return aiAlgorithmEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getAIAlgorithm_Base_Artifact() {
		return (EReference)aiAlgorithmEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getAIAlgorithm_Public() {
		return (EAttribute)aiAlgorithmEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getAIAlgorithm_AccessControl() {
		return (EAttribute)aiAlgorithmEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getAIAlgorithm_Randomize() {
		return (EAttribute)aiAlgorithmEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getAIAlgorithm_Regularisation() {
		return (EAttribute)aiAlgorithmEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getTrainingData() {
		return trainingDataEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getTrainingData_Base_Artifact() {
		return (EReference)trainingDataEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_Public() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_AccessControl() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_AnomalyDetection() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_Reduced() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_RegularAuditAndMonitoring() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_RegularUpdatesAndTraining() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_Trusted() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_Validation() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(8);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_Verification() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(9);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_Watermarking() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(10);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingData_RegularBackup() {
		return (EAttribute)trainingDataEClass.getEStructuralFeatures().get(11);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getAIApplication() {
		return aiApplicationEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getAIApplication_Base_Artifact() {
		return (EReference)aiApplicationEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getAIApplication_CheckModelResultAuthenticity() {
		return (EAttribute)aiApplicationEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getAIApplication_InputValidation() {
		return (EAttribute)aiApplicationEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getAIApplication_TamperEvidentLogging() {
		return (EAttribute)aiApplicationEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getAIApplication_RegularAuditAndMonitoring() {
		return (EAttribute)aiApplicationEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getTrainingDataServer() {
		return trainingDataServerEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getTrainingDataServer_Base_Node() {
		return (EReference)trainingDataServerEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getTrainingDataServer_SecureDataStorage() {
		return (EAttribute)trainingDataServerEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSecureAIService() {
		return secureAIServiceEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getSecureAIService_PackageIntegrityVerified() {
		return (EAttribute)secureAIServiceEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getSecureAIService_PackagesFromSecureSources() {
		return (EAttribute)secureAIServiceEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getSecureAIService_RegularSecurityAudits() {
		return (EAttribute)secureAIServiceEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getSecureAIService_RegularPackageUpdates() {
		return (EAttribute)secureAIServiceEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getSecureAIService_SecureDeployment() {
		return (EAttribute)secureAIServiceEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSecureAIService_Base_Package() {
		return (EReference)secureAIServiceEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSecureAIService_Base_Model() {
		return (EReference)secureAIServiceEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getFeedbackData() {
		return feedbackDataEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getFeedbackData_Base_Artifact() {
		return (EReference)feedbackDataEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getFeedbackData_AccessControl() {
		return (EAttribute)feedbackDataEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getFeedbackData_AnomalyDetection() {
		return (EAttribute)feedbackDataEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getFeedbackData_AuthenticityVerified() {
		return (EAttribute)feedbackDataEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getFeedbackData_Cleaning() {
		return (EAttribute)feedbackDataEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getFeedbackData_Validation() {
		return (EAttribute)feedbackDataEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getSecureCommPath() {
		return secureCommPathEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getSecureCommPath_Base_CommunicationPath() {
		return (EReference)secureCommPathEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getSecureCommPath_ConfidentialityPreserving() {
		return (EAttribute)secureCommPathEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getSecureCommPath_IntegrityPreserving() {
		return (EAttribute)secureCommPathEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getThreatComments() {
		return threatCommentsEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getThreatComments_InputManipulation() {
		return (EAttribute)threatCommentsEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getThreatComments_Base_Package() {
		return (EReference)threatCommentsEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getThreatComments_Base_Model() {
		return (EReference)threatCommentsEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getThreatComments_AISupplyChain() {
		return (EAttribute)threatCommentsEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getThreatComments_DataPoisoning() {
		return (EAttribute)threatCommentsEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getThreatComments_MembershipInference() {
		return (EAttribute)threatCommentsEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getThreatComments_ModelInversion() {
		return (EAttribute)threatCommentsEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getThreatComments_ModelPoisoning() {
		return (EAttribute)threatCommentsEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getThreatComments_ModelSkewing() {
		return (EAttribute)threatCommentsEClass.getEStructuralFeatures().get(8);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getThreatComments_ModelTheft() {
		return (EAttribute)threatCommentsEClass.getEStructuralFeatures().get(9);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getThreatComments_OutputIntegrity() {
		return (EAttribute)threatCommentsEClass.getEStructuralFeatures().get(10);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getThreatComments_TransferLearning() {
		return (EAttribute)threatCommentsEClass.getEStructuralFeatures().get(11);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Mltop10Factory getMltop10Factory() {
		return (Mltop10Factory)getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated) return;
		isCreated = true;

		// Create classes and their features
		mlModelEClass = createEClass(ML_MODEL);
		createEReference(mlModelEClass, ML_MODEL__BASE_ARTIFACT);
		createEAttribute(mlModelEClass, ML_MODEL__PUBLIC);
		createEAttribute(mlModelEClass, ML_MODEL__ACCESS_CONTROL);
		createEAttribute(mlModelEClass, ML_MODEL__ADVERSARIAL_TRAINING);
		createEAttribute(mlModelEClass, ML_MODEL__ANOMALY_DETECTION);
		createEAttribute(mlModelEClass, ML_MODEL__CRYPTOGRAPHICALLY_SECURED);
		createEAttribute(mlModelEClass, ML_MODEL__DEFENSE_MECHANISM);
		createEAttribute(mlModelEClass, ML_MODEL__DIFFERENTIAL_PRIVACY);
		createEAttribute(mlModelEClass, ML_MODEL__ENSEMBLE_MODEL);
		createEAttribute(mlModelEClass, ML_MODEL__OBFUSCATION);
		createEAttribute(mlModelEClass, ML_MODEL__LEGAL_PROTECTION);
		createEAttribute(mlModelEClass, ML_MODEL__REGULAR_BACKUP);
		createEAttribute(mlModelEClass, ML_MODEL__REGULAR_RETRAINING);
		createEAttribute(mlModelEClass, ML_MODEL__ROBUST_MODEL_DESIGN);
		createEAttribute(mlModelEClass, ML_MODEL__ROBUST_ARCHITECTURE);
		createEAttribute(mlModelEClass, ML_MODEL__REGULAR_AUDIT_AND_MONITORING);
		createEAttribute(mlModelEClass, ML_MODEL__REGULAR_PERFORMANCE_MONITORING);
		createEAttribute(mlModelEClass, ML_MODEL__REGULAR_TESTING_AND_MONITORING);
		createEAttribute(mlModelEClass, ML_MODEL__TRANSPARENCY);
		createEAttribute(mlModelEClass, ML_MODEL__VALIDATION);
		createEAttribute(mlModelEClass, ML_MODEL__WATERMARKING);

		aiAlgorithmEClass = createEClass(AI_ALGORITHM);
		createEReference(aiAlgorithmEClass, AI_ALGORITHM__BASE_ARTIFACT);
		createEAttribute(aiAlgorithmEClass, AI_ALGORITHM__PUBLIC);
		createEAttribute(aiAlgorithmEClass, AI_ALGORITHM__ACCESS_CONTROL);
		createEAttribute(aiAlgorithmEClass, AI_ALGORITHM__RANDOMIZE);
		createEAttribute(aiAlgorithmEClass, AI_ALGORITHM__REGULARISATION);

		trainingDataEClass = createEClass(TRAINING_DATA);
		createEReference(trainingDataEClass, TRAINING_DATA__BASE_ARTIFACT);
		createEAttribute(trainingDataEClass, TRAINING_DATA__PUBLIC);
		createEAttribute(trainingDataEClass, TRAINING_DATA__ACCESS_CONTROL);
		createEAttribute(trainingDataEClass, TRAINING_DATA__ANOMALY_DETECTION);
		createEAttribute(trainingDataEClass, TRAINING_DATA__REDUCED);
		createEAttribute(trainingDataEClass, TRAINING_DATA__REGULAR_AUDIT_AND_MONITORING);
		createEAttribute(trainingDataEClass, TRAINING_DATA__REGULAR_UPDATES_AND_TRAINING);
		createEAttribute(trainingDataEClass, TRAINING_DATA__TRUSTED);
		createEAttribute(trainingDataEClass, TRAINING_DATA__VALIDATION);
		createEAttribute(trainingDataEClass, TRAINING_DATA__VERIFICATION);
		createEAttribute(trainingDataEClass, TRAINING_DATA__WATERMARKING);
		createEAttribute(trainingDataEClass, TRAINING_DATA__REGULAR_BACKUP);

		aiApplicationEClass = createEClass(AI_APPLICATION);
		createEReference(aiApplicationEClass, AI_APPLICATION__BASE_ARTIFACT);
		createEAttribute(aiApplicationEClass, AI_APPLICATION__CHECK_MODEL_RESULT_AUTHENTICITY);
		createEAttribute(aiApplicationEClass, AI_APPLICATION__INPUT_VALIDATION);
		createEAttribute(aiApplicationEClass, AI_APPLICATION__TAMPER_EVIDENT_LOGGING);
		createEAttribute(aiApplicationEClass, AI_APPLICATION__REGULAR_AUDIT_AND_MONITORING);

		trainingDataServerEClass = createEClass(TRAINING_DATA_SERVER);
		createEReference(trainingDataServerEClass, TRAINING_DATA_SERVER__BASE_NODE);
		createEAttribute(trainingDataServerEClass, TRAINING_DATA_SERVER__SECURE_DATA_STORAGE);

		secureAIServiceEClass = createEClass(SECURE_AI_SERVICE);
		createEAttribute(secureAIServiceEClass, SECURE_AI_SERVICE__PACKAGE_INTEGRITY_VERIFIED);
		createEAttribute(secureAIServiceEClass, SECURE_AI_SERVICE__PACKAGES_FROM_SECURE_SOURCES);
		createEAttribute(secureAIServiceEClass, SECURE_AI_SERVICE__REGULAR_SECURITY_AUDITS);
		createEAttribute(secureAIServiceEClass, SECURE_AI_SERVICE__REGULAR_PACKAGE_UPDATES);
		createEAttribute(secureAIServiceEClass, SECURE_AI_SERVICE__SECURE_DEPLOYMENT);
		createEReference(secureAIServiceEClass, SECURE_AI_SERVICE__BASE_PACKAGE);
		createEReference(secureAIServiceEClass, SECURE_AI_SERVICE__BASE_MODEL);

		feedbackDataEClass = createEClass(FEEDBACK_DATA);
		createEReference(feedbackDataEClass, FEEDBACK_DATA__BASE_ARTIFACT);
		createEAttribute(feedbackDataEClass, FEEDBACK_DATA__ACCESS_CONTROL);
		createEAttribute(feedbackDataEClass, FEEDBACK_DATA__ANOMALY_DETECTION);
		createEAttribute(feedbackDataEClass, FEEDBACK_DATA__AUTHENTICITY_VERIFIED);
		createEAttribute(feedbackDataEClass, FEEDBACK_DATA__CLEANING);
		createEAttribute(feedbackDataEClass, FEEDBACK_DATA__VALIDATION);

		secureCommPathEClass = createEClass(SECURE_COMM_PATH);
		createEReference(secureCommPathEClass, SECURE_COMM_PATH__BASE_COMMUNICATION_PATH);
		createEAttribute(secureCommPathEClass, SECURE_COMM_PATH__CONFIDENTIALITY_PRESERVING);
		createEAttribute(secureCommPathEClass, SECURE_COMM_PATH__INTEGRITY_PRESERVING);

		threatCommentsEClass = createEClass(THREAT_COMMENTS);
		createEAttribute(threatCommentsEClass, THREAT_COMMENTS__INPUT_MANIPULATION);
		createEReference(threatCommentsEClass, THREAT_COMMENTS__BASE_PACKAGE);
		createEReference(threatCommentsEClass, THREAT_COMMENTS__BASE_MODEL);
		createEAttribute(threatCommentsEClass, THREAT_COMMENTS__AI_SUPPLY_CHAIN);
		createEAttribute(threatCommentsEClass, THREAT_COMMENTS__DATA_POISONING);
		createEAttribute(threatCommentsEClass, THREAT_COMMENTS__MEMBERSHIP_INFERENCE);
		createEAttribute(threatCommentsEClass, THREAT_COMMENTS__MODEL_INVERSION);
		createEAttribute(threatCommentsEClass, THREAT_COMMENTS__MODEL_POISONING);
		createEAttribute(threatCommentsEClass, THREAT_COMMENTS__MODEL_SKEWING);
		createEAttribute(threatCommentsEClass, THREAT_COMMENTS__MODEL_THEFT);
		createEAttribute(threatCommentsEClass, THREAT_COMMENTS__OUTPUT_INTEGRITY);
		createEAttribute(threatCommentsEClass, THREAT_COMMENTS__TRANSFER_LEARNING);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized) return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Obtain other dependent packages
		UMLPackage theUMLPackage = (UMLPackage)EPackage.Registry.INSTANCE.getEPackage(UMLPackage.eNS_URI);
		TypesPackage theTypesPackage = (TypesPackage)EPackage.Registry.INSTANCE.getEPackage(TypesPackage.eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes

		// Initialize classes, features, and operations; add parameters
		initEClass(mlModelEClass, MLModel.class, "MLModel", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getMLModel_Base_Artifact(), theUMLPackage.getArtifact(), null, "base_Artifact", null, 0, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_Public(), theTypesPackage.getBoolean(), "Public", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_AccessControl(), theTypesPackage.getBoolean(), "AccessControl", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_AdversarialTraining(), theTypesPackage.getBoolean(), "AdversarialTraining", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_AnomalyDetection(), theTypesPackage.getBoolean(), "AnomalyDetection", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_CryptographicallySecured(), theTypesPackage.getBoolean(), "CryptographicallySecured", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_DefenseMechanism(), theTypesPackage.getBoolean(), "DefenseMechanism", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_DifferentialPrivacy(), theTypesPackage.getBoolean(), "DifferentialPrivacy", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_EnsembleModel(), theTypesPackage.getBoolean(), "EnsembleModel", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_Obfuscation(), theTypesPackage.getBoolean(), "Obfuscation", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_LegalProtection(), theTypesPackage.getBoolean(), "LegalProtection", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_RegularBackup(), theTypesPackage.getBoolean(), "RegularBackup", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_RegularRetraining(), theTypesPackage.getBoolean(), "RegularRetraining", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_RobustModelDesign(), theTypesPackage.getBoolean(), "RobustModelDesign", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_RobustArchitecture(), theTypesPackage.getBoolean(), "RobustArchitecture", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_RegularAuditAndMonitoring(), theTypesPackage.getBoolean(), "RegularAuditAndMonitoring", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_RegularPerformanceMonitoring(), theTypesPackage.getBoolean(), "RegularPerformanceMonitoring", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_RegularTestingAndMonitoring(), theTypesPackage.getBoolean(), "RegularTestingAndMonitoring", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_Transparency(), theTypesPackage.getBoolean(), "Transparency", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_Validation(), theTypesPackage.getBoolean(), "Validation", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getMLModel_Watermarking(), theTypesPackage.getBoolean(), "Watermarking", null, 1, 1, MLModel.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(aiAlgorithmEClass, AIAlgorithm.class, "AIAlgorithm", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getAIAlgorithm_Base_Artifact(), theUMLPackage.getArtifact(), null, "base_Artifact", null, 0, 1, AIAlgorithm.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getAIAlgorithm_Public(), theTypesPackage.getBoolean(), "Public", null, 1, 1, AIAlgorithm.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getAIAlgorithm_AccessControl(), theTypesPackage.getBoolean(), "AccessControl", null, 1, 1, AIAlgorithm.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getAIAlgorithm_Randomize(), theTypesPackage.getBoolean(), "Randomize", null, 1, 1, AIAlgorithm.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getAIAlgorithm_Regularisation(), theTypesPackage.getBoolean(), "Regularisation", null, 1, 1, AIAlgorithm.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(trainingDataEClass, TrainingData.class, "TrainingData", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getTrainingData_Base_Artifact(), theUMLPackage.getArtifact(), null, "base_Artifact", null, 0, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_Public(), theTypesPackage.getBoolean(), "Public", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_AccessControl(), theTypesPackage.getBoolean(), "AccessControl", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_AnomalyDetection(), theTypesPackage.getBoolean(), "AnomalyDetection", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_Reduced(), theTypesPackage.getBoolean(), "Reduced", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_RegularAuditAndMonitoring(), theTypesPackage.getBoolean(), "RegularAuditAndMonitoring", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_RegularUpdatesAndTraining(), theTypesPackage.getBoolean(), "RegularUpdatesAndTraining", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_Trusted(), theTypesPackage.getBoolean(), "Trusted", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_Validation(), theTypesPackage.getBoolean(), "Validation", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_Verification(), theTypesPackage.getBoolean(), "Verification", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_Watermarking(), theTypesPackage.getBoolean(), "Watermarking", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingData_RegularBackup(), theTypesPackage.getBoolean(), "RegularBackup", null, 1, 1, TrainingData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(aiApplicationEClass, AIApplication.class, "AIApplication", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getAIApplication_Base_Artifact(), theUMLPackage.getArtifact(), null, "base_Artifact", null, 0, 1, AIApplication.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getAIApplication_CheckModelResultAuthenticity(), theTypesPackage.getBoolean(), "CheckModelResultAuthenticity", null, 1, 1, AIApplication.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getAIApplication_InputValidation(), theTypesPackage.getBoolean(), "InputValidation", null, 1, 1, AIApplication.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getAIApplication_TamperEvidentLogging(), theTypesPackage.getBoolean(), "TamperEvidentLogging", null, 1, 1, AIApplication.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getAIApplication_RegularAuditAndMonitoring(), theTypesPackage.getBoolean(), "RegularAuditAndMonitoring", null, 1, 1, AIApplication.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(trainingDataServerEClass, TrainingDataServer.class, "TrainingDataServer", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getTrainingDataServer_Base_Node(), theUMLPackage.getNode(), null, "base_Node", null, 0, 1, TrainingDataServer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getTrainingDataServer_SecureDataStorage(), theTypesPackage.getBoolean(), "SecureDataStorage", null, 1, 1, TrainingDataServer.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(secureAIServiceEClass, SecureAIService.class, "SecureAIService", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getSecureAIService_PackageIntegrityVerified(), theTypesPackage.getBoolean(), "PackageIntegrityVerified", null, 1, 1, SecureAIService.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getSecureAIService_PackagesFromSecureSources(), theTypesPackage.getBoolean(), "PackagesFromSecureSources", null, 1, 1, SecureAIService.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getSecureAIService_RegularSecurityAudits(), theTypesPackage.getBoolean(), "RegularSecurityAudits", null, 1, 1, SecureAIService.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getSecureAIService_RegularPackageUpdates(), theTypesPackage.getBoolean(), "RegularPackageUpdates", null, 1, 1, SecureAIService.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getSecureAIService_SecureDeployment(), theTypesPackage.getBoolean(), "SecureDeployment", null, 1, 1, SecureAIService.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getSecureAIService_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 0, 1, SecureAIService.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getSecureAIService_Base_Model(), theUMLPackage.getModel(), null, "base_Model", null, 0, 1, SecureAIService.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(feedbackDataEClass, FeedbackData.class, "FeedbackData", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getFeedbackData_Base_Artifact(), theUMLPackage.getArtifact(), null, "base_Artifact", null, 0, 1, FeedbackData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getFeedbackData_AccessControl(), theTypesPackage.getBoolean(), "AccessControl", null, 1, 1, FeedbackData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getFeedbackData_AnomalyDetection(), theTypesPackage.getBoolean(), "AnomalyDetection", null, 1, 1, FeedbackData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getFeedbackData_AuthenticityVerified(), theTypesPackage.getBoolean(), "AuthenticityVerified", null, 1, 1, FeedbackData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getFeedbackData_Cleaning(), theTypesPackage.getBoolean(), "Cleaning", null, 1, 1, FeedbackData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getFeedbackData_Validation(), theTypesPackage.getBoolean(), "Validation", null, 1, 1, FeedbackData.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(secureCommPathEClass, SecureCommPath.class, "SecureCommPath", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getSecureCommPath_Base_CommunicationPath(), theUMLPackage.getCommunicationPath(), null, "base_CommunicationPath", null, 0, 1, SecureCommPath.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getSecureCommPath_ConfidentialityPreserving(), theTypesPackage.getBoolean(), "ConfidentialityPreserving", null, 1, 1, SecureCommPath.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getSecureCommPath_IntegrityPreserving(), theTypesPackage.getBoolean(), "IntegrityPreserving", null, 1, 1, SecureCommPath.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(threatCommentsEClass, ThreatComments.class, "ThreatComments", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getThreatComments_InputManipulation(), theTypesPackage.getString(), "InputManipulation", null, 0, -1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getThreatComments_Base_Package(), theUMLPackage.getPackage(), null, "base_Package", null, 0, 1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getThreatComments_Base_Model(), theUMLPackage.getModel(), null, "base_Model", null, 0, 1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getThreatComments_AISupplyChain(), theTypesPackage.getString(), "AISupplyChain", null, 0, -1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getThreatComments_DataPoisoning(), theTypesPackage.getString(), "DataPoisoning", null, 0, -1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getThreatComments_MembershipInference(), theTypesPackage.getString(), "MembershipInference", null, 0, -1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getThreatComments_ModelInversion(), theTypesPackage.getString(), "ModelInversion", null, 0, -1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getThreatComments_ModelPoisoning(), theTypesPackage.getString(), "ModelPoisoning", null, 0, -1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getThreatComments_ModelSkewing(), theTypesPackage.getString(), "ModelSkewing", null, 0, -1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getThreatComments_ModelTheft(), theTypesPackage.getString(), "ModelTheft", null, 0, -1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getThreatComments_OutputIntegrity(), theTypesPackage.getString(), "OutputIntegrity", null, 0, -1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getThreatComments_TransferLearning(), theTypesPackage.getString(), "TransferLearning", null, 0, -1, ThreatComments.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		// Create resource
		createResource(eNS_URI);
	}

} //Mltop10PackageImpl
