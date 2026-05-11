/**
 */
package mltop10.impl;

import mltop10.MLModel;
import mltop10.Mltop10Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>ML Model</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link mltop10.impl.MLModelImpl#getBase_Artifact <em>Base Artifact</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isPublic <em>Public</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isAccessControl <em>Access Control</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isAdversarialTraining <em>Adversarial Training</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isAnomalyDetection <em>Anomaly Detection</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isCryptographicallySecured <em>Cryptographically Secured</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isDefenseMechanism <em>Defense Mechanism</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isDifferentialPrivacy <em>Differential Privacy</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isEnsembleModel <em>Ensemble Model</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isObfuscation <em>Obfuscation</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isLegalProtection <em>Legal Protection</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isRegularBackup <em>Regular Backup</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isRegularRetraining <em>Regular Retraining</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isRobustActivationFunction <em>Robust Activation Function</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isRobustArchitecture <em>Robust Architecture</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isRegularPerformanceMonitoring <em>Regular Performance Monitoring</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isRegularTestingAndMonitoring <em>Regular Testing And Monitoring</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isTransparency <em>Transparency</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isValidation <em>Validation</em>}</li>
 *   <li>{@link mltop10.impl.MLModelImpl#isWatermarking <em>Watermarking</em>}</li>
 * </ul>
 *
 * @generated
 */
public class MLModelImpl extends MinimalEObjectImpl.Container implements MLModel {
	/**
	 * The cached value of the '{@link #getBase_Artifact() <em>Base Artifact</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Artifact()
	 * @generated
	 * @ordered
	 */
	protected Artifact base_Artifact;

	/**
	 * The default value of the '{@link #isPublic() <em>Public</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isPublic()
	 * @generated
	 * @ordered
	 */
	protected static final boolean PUBLIC_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isPublic() <em>Public</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isPublic()
	 * @generated
	 * @ordered
	 */
	protected boolean public_ = PUBLIC_EDEFAULT;

	/**
	 * The default value of the '{@link #isAccessControl() <em>Access Control</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAccessControl()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ACCESS_CONTROL_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAccessControl() <em>Access Control</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAccessControl()
	 * @generated
	 * @ordered
	 */
	protected boolean accessControl = ACCESS_CONTROL_EDEFAULT;

	/**
	 * The default value of the '{@link #isAdversarialTraining() <em>Adversarial Training</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAdversarialTraining()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ADVERSARIAL_TRAINING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAdversarialTraining() <em>Adversarial Training</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAdversarialTraining()
	 * @generated
	 * @ordered
	 */
	protected boolean adversarialTraining = ADVERSARIAL_TRAINING_EDEFAULT;

	/**
	 * The default value of the '{@link #isAnomalyDetection() <em>Anomaly Detection</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAnomalyDetection()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ANOMALY_DETECTION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAnomalyDetection() <em>Anomaly Detection</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAnomalyDetection()
	 * @generated
	 * @ordered
	 */
	protected boolean anomalyDetection = ANOMALY_DETECTION_EDEFAULT;

	/**
	 * The default value of the '{@link #isCryptographicallySecured() <em>Cryptographically Secured</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isCryptographicallySecured()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CRYPTOGRAPHICALLY_SECURED_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isCryptographicallySecured() <em>Cryptographically Secured</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isCryptographicallySecured()
	 * @generated
	 * @ordered
	 */
	protected boolean cryptographicallySecured = CRYPTOGRAPHICALLY_SECURED_EDEFAULT;

	/**
	 * The default value of the '{@link #isDefenseMechanism() <em>Defense Mechanism</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isDefenseMechanism()
	 * @generated
	 * @ordered
	 */
	protected static final boolean DEFENSE_MECHANISM_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isDefenseMechanism() <em>Defense Mechanism</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isDefenseMechanism()
	 * @generated
	 * @ordered
	 */
	protected boolean defenseMechanism = DEFENSE_MECHANISM_EDEFAULT;

	/**
	 * The default value of the '{@link #isDifferentialPrivacy() <em>Differential Privacy</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isDifferentialPrivacy()
	 * @generated
	 * @ordered
	 */
	protected static final boolean DIFFERENTIAL_PRIVACY_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isDifferentialPrivacy() <em>Differential Privacy</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isDifferentialPrivacy()
	 * @generated
	 * @ordered
	 */
	protected boolean differentialPrivacy = DIFFERENTIAL_PRIVACY_EDEFAULT;

	/**
	 * The default value of the '{@link #isEnsembleModel() <em>Ensemble Model</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isEnsembleModel()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ENSEMBLE_MODEL_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isEnsembleModel() <em>Ensemble Model</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isEnsembleModel()
	 * @generated
	 * @ordered
	 */
	protected boolean ensembleModel = ENSEMBLE_MODEL_EDEFAULT;

	/**
	 * The default value of the '{@link #isObfuscation() <em>Obfuscation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isObfuscation()
	 * @generated
	 * @ordered
	 */
	protected static final boolean OBFUSCATION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isObfuscation() <em>Obfuscation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isObfuscation()
	 * @generated
	 * @ordered
	 */
	protected boolean obfuscation = OBFUSCATION_EDEFAULT;

	/**
	 * The default value of the '{@link #isLegalProtection() <em>Legal Protection</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isLegalProtection()
	 * @generated
	 * @ordered
	 */
	protected static final boolean LEGAL_PROTECTION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isLegalProtection() <em>Legal Protection</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isLegalProtection()
	 * @generated
	 * @ordered
	 */
	protected boolean legalProtection = LEGAL_PROTECTION_EDEFAULT;

	/**
	 * The default value of the '{@link #isRegularBackup() <em>Regular Backup</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularBackup()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REGULAR_BACKUP_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRegularBackup() <em>Regular Backup</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularBackup()
	 * @generated
	 * @ordered
	 */
	protected boolean regularBackup = REGULAR_BACKUP_EDEFAULT;

	/**
	 * The default value of the '{@link #isRegularRetraining() <em>Regular Retraining</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularRetraining()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REGULAR_RETRAINING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRegularRetraining() <em>Regular Retraining</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularRetraining()
	 * @generated
	 * @ordered
	 */
	protected boolean regularRetraining = REGULAR_RETRAINING_EDEFAULT;

	/**
	 * The default value of the '{@link #isRobustActivationFunction() <em>Robust Activation Function</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRobustActivationFunction()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ROBUST_ACTIVATION_FUNCTION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRobustActivationFunction() <em>Robust Activation Function</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRobustActivationFunction()
	 * @generated
	 * @ordered
	 */
	protected boolean robustActivationFunction = ROBUST_ACTIVATION_FUNCTION_EDEFAULT;

	/**
	 * The default value of the '{@link #isRobustArchitecture() <em>Robust Architecture</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRobustArchitecture()
	 * @generated
	 * @ordered
	 */
	protected static final boolean ROBUST_ARCHITECTURE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRobustArchitecture() <em>Robust Architecture</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRobustArchitecture()
	 * @generated
	 * @ordered
	 */
	protected boolean robustArchitecture = ROBUST_ARCHITECTURE_EDEFAULT;

	/**
	 * The default value of the '{@link #isRegularAuditAndMonitoring() <em>Regular Audit And Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularAuditAndMonitoring()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REGULAR_AUDIT_AND_MONITORING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRegularAuditAndMonitoring() <em>Regular Audit And Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularAuditAndMonitoring()
	 * @generated
	 * @ordered
	 */
	protected boolean regularAuditAndMonitoring = REGULAR_AUDIT_AND_MONITORING_EDEFAULT;

	/**
	 * The default value of the '{@link #isRegularPerformanceMonitoring() <em>Regular Performance Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularPerformanceMonitoring()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REGULAR_PERFORMANCE_MONITORING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRegularPerformanceMonitoring() <em>Regular Performance Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularPerformanceMonitoring()
	 * @generated
	 * @ordered
	 */
	protected boolean regularPerformanceMonitoring = REGULAR_PERFORMANCE_MONITORING_EDEFAULT;

	/**
	 * The default value of the '{@link #isRegularTestingAndMonitoring() <em>Regular Testing And Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularTestingAndMonitoring()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REGULAR_TESTING_AND_MONITORING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRegularTestingAndMonitoring() <em>Regular Testing And Monitoring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularTestingAndMonitoring()
	 * @generated
	 * @ordered
	 */
	protected boolean regularTestingAndMonitoring = REGULAR_TESTING_AND_MONITORING_EDEFAULT;

	/**
	 * The default value of the '{@link #isTransparency() <em>Transparency</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isTransparency()
	 * @generated
	 * @ordered
	 */
	protected static final boolean TRANSPARENCY_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isTransparency() <em>Transparency</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isTransparency()
	 * @generated
	 * @ordered
	 */
	protected boolean transparency = TRANSPARENCY_EDEFAULT;

	/**
	 * The default value of the '{@link #isValidation() <em>Validation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isValidation()
	 * @generated
	 * @ordered
	 */
	protected static final boolean VALIDATION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isValidation() <em>Validation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isValidation()
	 * @generated
	 * @ordered
	 */
	protected boolean validation = VALIDATION_EDEFAULT;

	/**
	 * The default value of the '{@link #isWatermarking() <em>Watermarking</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isWatermarking()
	 * @generated
	 * @ordered
	 */
	protected static final boolean WATERMARKING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isWatermarking() <em>Watermarking</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isWatermarking()
	 * @generated
	 * @ordered
	 */
	protected boolean watermarking = WATERMARKING_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected MLModelImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Mltop10Package.Literals.ML_MODEL;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Artifact getBase_Artifact() {
		if (base_Artifact != null && base_Artifact.eIsProxy()) {
			InternalEObject oldBase_Artifact = (InternalEObject)base_Artifact;
			base_Artifact = (Artifact)eResolveProxy(oldBase_Artifact);
			if (base_Artifact != oldBase_Artifact) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.ML_MODEL__BASE_ARTIFACT, oldBase_Artifact, base_Artifact));
			}
		}
		return base_Artifact;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Artifact basicGetBase_Artifact() {
		return base_Artifact;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_Artifact(Artifact newBase_Artifact) {
		Artifact oldBase_Artifact = base_Artifact;
		base_Artifact = newBase_Artifact;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__BASE_ARTIFACT, oldBase_Artifact, base_Artifact));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isPublic() {
		return public_;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setPublic(boolean newPublic) {
		boolean oldPublic = public_;
		public_ = newPublic;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__PUBLIC, oldPublic, public_));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isAccessControl() {
		return accessControl;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setAccessControl(boolean newAccessControl) {
		boolean oldAccessControl = accessControl;
		accessControl = newAccessControl;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__ACCESS_CONTROL, oldAccessControl, accessControl));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isAdversarialTraining() {
		return adversarialTraining;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setAdversarialTraining(boolean newAdversarialTraining) {
		boolean oldAdversarialTraining = adversarialTraining;
		adversarialTraining = newAdversarialTraining;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__ADVERSARIAL_TRAINING, oldAdversarialTraining, adversarialTraining));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isAnomalyDetection() {
		return anomalyDetection;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setAnomalyDetection(boolean newAnomalyDetection) {
		boolean oldAnomalyDetection = anomalyDetection;
		anomalyDetection = newAnomalyDetection;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__ANOMALY_DETECTION, oldAnomalyDetection, anomalyDetection));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isCryptographicallySecured() {
		return cryptographicallySecured;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setCryptographicallySecured(boolean newCryptographicallySecured) {
		boolean oldCryptographicallySecured = cryptographicallySecured;
		cryptographicallySecured = newCryptographicallySecured;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__CRYPTOGRAPHICALLY_SECURED, oldCryptographicallySecured, cryptographicallySecured));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isDefenseMechanism() {
		return defenseMechanism;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setDefenseMechanism(boolean newDefenseMechanism) {
		boolean oldDefenseMechanism = defenseMechanism;
		defenseMechanism = newDefenseMechanism;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__DEFENSE_MECHANISM, oldDefenseMechanism, defenseMechanism));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isDifferentialPrivacy() {
		return differentialPrivacy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setDifferentialPrivacy(boolean newDifferentialPrivacy) {
		boolean oldDifferentialPrivacy = differentialPrivacy;
		differentialPrivacy = newDifferentialPrivacy;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__DIFFERENTIAL_PRIVACY, oldDifferentialPrivacy, differentialPrivacy));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isEnsembleModel() {
		return ensembleModel;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setEnsembleModel(boolean newEnsembleModel) {
		boolean oldEnsembleModel = ensembleModel;
		ensembleModel = newEnsembleModel;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__ENSEMBLE_MODEL, oldEnsembleModel, ensembleModel));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isObfuscation() {
		return obfuscation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setObfuscation(boolean newObfuscation) {
		boolean oldObfuscation = obfuscation;
		obfuscation = newObfuscation;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__OBFUSCATION, oldObfuscation, obfuscation));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isLegalProtection() {
		return legalProtection;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setLegalProtection(boolean newLegalProtection) {
		boolean oldLegalProtection = legalProtection;
		legalProtection = newLegalProtection;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__LEGAL_PROTECTION, oldLegalProtection, legalProtection));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRegularBackup() {
		return regularBackup;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRegularBackup(boolean newRegularBackup) {
		boolean oldRegularBackup = regularBackup;
		regularBackup = newRegularBackup;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__REGULAR_BACKUP, oldRegularBackup, regularBackup));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRegularRetraining() {
		return regularRetraining;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRegularRetraining(boolean newRegularRetraining) {
		boolean oldRegularRetraining = regularRetraining;
		regularRetraining = newRegularRetraining;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__REGULAR_RETRAINING, oldRegularRetraining, regularRetraining));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRobustActivationFunction() {
		return robustActivationFunction;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRobustActivationFunction(boolean newRobustActivationFunction) {
		boolean oldRobustActivationFunction = robustActivationFunction;
		robustActivationFunction = newRobustActivationFunction;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__ROBUST_ACTIVATION_FUNCTION, oldRobustActivationFunction, robustActivationFunction));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRobustArchitecture() {
		return robustArchitecture;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRobustArchitecture(boolean newRobustArchitecture) {
		boolean oldRobustArchitecture = robustArchitecture;
		robustArchitecture = newRobustArchitecture;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__ROBUST_ARCHITECTURE, oldRobustArchitecture, robustArchitecture));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRegularAuditAndMonitoring() {
		return regularAuditAndMonitoring;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRegularAuditAndMonitoring(boolean newRegularAuditAndMonitoring) {
		boolean oldRegularAuditAndMonitoring = regularAuditAndMonitoring;
		regularAuditAndMonitoring = newRegularAuditAndMonitoring;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__REGULAR_AUDIT_AND_MONITORING, oldRegularAuditAndMonitoring, regularAuditAndMonitoring));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRegularPerformanceMonitoring() {
		return regularPerformanceMonitoring;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRegularPerformanceMonitoring(boolean newRegularPerformanceMonitoring) {
		boolean oldRegularPerformanceMonitoring = regularPerformanceMonitoring;
		regularPerformanceMonitoring = newRegularPerformanceMonitoring;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__REGULAR_PERFORMANCE_MONITORING, oldRegularPerformanceMonitoring, regularPerformanceMonitoring));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRegularTestingAndMonitoring() {
		return regularTestingAndMonitoring;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRegularTestingAndMonitoring(boolean newRegularTestingAndMonitoring) {
		boolean oldRegularTestingAndMonitoring = regularTestingAndMonitoring;
		regularTestingAndMonitoring = newRegularTestingAndMonitoring;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__REGULAR_TESTING_AND_MONITORING, oldRegularTestingAndMonitoring, regularTestingAndMonitoring));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isTransparency() {
		return transparency;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setTransparency(boolean newTransparency) {
		boolean oldTransparency = transparency;
		transparency = newTransparency;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__TRANSPARENCY, oldTransparency, transparency));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isValidation() {
		return validation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setValidation(boolean newValidation) {
		boolean oldValidation = validation;
		validation = newValidation;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__VALIDATION, oldValidation, validation));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isWatermarking() {
		return watermarking;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setWatermarking(boolean newWatermarking) {
		boolean oldWatermarking = watermarking;
		watermarking = newWatermarking;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.ML_MODEL__WATERMARKING, oldWatermarking, watermarking));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Mltop10Package.ML_MODEL__BASE_ARTIFACT:
				if (resolve) return getBase_Artifact();
				return basicGetBase_Artifact();
			case Mltop10Package.ML_MODEL__PUBLIC:
				return isPublic();
			case Mltop10Package.ML_MODEL__ACCESS_CONTROL:
				return isAccessControl();
			case Mltop10Package.ML_MODEL__ADVERSARIAL_TRAINING:
				return isAdversarialTraining();
			case Mltop10Package.ML_MODEL__ANOMALY_DETECTION:
				return isAnomalyDetection();
			case Mltop10Package.ML_MODEL__CRYPTOGRAPHICALLY_SECURED:
				return isCryptographicallySecured();
			case Mltop10Package.ML_MODEL__DEFENSE_MECHANISM:
				return isDefenseMechanism();
			case Mltop10Package.ML_MODEL__DIFFERENTIAL_PRIVACY:
				return isDifferentialPrivacy();
			case Mltop10Package.ML_MODEL__ENSEMBLE_MODEL:
				return isEnsembleModel();
			case Mltop10Package.ML_MODEL__OBFUSCATION:
				return isObfuscation();
			case Mltop10Package.ML_MODEL__LEGAL_PROTECTION:
				return isLegalProtection();
			case Mltop10Package.ML_MODEL__REGULAR_BACKUP:
				return isRegularBackup();
			case Mltop10Package.ML_MODEL__REGULAR_RETRAINING:
				return isRegularRetraining();
			case Mltop10Package.ML_MODEL__ROBUST_ACTIVATION_FUNCTION:
				return isRobustActivationFunction();
			case Mltop10Package.ML_MODEL__ROBUST_ARCHITECTURE:
				return isRobustArchitecture();
			case Mltop10Package.ML_MODEL__REGULAR_AUDIT_AND_MONITORING:
				return isRegularAuditAndMonitoring();
			case Mltop10Package.ML_MODEL__REGULAR_PERFORMANCE_MONITORING:
				return isRegularPerformanceMonitoring();
			case Mltop10Package.ML_MODEL__REGULAR_TESTING_AND_MONITORING:
				return isRegularTestingAndMonitoring();
			case Mltop10Package.ML_MODEL__TRANSPARENCY:
				return isTransparency();
			case Mltop10Package.ML_MODEL__VALIDATION:
				return isValidation();
			case Mltop10Package.ML_MODEL__WATERMARKING:
				return isWatermarking();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case Mltop10Package.ML_MODEL__BASE_ARTIFACT:
				setBase_Artifact((Artifact)newValue);
				return;
			case Mltop10Package.ML_MODEL__PUBLIC:
				setPublic((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__ACCESS_CONTROL:
				setAccessControl((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__ADVERSARIAL_TRAINING:
				setAdversarialTraining((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__ANOMALY_DETECTION:
				setAnomalyDetection((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__CRYPTOGRAPHICALLY_SECURED:
				setCryptographicallySecured((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__DEFENSE_MECHANISM:
				setDefenseMechanism((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__DIFFERENTIAL_PRIVACY:
				setDifferentialPrivacy((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__ENSEMBLE_MODEL:
				setEnsembleModel((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__OBFUSCATION:
				setObfuscation((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__LEGAL_PROTECTION:
				setLegalProtection((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__REGULAR_BACKUP:
				setRegularBackup((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__REGULAR_RETRAINING:
				setRegularRetraining((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__ROBUST_ACTIVATION_FUNCTION:
				setRobustActivationFunction((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__ROBUST_ARCHITECTURE:
				setRobustArchitecture((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__REGULAR_AUDIT_AND_MONITORING:
				setRegularAuditAndMonitoring((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__REGULAR_PERFORMANCE_MONITORING:
				setRegularPerformanceMonitoring((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__REGULAR_TESTING_AND_MONITORING:
				setRegularTestingAndMonitoring((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__TRANSPARENCY:
				setTransparency((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__VALIDATION:
				setValidation((Boolean)newValue);
				return;
			case Mltop10Package.ML_MODEL__WATERMARKING:
				setWatermarking((Boolean)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case Mltop10Package.ML_MODEL__BASE_ARTIFACT:
				setBase_Artifact((Artifact)null);
				return;
			case Mltop10Package.ML_MODEL__PUBLIC:
				setPublic(PUBLIC_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__ACCESS_CONTROL:
				setAccessControl(ACCESS_CONTROL_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__ADVERSARIAL_TRAINING:
				setAdversarialTraining(ADVERSARIAL_TRAINING_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__ANOMALY_DETECTION:
				setAnomalyDetection(ANOMALY_DETECTION_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__CRYPTOGRAPHICALLY_SECURED:
				setCryptographicallySecured(CRYPTOGRAPHICALLY_SECURED_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__DEFENSE_MECHANISM:
				setDefenseMechanism(DEFENSE_MECHANISM_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__DIFFERENTIAL_PRIVACY:
				setDifferentialPrivacy(DIFFERENTIAL_PRIVACY_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__ENSEMBLE_MODEL:
				setEnsembleModel(ENSEMBLE_MODEL_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__OBFUSCATION:
				setObfuscation(OBFUSCATION_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__LEGAL_PROTECTION:
				setLegalProtection(LEGAL_PROTECTION_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__REGULAR_BACKUP:
				setRegularBackup(REGULAR_BACKUP_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__REGULAR_RETRAINING:
				setRegularRetraining(REGULAR_RETRAINING_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__ROBUST_ACTIVATION_FUNCTION:
				setRobustActivationFunction(ROBUST_ACTIVATION_FUNCTION_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__ROBUST_ARCHITECTURE:
				setRobustArchitecture(ROBUST_ARCHITECTURE_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__REGULAR_AUDIT_AND_MONITORING:
				setRegularAuditAndMonitoring(REGULAR_AUDIT_AND_MONITORING_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__REGULAR_PERFORMANCE_MONITORING:
				setRegularPerformanceMonitoring(REGULAR_PERFORMANCE_MONITORING_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__REGULAR_TESTING_AND_MONITORING:
				setRegularTestingAndMonitoring(REGULAR_TESTING_AND_MONITORING_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__TRANSPARENCY:
				setTransparency(TRANSPARENCY_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__VALIDATION:
				setValidation(VALIDATION_EDEFAULT);
				return;
			case Mltop10Package.ML_MODEL__WATERMARKING:
				setWatermarking(WATERMARKING_EDEFAULT);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case Mltop10Package.ML_MODEL__BASE_ARTIFACT:
				return base_Artifact != null;
			case Mltop10Package.ML_MODEL__PUBLIC:
				return public_ != PUBLIC_EDEFAULT;
			case Mltop10Package.ML_MODEL__ACCESS_CONTROL:
				return accessControl != ACCESS_CONTROL_EDEFAULT;
			case Mltop10Package.ML_MODEL__ADVERSARIAL_TRAINING:
				return adversarialTraining != ADVERSARIAL_TRAINING_EDEFAULT;
			case Mltop10Package.ML_MODEL__ANOMALY_DETECTION:
				return anomalyDetection != ANOMALY_DETECTION_EDEFAULT;
			case Mltop10Package.ML_MODEL__CRYPTOGRAPHICALLY_SECURED:
				return cryptographicallySecured != CRYPTOGRAPHICALLY_SECURED_EDEFAULT;
			case Mltop10Package.ML_MODEL__DEFENSE_MECHANISM:
				return defenseMechanism != DEFENSE_MECHANISM_EDEFAULT;
			case Mltop10Package.ML_MODEL__DIFFERENTIAL_PRIVACY:
				return differentialPrivacy != DIFFERENTIAL_PRIVACY_EDEFAULT;
			case Mltop10Package.ML_MODEL__ENSEMBLE_MODEL:
				return ensembleModel != ENSEMBLE_MODEL_EDEFAULT;
			case Mltop10Package.ML_MODEL__OBFUSCATION:
				return obfuscation != OBFUSCATION_EDEFAULT;
			case Mltop10Package.ML_MODEL__LEGAL_PROTECTION:
				return legalProtection != LEGAL_PROTECTION_EDEFAULT;
			case Mltop10Package.ML_MODEL__REGULAR_BACKUP:
				return regularBackup != REGULAR_BACKUP_EDEFAULT;
			case Mltop10Package.ML_MODEL__REGULAR_RETRAINING:
				return regularRetraining != REGULAR_RETRAINING_EDEFAULT;
			case Mltop10Package.ML_MODEL__ROBUST_ACTIVATION_FUNCTION:
				return robustActivationFunction != ROBUST_ACTIVATION_FUNCTION_EDEFAULT;
			case Mltop10Package.ML_MODEL__ROBUST_ARCHITECTURE:
				return robustArchitecture != ROBUST_ARCHITECTURE_EDEFAULT;
			case Mltop10Package.ML_MODEL__REGULAR_AUDIT_AND_MONITORING:
				return regularAuditAndMonitoring != REGULAR_AUDIT_AND_MONITORING_EDEFAULT;
			case Mltop10Package.ML_MODEL__REGULAR_PERFORMANCE_MONITORING:
				return regularPerformanceMonitoring != REGULAR_PERFORMANCE_MONITORING_EDEFAULT;
			case Mltop10Package.ML_MODEL__REGULAR_TESTING_AND_MONITORING:
				return regularTestingAndMonitoring != REGULAR_TESTING_AND_MONITORING_EDEFAULT;
			case Mltop10Package.ML_MODEL__TRANSPARENCY:
				return transparency != TRANSPARENCY_EDEFAULT;
			case Mltop10Package.ML_MODEL__VALIDATION:
				return validation != VALIDATION_EDEFAULT;
			case Mltop10Package.ML_MODEL__WATERMARKING:
				return watermarking != WATERMARKING_EDEFAULT;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuilder result = new StringBuilder(super.toString());
		result.append(" (Public: ");
		result.append(public_);
		result.append(", AccessControl: ");
		result.append(accessControl);
		result.append(", AdversarialTraining: ");
		result.append(adversarialTraining);
		result.append(", AnomalyDetection: ");
		result.append(anomalyDetection);
		result.append(", CryptographicallySecured: ");
		result.append(cryptographicallySecured);
		result.append(", DefenseMechanism: ");
		result.append(defenseMechanism);
		result.append(", DifferentialPrivacy: ");
		result.append(differentialPrivacy);
		result.append(", EnsembleModel: ");
		result.append(ensembleModel);
		result.append(", Obfuscation: ");
		result.append(obfuscation);
		result.append(", LegalProtection: ");
		result.append(legalProtection);
		result.append(", RegularBackup: ");
		result.append(regularBackup);
		result.append(", RegularRetraining: ");
		result.append(regularRetraining);
		result.append(", RobustActivationFunction: ");
		result.append(robustActivationFunction);
		result.append(", RobustArchitecture: ");
		result.append(robustArchitecture);
		result.append(", RegularAuditAndMonitoring: ");
		result.append(regularAuditAndMonitoring);
		result.append(", RegularPerformanceMonitoring: ");
		result.append(regularPerformanceMonitoring);
		result.append(", RegularTestingAndMonitoring: ");
		result.append(regularTestingAndMonitoring);
		result.append(", Transparency: ");
		result.append(transparency);
		result.append(", Validation: ");
		result.append(validation);
		result.append(", Watermarking: ");
		result.append(watermarking);
		result.append(')');
		return result.toString();
	}

} //MLModelImpl
