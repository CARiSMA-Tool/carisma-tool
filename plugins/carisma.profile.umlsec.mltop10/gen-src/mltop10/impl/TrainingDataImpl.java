/**
 */
package mltop10.impl;

import mltop10.Mltop10Package;
import mltop10.TrainingData;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Training Data</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link mltop10.impl.TrainingDataImpl#getBase_Artifact <em>Base Artifact</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isPublic <em>Public</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isAccessControl <em>Access Control</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isAnomalyDetection <em>Anomaly Detection</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isReduced <em>Reduced</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isRegularUpdatesAndTraining <em>Regular Updates And Training</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isTrusted <em>Trusted</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isValidation <em>Validation</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isVerification <em>Verification</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isWatermarking <em>Watermarking</em>}</li>
 *   <li>{@link mltop10.impl.TrainingDataImpl#isRegularBackup <em>Regular Backup</em>}</li>
 * </ul>
 *
 * @generated
 */
public class TrainingDataImpl extends MinimalEObjectImpl.Container implements TrainingData {
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
	 * The default value of the '{@link #isReduced() <em>Reduced</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isReduced()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REDUCED_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isReduced() <em>Reduced</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isReduced()
	 * @generated
	 * @ordered
	 */
	protected boolean reduced = REDUCED_EDEFAULT;

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
	 * The default value of the '{@link #isRegularUpdatesAndTraining() <em>Regular Updates And Training</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularUpdatesAndTraining()
	 * @generated
	 * @ordered
	 */
	protected static final boolean REGULAR_UPDATES_AND_TRAINING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isRegularUpdatesAndTraining() <em>Regular Updates And Training</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isRegularUpdatesAndTraining()
	 * @generated
	 * @ordered
	 */
	protected boolean regularUpdatesAndTraining = REGULAR_UPDATES_AND_TRAINING_EDEFAULT;

	/**
	 * The default value of the '{@link #isTrusted() <em>Trusted</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isTrusted()
	 * @generated
	 * @ordered
	 */
	protected static final boolean TRUSTED_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isTrusted() <em>Trusted</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isTrusted()
	 * @generated
	 * @ordered
	 */
	protected boolean trusted = TRUSTED_EDEFAULT;

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
	 * The default value of the '{@link #isVerification() <em>Verification</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isVerification()
	 * @generated
	 * @ordered
	 */
	protected static final boolean VERIFICATION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isVerification() <em>Verification</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isVerification()
	 * @generated
	 * @ordered
	 */
	protected boolean verification = VERIFICATION_EDEFAULT;

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
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TrainingDataImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Mltop10Package.Literals.TRAINING_DATA;
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.TRAINING_DATA__BASE_ARTIFACT, oldBase_Artifact, base_Artifact));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__BASE_ARTIFACT, oldBase_Artifact, base_Artifact));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__PUBLIC, oldPublic, public_));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__ACCESS_CONTROL, oldAccessControl, accessControl));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__ANOMALY_DETECTION, oldAnomalyDetection, anomalyDetection));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isReduced() {
		return reduced;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setReduced(boolean newReduced) {
		boolean oldReduced = reduced;
		reduced = newReduced;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__REDUCED, oldReduced, reduced));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__REGULAR_AUDIT_AND_MONITORING, oldRegularAuditAndMonitoring, regularAuditAndMonitoring));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isRegularUpdatesAndTraining() {
		return regularUpdatesAndTraining;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRegularUpdatesAndTraining(boolean newRegularUpdatesAndTraining) {
		boolean oldRegularUpdatesAndTraining = regularUpdatesAndTraining;
		regularUpdatesAndTraining = newRegularUpdatesAndTraining;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__REGULAR_UPDATES_AND_TRAINING, oldRegularUpdatesAndTraining, regularUpdatesAndTraining));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isTrusted() {
		return trusted;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setTrusted(boolean newTrusted) {
		boolean oldTrusted = trusted;
		trusted = newTrusted;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__TRUSTED, oldTrusted, trusted));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__VALIDATION, oldValidation, validation));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isVerification() {
		return verification;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setVerification(boolean newVerification) {
		boolean oldVerification = verification;
		verification = newVerification;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__VERIFICATION, oldVerification, verification));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__WATERMARKING, oldWatermarking, watermarking));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.TRAINING_DATA__REGULAR_BACKUP, oldRegularBackup, regularBackup));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Mltop10Package.TRAINING_DATA__BASE_ARTIFACT:
				if (resolve) return getBase_Artifact();
				return basicGetBase_Artifact();
			case Mltop10Package.TRAINING_DATA__PUBLIC:
				return isPublic();
			case Mltop10Package.TRAINING_DATA__ACCESS_CONTROL:
				return isAccessControl();
			case Mltop10Package.TRAINING_DATA__ANOMALY_DETECTION:
				return isAnomalyDetection();
			case Mltop10Package.TRAINING_DATA__REDUCED:
				return isReduced();
			case Mltop10Package.TRAINING_DATA__REGULAR_AUDIT_AND_MONITORING:
				return isRegularAuditAndMonitoring();
			case Mltop10Package.TRAINING_DATA__REGULAR_UPDATES_AND_TRAINING:
				return isRegularUpdatesAndTraining();
			case Mltop10Package.TRAINING_DATA__TRUSTED:
				return isTrusted();
			case Mltop10Package.TRAINING_DATA__VALIDATION:
				return isValidation();
			case Mltop10Package.TRAINING_DATA__VERIFICATION:
				return isVerification();
			case Mltop10Package.TRAINING_DATA__WATERMARKING:
				return isWatermarking();
			case Mltop10Package.TRAINING_DATA__REGULAR_BACKUP:
				return isRegularBackup();
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
			case Mltop10Package.TRAINING_DATA__BASE_ARTIFACT:
				setBase_Artifact((Artifact)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__PUBLIC:
				setPublic((Boolean)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__ACCESS_CONTROL:
				setAccessControl((Boolean)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__ANOMALY_DETECTION:
				setAnomalyDetection((Boolean)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__REDUCED:
				setReduced((Boolean)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__REGULAR_AUDIT_AND_MONITORING:
				setRegularAuditAndMonitoring((Boolean)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__REGULAR_UPDATES_AND_TRAINING:
				setRegularUpdatesAndTraining((Boolean)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__TRUSTED:
				setTrusted((Boolean)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__VALIDATION:
				setValidation((Boolean)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__VERIFICATION:
				setVerification((Boolean)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__WATERMARKING:
				setWatermarking((Boolean)newValue);
				return;
			case Mltop10Package.TRAINING_DATA__REGULAR_BACKUP:
				setRegularBackup((Boolean)newValue);
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
			case Mltop10Package.TRAINING_DATA__BASE_ARTIFACT:
				setBase_Artifact((Artifact)null);
				return;
			case Mltop10Package.TRAINING_DATA__PUBLIC:
				setPublic(PUBLIC_EDEFAULT);
				return;
			case Mltop10Package.TRAINING_DATA__ACCESS_CONTROL:
				setAccessControl(ACCESS_CONTROL_EDEFAULT);
				return;
			case Mltop10Package.TRAINING_DATA__ANOMALY_DETECTION:
				setAnomalyDetection(ANOMALY_DETECTION_EDEFAULT);
				return;
			case Mltop10Package.TRAINING_DATA__REDUCED:
				setReduced(REDUCED_EDEFAULT);
				return;
			case Mltop10Package.TRAINING_DATA__REGULAR_AUDIT_AND_MONITORING:
				setRegularAuditAndMonitoring(REGULAR_AUDIT_AND_MONITORING_EDEFAULT);
				return;
			case Mltop10Package.TRAINING_DATA__REGULAR_UPDATES_AND_TRAINING:
				setRegularUpdatesAndTraining(REGULAR_UPDATES_AND_TRAINING_EDEFAULT);
				return;
			case Mltop10Package.TRAINING_DATA__TRUSTED:
				setTrusted(TRUSTED_EDEFAULT);
				return;
			case Mltop10Package.TRAINING_DATA__VALIDATION:
				setValidation(VALIDATION_EDEFAULT);
				return;
			case Mltop10Package.TRAINING_DATA__VERIFICATION:
				setVerification(VERIFICATION_EDEFAULT);
				return;
			case Mltop10Package.TRAINING_DATA__WATERMARKING:
				setWatermarking(WATERMARKING_EDEFAULT);
				return;
			case Mltop10Package.TRAINING_DATA__REGULAR_BACKUP:
				setRegularBackup(REGULAR_BACKUP_EDEFAULT);
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
			case Mltop10Package.TRAINING_DATA__BASE_ARTIFACT:
				return base_Artifact != null;
			case Mltop10Package.TRAINING_DATA__PUBLIC:
				return public_ != PUBLIC_EDEFAULT;
			case Mltop10Package.TRAINING_DATA__ACCESS_CONTROL:
				return accessControl != ACCESS_CONTROL_EDEFAULT;
			case Mltop10Package.TRAINING_DATA__ANOMALY_DETECTION:
				return anomalyDetection != ANOMALY_DETECTION_EDEFAULT;
			case Mltop10Package.TRAINING_DATA__REDUCED:
				return reduced != REDUCED_EDEFAULT;
			case Mltop10Package.TRAINING_DATA__REGULAR_AUDIT_AND_MONITORING:
				return regularAuditAndMonitoring != REGULAR_AUDIT_AND_MONITORING_EDEFAULT;
			case Mltop10Package.TRAINING_DATA__REGULAR_UPDATES_AND_TRAINING:
				return regularUpdatesAndTraining != REGULAR_UPDATES_AND_TRAINING_EDEFAULT;
			case Mltop10Package.TRAINING_DATA__TRUSTED:
				return trusted != TRUSTED_EDEFAULT;
			case Mltop10Package.TRAINING_DATA__VALIDATION:
				return validation != VALIDATION_EDEFAULT;
			case Mltop10Package.TRAINING_DATA__VERIFICATION:
				return verification != VERIFICATION_EDEFAULT;
			case Mltop10Package.TRAINING_DATA__WATERMARKING:
				return watermarking != WATERMARKING_EDEFAULT;
			case Mltop10Package.TRAINING_DATA__REGULAR_BACKUP:
				return regularBackup != REGULAR_BACKUP_EDEFAULT;
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
		result.append(", AnomalyDetection: ");
		result.append(anomalyDetection);
		result.append(", Reduced: ");
		result.append(reduced);
		result.append(", RegularAuditAndMonitoring: ");
		result.append(regularAuditAndMonitoring);
		result.append(", RegularUpdatesAndTraining: ");
		result.append(regularUpdatesAndTraining);
		result.append(", Trusted: ");
		result.append(trusted);
		result.append(", Validation: ");
		result.append(validation);
		result.append(", Verification: ");
		result.append(verification);
		result.append(", Watermarking: ");
		result.append(watermarking);
		result.append(", RegularBackup: ");
		result.append(regularBackup);
		result.append(')');
		return result.toString();
	}

} //TrainingDataImpl
