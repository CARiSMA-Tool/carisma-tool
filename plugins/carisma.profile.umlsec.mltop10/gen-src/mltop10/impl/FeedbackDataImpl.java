/**
 */
package mltop10.impl;

import mltop10.FeedbackData;
import mltop10.Mltop10Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Feedback Data</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link mltop10.impl.FeedbackDataImpl#getBase_Artifact <em>Base Artifact</em>}</li>
 *   <li>{@link mltop10.impl.FeedbackDataImpl#isAccessControl <em>Access Control</em>}</li>
 *   <li>{@link mltop10.impl.FeedbackDataImpl#isAnomalyDetection <em>Anomaly Detection</em>}</li>
 *   <li>{@link mltop10.impl.FeedbackDataImpl#isAuthenticityVerified <em>Authenticity Verified</em>}</li>
 *   <li>{@link mltop10.impl.FeedbackDataImpl#isCleaning <em>Cleaning</em>}</li>
 *   <li>{@link mltop10.impl.FeedbackDataImpl#isValidation <em>Validation</em>}</li>
 * </ul>
 *
 * @generated
 */
public class FeedbackDataImpl extends MinimalEObjectImpl.Container implements FeedbackData {
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
	 * The default value of the '{@link #isAuthenticityVerified() <em>Authenticity Verified</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAuthenticityVerified()
	 * @generated
	 * @ordered
	 */
	protected static final boolean AUTHENTICITY_VERIFIED_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isAuthenticityVerified() <em>Authenticity Verified</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isAuthenticityVerified()
	 * @generated
	 * @ordered
	 */
	protected boolean authenticityVerified = AUTHENTICITY_VERIFIED_EDEFAULT;

	/**
	 * The default value of the '{@link #isCleaning() <em>Cleaning</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isCleaning()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CLEANING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isCleaning() <em>Cleaning</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isCleaning()
	 * @generated
	 * @ordered
	 */
	protected boolean cleaning = CLEANING_EDEFAULT;

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
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected FeedbackDataImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Mltop10Package.Literals.FEEDBACK_DATA;
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.FEEDBACK_DATA__BASE_ARTIFACT, oldBase_Artifact, base_Artifact));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.FEEDBACK_DATA__BASE_ARTIFACT, oldBase_Artifact, base_Artifact));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.FEEDBACK_DATA__ACCESS_CONTROL, oldAccessControl, accessControl));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.FEEDBACK_DATA__ANOMALY_DETECTION, oldAnomalyDetection, anomalyDetection));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isAuthenticityVerified() {
		return authenticityVerified;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setAuthenticityVerified(boolean newAuthenticityVerified) {
		boolean oldAuthenticityVerified = authenticityVerified;
		authenticityVerified = newAuthenticityVerified;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.FEEDBACK_DATA__AUTHENTICITY_VERIFIED, oldAuthenticityVerified, authenticityVerified));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isCleaning() {
		return cleaning;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setCleaning(boolean newCleaning) {
		boolean oldCleaning = cleaning;
		cleaning = newCleaning;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.FEEDBACK_DATA__CLEANING, oldCleaning, cleaning));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.FEEDBACK_DATA__VALIDATION, oldValidation, validation));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Mltop10Package.FEEDBACK_DATA__BASE_ARTIFACT:
				if (resolve) return getBase_Artifact();
				return basicGetBase_Artifact();
			case Mltop10Package.FEEDBACK_DATA__ACCESS_CONTROL:
				return isAccessControl();
			case Mltop10Package.FEEDBACK_DATA__ANOMALY_DETECTION:
				return isAnomalyDetection();
			case Mltop10Package.FEEDBACK_DATA__AUTHENTICITY_VERIFIED:
				return isAuthenticityVerified();
			case Mltop10Package.FEEDBACK_DATA__CLEANING:
				return isCleaning();
			case Mltop10Package.FEEDBACK_DATA__VALIDATION:
				return isValidation();
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
			case Mltop10Package.FEEDBACK_DATA__BASE_ARTIFACT:
				setBase_Artifact((Artifact)newValue);
				return;
			case Mltop10Package.FEEDBACK_DATA__ACCESS_CONTROL:
				setAccessControl((Boolean)newValue);
				return;
			case Mltop10Package.FEEDBACK_DATA__ANOMALY_DETECTION:
				setAnomalyDetection((Boolean)newValue);
				return;
			case Mltop10Package.FEEDBACK_DATA__AUTHENTICITY_VERIFIED:
				setAuthenticityVerified((Boolean)newValue);
				return;
			case Mltop10Package.FEEDBACK_DATA__CLEANING:
				setCleaning((Boolean)newValue);
				return;
			case Mltop10Package.FEEDBACK_DATA__VALIDATION:
				setValidation((Boolean)newValue);
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
			case Mltop10Package.FEEDBACK_DATA__BASE_ARTIFACT:
				setBase_Artifact((Artifact)null);
				return;
			case Mltop10Package.FEEDBACK_DATA__ACCESS_CONTROL:
				setAccessControl(ACCESS_CONTROL_EDEFAULT);
				return;
			case Mltop10Package.FEEDBACK_DATA__ANOMALY_DETECTION:
				setAnomalyDetection(ANOMALY_DETECTION_EDEFAULT);
				return;
			case Mltop10Package.FEEDBACK_DATA__AUTHENTICITY_VERIFIED:
				setAuthenticityVerified(AUTHENTICITY_VERIFIED_EDEFAULT);
				return;
			case Mltop10Package.FEEDBACK_DATA__CLEANING:
				setCleaning(CLEANING_EDEFAULT);
				return;
			case Mltop10Package.FEEDBACK_DATA__VALIDATION:
				setValidation(VALIDATION_EDEFAULT);
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
			case Mltop10Package.FEEDBACK_DATA__BASE_ARTIFACT:
				return base_Artifact != null;
			case Mltop10Package.FEEDBACK_DATA__ACCESS_CONTROL:
				return accessControl != ACCESS_CONTROL_EDEFAULT;
			case Mltop10Package.FEEDBACK_DATA__ANOMALY_DETECTION:
				return anomalyDetection != ANOMALY_DETECTION_EDEFAULT;
			case Mltop10Package.FEEDBACK_DATA__AUTHENTICITY_VERIFIED:
				return authenticityVerified != AUTHENTICITY_VERIFIED_EDEFAULT;
			case Mltop10Package.FEEDBACK_DATA__CLEANING:
				return cleaning != CLEANING_EDEFAULT;
			case Mltop10Package.FEEDBACK_DATA__VALIDATION:
				return validation != VALIDATION_EDEFAULT;
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
		result.append(" (AccessControl: ");
		result.append(accessControl);
		result.append(", AnomalyDetection: ");
		result.append(anomalyDetection);
		result.append(", AuthenticityVerified: ");
		result.append(authenticityVerified);
		result.append(", Cleaning: ");
		result.append(cleaning);
		result.append(", Validation: ");
		result.append(validation);
		result.append(')');
		return result.toString();
	}

} //FeedbackDataImpl
