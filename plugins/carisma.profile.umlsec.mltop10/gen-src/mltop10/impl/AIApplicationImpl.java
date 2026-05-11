/**
 */
package mltop10.impl;

import mltop10.AIApplication;
import mltop10.Mltop10Package;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Artifact;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>AI Application</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link mltop10.impl.AIApplicationImpl#getBase_Artifact <em>Base Artifact</em>}</li>
 *   <li>{@link mltop10.impl.AIApplicationImpl#isCheckModelResultAuthenticity <em>Check Model Result Authenticity</em>}</li>
 *   <li>{@link mltop10.impl.AIApplicationImpl#isInputValidation <em>Input Validation</em>}</li>
 *   <li>{@link mltop10.impl.AIApplicationImpl#isTamperEvidentLogging <em>Tamper Evident Logging</em>}</li>
 *   <li>{@link mltop10.impl.AIApplicationImpl#isRegularAuditAndMonitoring <em>Regular Audit And Monitoring</em>}</li>
 * </ul>
 *
 * @generated
 */
public class AIApplicationImpl extends MinimalEObjectImpl.Container implements AIApplication {
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
	 * The default value of the '{@link #isCheckModelResultAuthenticity() <em>Check Model Result Authenticity</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isCheckModelResultAuthenticity()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CHECK_MODEL_RESULT_AUTHENTICITY_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isCheckModelResultAuthenticity() <em>Check Model Result Authenticity</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isCheckModelResultAuthenticity()
	 * @generated
	 * @ordered
	 */
	protected boolean checkModelResultAuthenticity = CHECK_MODEL_RESULT_AUTHENTICITY_EDEFAULT;

	/**
	 * The default value of the '{@link #isInputValidation() <em>Input Validation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isInputValidation()
	 * @generated
	 * @ordered
	 */
	protected static final boolean INPUT_VALIDATION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isInputValidation() <em>Input Validation</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isInputValidation()
	 * @generated
	 * @ordered
	 */
	protected boolean inputValidation = INPUT_VALIDATION_EDEFAULT;

	/**
	 * The default value of the '{@link #isTamperEvidentLogging() <em>Tamper Evident Logging</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isTamperEvidentLogging()
	 * @generated
	 * @ordered
	 */
	protected static final boolean TAMPER_EVIDENT_LOGGING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isTamperEvidentLogging() <em>Tamper Evident Logging</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isTamperEvidentLogging()
	 * @generated
	 * @ordered
	 */
	protected boolean tamperEvidentLogging = TAMPER_EVIDENT_LOGGING_EDEFAULT;

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
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected AIApplicationImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Mltop10Package.Literals.AI_APPLICATION;
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.AI_APPLICATION__BASE_ARTIFACT, oldBase_Artifact, base_Artifact));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.AI_APPLICATION__BASE_ARTIFACT, oldBase_Artifact, base_Artifact));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isCheckModelResultAuthenticity() {
		return checkModelResultAuthenticity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setCheckModelResultAuthenticity(boolean newCheckModelResultAuthenticity) {
		boolean oldCheckModelResultAuthenticity = checkModelResultAuthenticity;
		checkModelResultAuthenticity = newCheckModelResultAuthenticity;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.AI_APPLICATION__CHECK_MODEL_RESULT_AUTHENTICITY, oldCheckModelResultAuthenticity, checkModelResultAuthenticity));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isInputValidation() {
		return inputValidation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setInputValidation(boolean newInputValidation) {
		boolean oldInputValidation = inputValidation;
		inputValidation = newInputValidation;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.AI_APPLICATION__INPUT_VALIDATION, oldInputValidation, inputValidation));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isTamperEvidentLogging() {
		return tamperEvidentLogging;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setTamperEvidentLogging(boolean newTamperEvidentLogging) {
		boolean oldTamperEvidentLogging = tamperEvidentLogging;
		tamperEvidentLogging = newTamperEvidentLogging;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.AI_APPLICATION__TAMPER_EVIDENT_LOGGING, oldTamperEvidentLogging, tamperEvidentLogging));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.AI_APPLICATION__REGULAR_AUDIT_AND_MONITORING, oldRegularAuditAndMonitoring, regularAuditAndMonitoring));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Mltop10Package.AI_APPLICATION__BASE_ARTIFACT:
				if (resolve) return getBase_Artifact();
				return basicGetBase_Artifact();
			case Mltop10Package.AI_APPLICATION__CHECK_MODEL_RESULT_AUTHENTICITY:
				return isCheckModelResultAuthenticity();
			case Mltop10Package.AI_APPLICATION__INPUT_VALIDATION:
				return isInputValidation();
			case Mltop10Package.AI_APPLICATION__TAMPER_EVIDENT_LOGGING:
				return isTamperEvidentLogging();
			case Mltop10Package.AI_APPLICATION__REGULAR_AUDIT_AND_MONITORING:
				return isRegularAuditAndMonitoring();
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
			case Mltop10Package.AI_APPLICATION__BASE_ARTIFACT:
				setBase_Artifact((Artifact)newValue);
				return;
			case Mltop10Package.AI_APPLICATION__CHECK_MODEL_RESULT_AUTHENTICITY:
				setCheckModelResultAuthenticity((Boolean)newValue);
				return;
			case Mltop10Package.AI_APPLICATION__INPUT_VALIDATION:
				setInputValidation((Boolean)newValue);
				return;
			case Mltop10Package.AI_APPLICATION__TAMPER_EVIDENT_LOGGING:
				setTamperEvidentLogging((Boolean)newValue);
				return;
			case Mltop10Package.AI_APPLICATION__REGULAR_AUDIT_AND_MONITORING:
				setRegularAuditAndMonitoring((Boolean)newValue);
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
			case Mltop10Package.AI_APPLICATION__BASE_ARTIFACT:
				setBase_Artifact((Artifact)null);
				return;
			case Mltop10Package.AI_APPLICATION__CHECK_MODEL_RESULT_AUTHENTICITY:
				setCheckModelResultAuthenticity(CHECK_MODEL_RESULT_AUTHENTICITY_EDEFAULT);
				return;
			case Mltop10Package.AI_APPLICATION__INPUT_VALIDATION:
				setInputValidation(INPUT_VALIDATION_EDEFAULT);
				return;
			case Mltop10Package.AI_APPLICATION__TAMPER_EVIDENT_LOGGING:
				setTamperEvidentLogging(TAMPER_EVIDENT_LOGGING_EDEFAULT);
				return;
			case Mltop10Package.AI_APPLICATION__REGULAR_AUDIT_AND_MONITORING:
				setRegularAuditAndMonitoring(REGULAR_AUDIT_AND_MONITORING_EDEFAULT);
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
			case Mltop10Package.AI_APPLICATION__BASE_ARTIFACT:
				return base_Artifact != null;
			case Mltop10Package.AI_APPLICATION__CHECK_MODEL_RESULT_AUTHENTICITY:
				return checkModelResultAuthenticity != CHECK_MODEL_RESULT_AUTHENTICITY_EDEFAULT;
			case Mltop10Package.AI_APPLICATION__INPUT_VALIDATION:
				return inputValidation != INPUT_VALIDATION_EDEFAULT;
			case Mltop10Package.AI_APPLICATION__TAMPER_EVIDENT_LOGGING:
				return tamperEvidentLogging != TAMPER_EVIDENT_LOGGING_EDEFAULT;
			case Mltop10Package.AI_APPLICATION__REGULAR_AUDIT_AND_MONITORING:
				return regularAuditAndMonitoring != REGULAR_AUDIT_AND_MONITORING_EDEFAULT;
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
		result.append(" (CheckModelResultAuthenticity: ");
		result.append(checkModelResultAuthenticity);
		result.append(", InputValidation: ");
		result.append(inputValidation);
		result.append(", TamperEvidentLogging: ");
		result.append(tamperEvidentLogging);
		result.append(", RegularAuditAndMonitoring: ");
		result.append(regularAuditAndMonitoring);
		result.append(')');
		return result.toString();
	}

} //AIApplicationImpl
