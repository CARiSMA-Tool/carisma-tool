/**
 */
package mltop10.impl;

import mltop10.Mltop10Package;
import mltop10.SecureCommPath;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.CommunicationPath;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Secure Comm Path</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link mltop10.impl.SecureCommPathImpl#getBase_CommunicationPath <em>Base Communication Path</em>}</li>
 *   <li>{@link mltop10.impl.SecureCommPathImpl#isConfidentialityPreserving <em>Confidentiality Preserving</em>}</li>
 *   <li>{@link mltop10.impl.SecureCommPathImpl#isIntegrityPreserving <em>Integrity Preserving</em>}</li>
 * </ul>
 *
 * @generated
 */
public class SecureCommPathImpl extends MinimalEObjectImpl.Container implements SecureCommPath {
	/**
	 * The cached value of the '{@link #getBase_CommunicationPath() <em>Base Communication Path</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_CommunicationPath()
	 * @generated
	 * @ordered
	 */
	protected CommunicationPath base_CommunicationPath;

	/**
	 * The default value of the '{@link #isConfidentialityPreserving() <em>Confidentiality Preserving</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isConfidentialityPreserving()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CONFIDENTIALITY_PRESERVING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isConfidentialityPreserving() <em>Confidentiality Preserving</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isConfidentialityPreserving()
	 * @generated
	 * @ordered
	 */
	protected boolean confidentialityPreserving = CONFIDENTIALITY_PRESERVING_EDEFAULT;

	/**
	 * The default value of the '{@link #isIntegrityPreserving() <em>Integrity Preserving</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isIntegrityPreserving()
	 * @generated
	 * @ordered
	 */
	protected static final boolean INTEGRITY_PRESERVING_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isIntegrityPreserving() <em>Integrity Preserving</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isIntegrityPreserving()
	 * @generated
	 * @ordered
	 */
	protected boolean integrityPreserving = INTEGRITY_PRESERVING_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SecureCommPathImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Mltop10Package.Literals.SECURE_COMM_PATH;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public CommunicationPath getBase_CommunicationPath() {
		if (base_CommunicationPath != null && base_CommunicationPath.eIsProxy()) {
			InternalEObject oldBase_CommunicationPath = (InternalEObject)base_CommunicationPath;
			base_CommunicationPath = (CommunicationPath)eResolveProxy(oldBase_CommunicationPath);
			if (base_CommunicationPath != oldBase_CommunicationPath) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Mltop10Package.SECURE_COMM_PATH__BASE_COMMUNICATION_PATH, oldBase_CommunicationPath, base_CommunicationPath));
			}
		}
		return base_CommunicationPath;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CommunicationPath basicGetBase_CommunicationPath() {
		return base_CommunicationPath;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_CommunicationPath(CommunicationPath newBase_CommunicationPath) {
		CommunicationPath oldBase_CommunicationPath = base_CommunicationPath;
		base_CommunicationPath = newBase_CommunicationPath;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.SECURE_COMM_PATH__BASE_COMMUNICATION_PATH, oldBase_CommunicationPath, base_CommunicationPath));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isConfidentialityPreserving() {
		return confidentialityPreserving;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setConfidentialityPreserving(boolean newConfidentialityPreserving) {
		boolean oldConfidentialityPreserving = confidentialityPreserving;
		confidentialityPreserving = newConfidentialityPreserving;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.SECURE_COMM_PATH__CONFIDENTIALITY_PRESERVING, oldConfidentialityPreserving, confidentialityPreserving));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean isIntegrityPreserving() {
		return integrityPreserving;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setIntegrityPreserving(boolean newIntegrityPreserving) {
		boolean oldIntegrityPreserving = integrityPreserving;
		integrityPreserving = newIntegrityPreserving;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Mltop10Package.SECURE_COMM_PATH__INTEGRITY_PRESERVING, oldIntegrityPreserving, integrityPreserving));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Mltop10Package.SECURE_COMM_PATH__BASE_COMMUNICATION_PATH:
				if (resolve) return getBase_CommunicationPath();
				return basicGetBase_CommunicationPath();
			case Mltop10Package.SECURE_COMM_PATH__CONFIDENTIALITY_PRESERVING:
				return isConfidentialityPreserving();
			case Mltop10Package.SECURE_COMM_PATH__INTEGRITY_PRESERVING:
				return isIntegrityPreserving();
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
			case Mltop10Package.SECURE_COMM_PATH__BASE_COMMUNICATION_PATH:
				setBase_CommunicationPath((CommunicationPath)newValue);
				return;
			case Mltop10Package.SECURE_COMM_PATH__CONFIDENTIALITY_PRESERVING:
				setConfidentialityPreserving((Boolean)newValue);
				return;
			case Mltop10Package.SECURE_COMM_PATH__INTEGRITY_PRESERVING:
				setIntegrityPreserving((Boolean)newValue);
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
			case Mltop10Package.SECURE_COMM_PATH__BASE_COMMUNICATION_PATH:
				setBase_CommunicationPath((CommunicationPath)null);
				return;
			case Mltop10Package.SECURE_COMM_PATH__CONFIDENTIALITY_PRESERVING:
				setConfidentialityPreserving(CONFIDENTIALITY_PRESERVING_EDEFAULT);
				return;
			case Mltop10Package.SECURE_COMM_PATH__INTEGRITY_PRESERVING:
				setIntegrityPreserving(INTEGRITY_PRESERVING_EDEFAULT);
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
			case Mltop10Package.SECURE_COMM_PATH__BASE_COMMUNICATION_PATH:
				return base_CommunicationPath != null;
			case Mltop10Package.SECURE_COMM_PATH__CONFIDENTIALITY_PRESERVING:
				return confidentialityPreserving != CONFIDENTIALITY_PRESERVING_EDEFAULT;
			case Mltop10Package.SECURE_COMM_PATH__INTEGRITY_PRESERVING:
				return integrityPreserving != INTEGRITY_PRESERVING_EDEFAULT;
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
		result.append(" (ConfidentialityPreserving: ");
		result.append(confidentialityPreserving);
		result.append(", IntegrityPreserving: ");
		result.append(integrityPreserving);
		result.append(')');
		return result.toString();
	}

} //SecureCommPathImpl
