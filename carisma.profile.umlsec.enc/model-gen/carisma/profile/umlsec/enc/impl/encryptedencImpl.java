/**
 */
package carisma.profile.umlsec.enc.impl;

import carisma.profile.umlsec.enc.EncPackage;
import carisma.profile.umlsec.enc.encryptedenc;

import carisma.profile.umlsec.impl.encryptedImpl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>encryptedenc</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.enc.impl.encryptedencImpl#getAlg <em>Alg</em>}</li>
 *   <li>{@link carisma.profile.umlsec.enc.impl.encryptedencImpl#getKeylength <em>Keylength</em>}</li>
 * </ul>
 *
 * @generated
 */
public class encryptedencImpl extends encryptedImpl implements encryptedenc {
	/**
	 * The default value of the '{@link #getAlg() <em>Alg</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAlg()
	 * @generated
	 * @ordered
	 */
	protected static final String ALG_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getAlg() <em>Alg</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAlg()
	 * @generated
	 * @ordered
	 */
	protected String alg = ALG_EDEFAULT;

	/**
	 * The default value of the '{@link #getKeylength() <em>Keylength</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getKeylength()
	 * @generated
	 * @ordered
	 */
	protected static final String KEYLENGTH_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getKeylength() <em>Keylength</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getKeylength()
	 * @generated
	 * @ordered
	 */
	protected String keylength = KEYLENGTH_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected encryptedencImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return EncPackage.Literals.ENCRYPTEDENC;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getAlg() {
		return alg;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAlg(String newAlg) {
		String oldAlg = alg;
		alg = newAlg;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, EncPackage.ENCRYPTEDENC__ALG, oldAlg, alg));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getKeylength() {
		return keylength;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setKeylength(String newKeylength) {
		String oldKeylength = keylength;
		keylength = newKeylength;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, EncPackage.ENCRYPTEDENC__KEYLENGTH, oldKeylength, keylength));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case EncPackage.ENCRYPTEDENC__ALG:
				return getAlg();
			case EncPackage.ENCRYPTEDENC__KEYLENGTH:
				return getKeylength();
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
			case EncPackage.ENCRYPTEDENC__ALG:
				setAlg((String)newValue);
				return;
			case EncPackage.ENCRYPTEDENC__KEYLENGTH:
				setKeylength((String)newValue);
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
			case EncPackage.ENCRYPTEDENC__ALG:
				setAlg(ALG_EDEFAULT);
				return;
			case EncPackage.ENCRYPTEDENC__KEYLENGTH:
				setKeylength(KEYLENGTH_EDEFAULT);
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
			case EncPackage.ENCRYPTEDENC__ALG:
				return ALG_EDEFAULT == null ? alg != null : !ALG_EDEFAULT.equals(alg);
			case EncPackage.ENCRYPTEDENC__KEYLENGTH:
				return KEYLENGTH_EDEFAULT == null ? keylength != null : !KEYLENGTH_EDEFAULT.equals(keylength);
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

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (alg: ");
		result.append(alg);
		result.append(", keylength: ");
		result.append(keylength);
		result.append(')');
		return result.toString();
	}

} //encryptedencImpl
