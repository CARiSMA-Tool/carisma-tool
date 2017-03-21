/**
 */
package UMLsecenc.impl;

import UMLsecenc.UMLsecencPackage;
import UMLsecenc.encryptedenc;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.CommunicationPath;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>encryptedenc</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link UMLsecenc.impl.encryptedencImpl#getAlg <em>Alg</em>}</li>
 *   <li>{@link UMLsecenc.impl.encryptedencImpl#getKeylength <em>Keylength</em>}</li>
 *   <li>{@link UMLsecenc.impl.encryptedencImpl#getBase_CommunicationPath <em>Base Communication Path</em>}</li>
 * </ul>
 *
 * @generated
 */
public class encryptedencImpl extends MinimalEObjectImpl.Container implements encryptedenc {
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
	 * The cached value of the '{@link #getBase_CommunicationPath() <em>Base Communication Path</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_CommunicationPath()
	 * @generated
	 * @ordered
	 */
	protected CommunicationPath base_CommunicationPath;

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
		return UMLsecencPackage.Literals.ENCRYPTEDENC;
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
			eNotify(new ENotificationImpl(this, Notification.SET, UMLsecencPackage.ENCRYPTEDENC__ALG, oldAlg, alg));
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
			eNotify(new ENotificationImpl(this, Notification.SET, UMLsecencPackage.ENCRYPTEDENC__KEYLENGTH, oldKeylength, keylength));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CommunicationPath getBase_CommunicationPath() {
		if (base_CommunicationPath != null && base_CommunicationPath.eIsProxy()) {
			InternalEObject oldBase_CommunicationPath = (InternalEObject)base_CommunicationPath;
			base_CommunicationPath = (CommunicationPath)eResolveProxy(oldBase_CommunicationPath);
			if (base_CommunicationPath != oldBase_CommunicationPath) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UMLsecencPackage.ENCRYPTEDENC__BASE_COMMUNICATION_PATH, oldBase_CommunicationPath, base_CommunicationPath));
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
	public void setBase_CommunicationPath(CommunicationPath newBase_CommunicationPath) {
		CommunicationPath oldBase_CommunicationPath = base_CommunicationPath;
		base_CommunicationPath = newBase_CommunicationPath;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UMLsecencPackage.ENCRYPTEDENC__BASE_COMMUNICATION_PATH, oldBase_CommunicationPath, base_CommunicationPath));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UMLsecencPackage.ENCRYPTEDENC__ALG:
				return getAlg();
			case UMLsecencPackage.ENCRYPTEDENC__KEYLENGTH:
				return getKeylength();
			case UMLsecencPackage.ENCRYPTEDENC__BASE_COMMUNICATION_PATH:
				if (resolve) return getBase_CommunicationPath();
				return basicGetBase_CommunicationPath();
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
			case UMLsecencPackage.ENCRYPTEDENC__ALG:
				setAlg((String)newValue);
				return;
			case UMLsecencPackage.ENCRYPTEDENC__KEYLENGTH:
				setKeylength((String)newValue);
				return;
			case UMLsecencPackage.ENCRYPTEDENC__BASE_COMMUNICATION_PATH:
				setBase_CommunicationPath((CommunicationPath)newValue);
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
			case UMLsecencPackage.ENCRYPTEDENC__ALG:
				setAlg(ALG_EDEFAULT);
				return;
			case UMLsecencPackage.ENCRYPTEDENC__KEYLENGTH:
				setKeylength(KEYLENGTH_EDEFAULT);
				return;
			case UMLsecencPackage.ENCRYPTEDENC__BASE_COMMUNICATION_PATH:
				setBase_CommunicationPath((CommunicationPath)null);
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
			case UMLsecencPackage.ENCRYPTEDENC__ALG:
				return ALG_EDEFAULT == null ? alg != null : !ALG_EDEFAULT.equals(alg);
			case UMLsecencPackage.ENCRYPTEDENC__KEYLENGTH:
				return KEYLENGTH_EDEFAULT == null ? keylength != null : !KEYLENGTH_EDEFAULT.equals(keylength);
			case UMLsecencPackage.ENCRYPTEDENC__BASE_COMMUNICATION_PATH:
				return base_CommunicationPath != null;
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
