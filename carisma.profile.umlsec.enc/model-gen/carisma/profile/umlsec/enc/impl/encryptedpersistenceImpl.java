/**
 */
package carisma.profile.umlsec.enc.impl;

import carisma.profile.umlsec.enc.EncPackage;
import carisma.profile.umlsec.enc.encryptedpersistence;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>encryptedpersistence</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.enc.impl.encryptedpersistenceImpl#getBase_Class <em>Base Class</em>}</li>
 *   <li>{@link carisma.profile.umlsec.enc.impl.encryptedpersistenceImpl#getAlg <em>Alg</em>}</li>
 *   <li>{@link carisma.profile.umlsec.enc.impl.encryptedpersistenceImpl#getKeylength <em>Keylength</em>}</li>
 * </ul>
 *
 * @generated
 */
public class encryptedpersistenceImpl extends MinimalEObjectImpl.Container implements encryptedpersistence {
	/**
	 * The cached value of the '{@link #getBase_Class() <em>Base Class</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Class()
	 * @generated
	 * @ordered
	 */
	protected org.eclipse.uml2.uml.Class base_Class;

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
	protected encryptedpersistenceImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return EncPackage.Literals.ENCRYPTEDPERSISTENCE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.uml2.uml.Class getBase_Class() {
		if (base_Class != null && base_Class.eIsProxy()) {
			InternalEObject oldBase_Class = (InternalEObject)base_Class;
			base_Class = (org.eclipse.uml2.uml.Class)eResolveProxy(oldBase_Class);
			if (base_Class != oldBase_Class) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, EncPackage.ENCRYPTEDPERSISTENCE__BASE_CLASS, oldBase_Class, base_Class));
			}
		}
		return base_Class;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.uml2.uml.Class basicGetBase_Class() {
		return base_Class;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBase_Class(org.eclipse.uml2.uml.Class newBase_Class) {
		org.eclipse.uml2.uml.Class oldBase_Class = base_Class;
		base_Class = newBase_Class;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, EncPackage.ENCRYPTEDPERSISTENCE__BASE_CLASS, oldBase_Class, base_Class));
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
			eNotify(new ENotificationImpl(this, Notification.SET, EncPackage.ENCRYPTEDPERSISTENCE__ALG, oldAlg, alg));
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
			eNotify(new ENotificationImpl(this, Notification.SET, EncPackage.ENCRYPTEDPERSISTENCE__KEYLENGTH, oldKeylength, keylength));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case EncPackage.ENCRYPTEDPERSISTENCE__BASE_CLASS:
				if (resolve) return getBase_Class();
				return basicGetBase_Class();
			case EncPackage.ENCRYPTEDPERSISTENCE__ALG:
				return getAlg();
			case EncPackage.ENCRYPTEDPERSISTENCE__KEYLENGTH:
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
			case EncPackage.ENCRYPTEDPERSISTENCE__BASE_CLASS:
				setBase_Class((org.eclipse.uml2.uml.Class)newValue);
				return;
			case EncPackage.ENCRYPTEDPERSISTENCE__ALG:
				setAlg((String)newValue);
				return;
			case EncPackage.ENCRYPTEDPERSISTENCE__KEYLENGTH:
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
			case EncPackage.ENCRYPTEDPERSISTENCE__BASE_CLASS:
				setBase_Class((org.eclipse.uml2.uml.Class)null);
				return;
			case EncPackage.ENCRYPTEDPERSISTENCE__ALG:
				setAlg(ALG_EDEFAULT);
				return;
			case EncPackage.ENCRYPTEDPERSISTENCE__KEYLENGTH:
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
			case EncPackage.ENCRYPTEDPERSISTENCE__BASE_CLASS:
				return base_Class != null;
			case EncPackage.ENCRYPTEDPERSISTENCE__ALG:
				return ALG_EDEFAULT == null ? alg != null : !ALG_EDEFAULT.equals(alg);
			case EncPackage.ENCRYPTEDPERSISTENCE__KEYLENGTH:
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

} //encryptedpersistenceImpl
