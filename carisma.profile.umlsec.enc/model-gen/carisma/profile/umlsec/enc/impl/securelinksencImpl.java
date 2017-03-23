/**
 */
package carisma.profile.umlsec.enc.impl;

import carisma.profile.umlsec.enc.EncPackage;
import carisma.profile.umlsec.enc.securelinksenc;

import carisma.profile.umlsec.impl.securelinksImpl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>securelinksenc</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.enc.impl.securelinksencImpl#getEncAdversary <em>Enc Adversary</em>}</li>
 *   <li>{@link carisma.profile.umlsec.enc.impl.securelinksencImpl#getEncModel <em>Enc Model</em>}</li>
 * </ul>
 *
 * @generated
 */
public class securelinksencImpl extends securelinksImpl implements securelinksenc {
	/**
	 * The default value of the '{@link #getEncAdversary() <em>Enc Adversary</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEncAdversary()
	 * @generated
	 * @ordered
	 */
	protected static final String ENC_ADVERSARY_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getEncAdversary() <em>Enc Adversary</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEncAdversary()
	 * @generated
	 * @ordered
	 */
	protected String encAdversary = ENC_ADVERSARY_EDEFAULT;

	/**
	 * The default value of the '{@link #getEncModel() <em>Enc Model</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEncModel()
	 * @generated
	 * @ordered
	 */
	protected static final String ENC_MODEL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getEncModel() <em>Enc Model</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEncModel()
	 * @generated
	 * @ordered
	 */
	protected String encModel = ENC_MODEL_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected securelinksencImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return EncPackage.Literals.SECURELINKSENC;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getEncAdversary() {
		return encAdversary;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setEncAdversary(String newEncAdversary) {
		String oldEncAdversary = encAdversary;
		encAdversary = newEncAdversary;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, EncPackage.SECURELINKSENC__ENC_ADVERSARY, oldEncAdversary, encAdversary));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getEncModel() {
		return encModel;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setEncModel(String newEncModel) {
		String oldEncModel = encModel;
		encModel = newEncModel;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, EncPackage.SECURELINKSENC__ENC_MODEL, oldEncModel, encModel));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case EncPackage.SECURELINKSENC__ENC_ADVERSARY:
				return getEncAdversary();
			case EncPackage.SECURELINKSENC__ENC_MODEL:
				return getEncModel();
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
			case EncPackage.SECURELINKSENC__ENC_ADVERSARY:
				setEncAdversary((String)newValue);
				return;
			case EncPackage.SECURELINKSENC__ENC_MODEL:
				setEncModel((String)newValue);
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
			case EncPackage.SECURELINKSENC__ENC_ADVERSARY:
				setEncAdversary(ENC_ADVERSARY_EDEFAULT);
				return;
			case EncPackage.SECURELINKSENC__ENC_MODEL:
				setEncModel(ENC_MODEL_EDEFAULT);
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
			case EncPackage.SECURELINKSENC__ENC_ADVERSARY:
				return ENC_ADVERSARY_EDEFAULT == null ? encAdversary != null : !ENC_ADVERSARY_EDEFAULT.equals(encAdversary);
			case EncPackage.SECURELINKSENC__ENC_MODEL:
				return ENC_MODEL_EDEFAULT == null ? encModel != null : !ENC_MODEL_EDEFAULT.equals(encModel);
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
		result.append(" (encAdversary: ");
		result.append(encAdversary);
		result.append(", encModel: ");
		result.append(encModel);
		result.append(')');
		return result.toString();
	}

} //securelinksencImpl
