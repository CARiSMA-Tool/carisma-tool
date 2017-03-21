/**
 */
package UMLsecenc.impl;

import UMLsecenc.UMLsecencPackage;
import UMLsecenc.securelinksenc;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>securelinksenc</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link UMLsecenc.impl.securelinksencImpl#getAdversary <em>Adversary</em>}</li>
 *   <li>{@link UMLsecenc.impl.securelinksencImpl#getEncAdversary <em>Enc Adversary</em>}</li>
 *   <li>{@link UMLsecenc.impl.securelinksencImpl#getEncModel <em>Enc Model</em>}</li>
 *   <li>{@link UMLsecenc.impl.securelinksencImpl#getBase_Package <em>Base Package</em>}</li>
 * </ul>
 *
 * @generated
 */
public class securelinksencImpl extends MinimalEObjectImpl.Container implements securelinksenc {
	/**
	 * The default value of the '{@link #getAdversary() <em>Adversary</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAdversary()
	 * @generated
	 * @ordered
	 */
	protected static final String ADVERSARY_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getAdversary() <em>Adversary</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAdversary()
	 * @generated
	 * @ordered
	 */
	protected String adversary = ADVERSARY_EDEFAULT;

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
	 * The cached value of the '{@link #getBase_Package() <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Package()
	 * @generated
	 * @ordered
	 */
	protected org.eclipse.uml2.uml.Package base_Package;

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
		return UMLsecencPackage.Literals.SECURELINKSENC;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getAdversary() {
		return adversary;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAdversary(String newAdversary) {
		String oldAdversary = adversary;
		adversary = newAdversary;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UMLsecencPackage.SECURELINKSENC__ADVERSARY, oldAdversary, adversary));
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
			eNotify(new ENotificationImpl(this, Notification.SET, UMLsecencPackage.SECURELINKSENC__ENC_ADVERSARY, oldEncAdversary, encAdversary));
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
			eNotify(new ENotificationImpl(this, Notification.SET, UMLsecencPackage.SECURELINKSENC__ENC_MODEL, oldEncModel, encModel));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.uml2.uml.Package getBase_Package() {
		if (base_Package != null && base_Package.eIsProxy()) {
			InternalEObject oldBase_Package = (InternalEObject)base_Package;
			base_Package = (org.eclipse.uml2.uml.Package)eResolveProxy(oldBase_Package);
			if (base_Package != oldBase_Package) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UMLsecencPackage.SECURELINKSENC__BASE_PACKAGE, oldBase_Package, base_Package));
			}
		}
		return base_Package;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.uml2.uml.Package basicGetBase_Package() {
		return base_Package;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBase_Package(org.eclipse.uml2.uml.Package newBase_Package) {
		org.eclipse.uml2.uml.Package oldBase_Package = base_Package;
		base_Package = newBase_Package;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UMLsecencPackage.SECURELINKSENC__BASE_PACKAGE, oldBase_Package, base_Package));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UMLsecencPackage.SECURELINKSENC__ADVERSARY:
				return getAdversary();
			case UMLsecencPackage.SECURELINKSENC__ENC_ADVERSARY:
				return getEncAdversary();
			case UMLsecencPackage.SECURELINKSENC__ENC_MODEL:
				return getEncModel();
			case UMLsecencPackage.SECURELINKSENC__BASE_PACKAGE:
				if (resolve) return getBase_Package();
				return basicGetBase_Package();
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
			case UMLsecencPackage.SECURELINKSENC__ADVERSARY:
				setAdversary((String)newValue);
				return;
			case UMLsecencPackage.SECURELINKSENC__ENC_ADVERSARY:
				setEncAdversary((String)newValue);
				return;
			case UMLsecencPackage.SECURELINKSENC__ENC_MODEL:
				setEncModel((String)newValue);
				return;
			case UMLsecencPackage.SECURELINKSENC__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)newValue);
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
			case UMLsecencPackage.SECURELINKSENC__ADVERSARY:
				setAdversary(ADVERSARY_EDEFAULT);
				return;
			case UMLsecencPackage.SECURELINKSENC__ENC_ADVERSARY:
				setEncAdversary(ENC_ADVERSARY_EDEFAULT);
				return;
			case UMLsecencPackage.SECURELINKSENC__ENC_MODEL:
				setEncModel(ENC_MODEL_EDEFAULT);
				return;
			case UMLsecencPackage.SECURELINKSENC__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)null);
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
			case UMLsecencPackage.SECURELINKSENC__ADVERSARY:
				return ADVERSARY_EDEFAULT == null ? adversary != null : !ADVERSARY_EDEFAULT.equals(adversary);
			case UMLsecencPackage.SECURELINKSENC__ENC_ADVERSARY:
				return ENC_ADVERSARY_EDEFAULT == null ? encAdversary != null : !ENC_ADVERSARY_EDEFAULT.equals(encAdversary);
			case UMLsecencPackage.SECURELINKSENC__ENC_MODEL:
				return ENC_MODEL_EDEFAULT == null ? encModel != null : !ENC_MODEL_EDEFAULT.equals(encModel);
			case UMLsecencPackage.SECURELINKSENC__BASE_PACKAGE:
				return base_Package != null;
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
		result.append(" (adversary: ");
		result.append(adversary);
		result.append(", encAdversary: ");
		result.append(encAdversary);
		result.append(", encModel: ");
		result.append(encModel);
		result.append(')');
		return result.toString();
	}

} //securelinksencImpl
