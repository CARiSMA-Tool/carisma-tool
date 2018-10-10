/**
 */
package carisma.profile.umlsec.impl;

import carisma.profile.umlsec.UmlsecPackage;
import carisma.profile.umlsec.datasecurity;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>datasecurity</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.impl.datasecurityImpl#getAdversary <em>Adversary</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.datasecurityImpl#getBase_Package <em>Base Package</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.datasecurityImpl#getAuthenticity <em>Authenticity</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.datasecurityImpl#getIntegrity <em>Integrity</em>}</li>
 * </ul>
 *
 * @generated
 */
public class datasecurityImpl extends MinimalEObjectImpl.Container implements datasecurity {
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
	 * The cached value of the '{@link #getBase_Package() <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Package()
	 * @generated
	 * @ordered
	 */
	protected org.eclipse.uml2.uml.Package base_Package;

	/**
	 * The default value of the '{@link #getAuthenticity() <em>Authenticity</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAuthenticity()
	 * @generated
	 * @ordered
	 */
	protected static final String AUTHENTICITY_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getAuthenticity() <em>Authenticity</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAuthenticity()
	 * @generated
	 * @ordered
	 */
	protected String authenticity = AUTHENTICITY_EDEFAULT;

	/**
	 * The default value of the '{@link #getIntegrity() <em>Integrity</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIntegrity()
	 * @generated
	 * @ordered
	 */
	protected static final String INTEGRITY_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getIntegrity() <em>Integrity</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getIntegrity()
	 * @generated
	 * @ordered
	 */
	protected String integrity = INTEGRITY_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected datasecurityImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmlsecPackage.Literals.DATASECURITY;
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
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.DATASECURITY__ADVERSARY, oldAdversary, adversary));
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UmlsecPackage.DATASECURITY__BASE_PACKAGE, oldBase_Package, base_Package));
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
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.DATASECURITY__BASE_PACKAGE, oldBase_Package, base_Package));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getAuthenticity() {
		return authenticity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAuthenticity(String newAuthenticity) {
		String oldAuthenticity = authenticity;
		authenticity = newAuthenticity;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.DATASECURITY__AUTHENTICITY, oldAuthenticity, authenticity));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getIntegrity() {
		return integrity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setIntegrity(String newIntegrity) {
		String oldIntegrity = integrity;
		integrity = newIntegrity;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.DATASECURITY__INTEGRITY, oldIntegrity, integrity));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmlsecPackage.DATASECURITY__ADVERSARY:
				return getAdversary();
			case UmlsecPackage.DATASECURITY__BASE_PACKAGE:
				if (resolve) return getBase_Package();
				return basicGetBase_Package();
			case UmlsecPackage.DATASECURITY__AUTHENTICITY:
				return getAuthenticity();
			case UmlsecPackage.DATASECURITY__INTEGRITY:
				return getIntegrity();
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
			case UmlsecPackage.DATASECURITY__ADVERSARY:
				setAdversary((String)newValue);
				return;
			case UmlsecPackage.DATASECURITY__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)newValue);
				return;
			case UmlsecPackage.DATASECURITY__AUTHENTICITY:
				setAuthenticity((String)newValue);
				return;
			case UmlsecPackage.DATASECURITY__INTEGRITY:
				setIntegrity((String)newValue);
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
			case UmlsecPackage.DATASECURITY__ADVERSARY:
				setAdversary(ADVERSARY_EDEFAULT);
				return;
			case UmlsecPackage.DATASECURITY__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)null);
				return;
			case UmlsecPackage.DATASECURITY__AUTHENTICITY:
				setAuthenticity(AUTHENTICITY_EDEFAULT);
				return;
			case UmlsecPackage.DATASECURITY__INTEGRITY:
				setIntegrity(INTEGRITY_EDEFAULT);
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
			case UmlsecPackage.DATASECURITY__ADVERSARY:
				return ADVERSARY_EDEFAULT == null ? adversary != null : !ADVERSARY_EDEFAULT.equals(adversary);
			case UmlsecPackage.DATASECURITY__BASE_PACKAGE:
				return base_Package != null;
			case UmlsecPackage.DATASECURITY__AUTHENTICITY:
				return AUTHENTICITY_EDEFAULT == null ? authenticity != null : !AUTHENTICITY_EDEFAULT.equals(authenticity);
			case UmlsecPackage.DATASECURITY__INTEGRITY:
				return INTEGRITY_EDEFAULT == null ? integrity != null : !INTEGRITY_EDEFAULT.equals(integrity);
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
		result.append(", authenticity: ");
		result.append(authenticity);
		result.append(", integrity: ");
		result.append(integrity);
		result.append(')');
		return result.toString();
	}

} //datasecurityImpl
