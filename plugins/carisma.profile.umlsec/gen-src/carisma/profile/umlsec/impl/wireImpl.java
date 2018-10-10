/**
 */
package carisma.profile.umlsec.impl;

import carisma.profile.umlsec.UmlsecPackage;
import carisma.profile.umlsec.wire;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.CommunicationPath;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>wire</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.impl.wireImpl#getBase_CommunicationPath <em>Base Communication Path</em>}</li>
 * </ul>
 *
 * @generated
 */
public class wireImpl extends MinimalEObjectImpl.Container implements wire {
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
	protected wireImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmlsecPackage.Literals.WIRE;
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UmlsecPackage.WIRE__BASE_COMMUNICATION_PATH, oldBase_CommunicationPath, base_CommunicationPath));
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
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.WIRE__BASE_COMMUNICATION_PATH, oldBase_CommunicationPath, base_CommunicationPath));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmlsecPackage.WIRE__BASE_COMMUNICATION_PATH:
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
			case UmlsecPackage.WIRE__BASE_COMMUNICATION_PATH:
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
			case UmlsecPackage.WIRE__BASE_COMMUNICATION_PATH:
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
			case UmlsecPackage.WIRE__BASE_COMMUNICATION_PATH:
				return base_CommunicationPath != null;
		}
		return super.eIsSet(featureID);
	}

} //wireImpl
