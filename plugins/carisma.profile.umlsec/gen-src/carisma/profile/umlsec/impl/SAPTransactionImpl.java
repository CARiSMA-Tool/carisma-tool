/**
 */
package carisma.profile.umlsec.impl;

import carisma.profile.umlsec.SAPTransaction;
import carisma.profile.umlsec.UmlsecPackage;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Action;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>SAP Transaction</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.impl.SAPTransactionImpl#getId <em>Id</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.SAPTransactionImpl#getBase_Action <em>Base Action</em>}</li>
 * </ul>
 *
 * @generated
 */
public class SAPTransactionImpl extends MinimalEObjectImpl.Container implements SAPTransaction {
	/**
	 * The default value of the '{@link #getId() <em>Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getId()
	 * @generated
	 * @ordered
	 */
	protected static final String ID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getId() <em>Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getId()
	 * @generated
	 * @ordered
	 */
	protected String id = ID_EDEFAULT;

	/**
	 * The cached value of the '{@link #getBase_Action() <em>Base Action</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Action()
	 * @generated
	 * @ordered
	 */
	protected Action base_Action;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SAPTransactionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmlsecPackage.Literals.SAP_TRANSACTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getId() {
		return id;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setId(String newId) {
		String oldId = id;
		id = newId;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.SAP_TRANSACTION__ID, oldId, id));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action getBase_Action() {
		if (base_Action != null && base_Action.eIsProxy()) {
			InternalEObject oldBase_Action = (InternalEObject)base_Action;
			base_Action = (Action)eResolveProxy(oldBase_Action);
			if (base_Action != oldBase_Action) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UmlsecPackage.SAP_TRANSACTION__BASE_ACTION, oldBase_Action, base_Action));
			}
		}
		return base_Action;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action basicGetBase_Action() {
		return base_Action;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBase_Action(Action newBase_Action) {
		Action oldBase_Action = base_Action;
		base_Action = newBase_Action;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.SAP_TRANSACTION__BASE_ACTION, oldBase_Action, base_Action));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmlsecPackage.SAP_TRANSACTION__ID:
				return getId();
			case UmlsecPackage.SAP_TRANSACTION__BASE_ACTION:
				if (resolve) return getBase_Action();
				return basicGetBase_Action();
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
			case UmlsecPackage.SAP_TRANSACTION__ID:
				setId((String)newValue);
				return;
			case UmlsecPackage.SAP_TRANSACTION__BASE_ACTION:
				setBase_Action((Action)newValue);
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
			case UmlsecPackage.SAP_TRANSACTION__ID:
				setId(ID_EDEFAULT);
				return;
			case UmlsecPackage.SAP_TRANSACTION__BASE_ACTION:
				setBase_Action((Action)null);
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
			case UmlsecPackage.SAP_TRANSACTION__ID:
				return ID_EDEFAULT == null ? id != null : !ID_EDEFAULT.equals(id);
			case UmlsecPackage.SAP_TRANSACTION__BASE_ACTION:
				return base_Action != null;
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
		result.append(" (id: ");
		result.append(id);
		result.append(')');
		return result.toString();
	}

} //SAPTransactionImpl
