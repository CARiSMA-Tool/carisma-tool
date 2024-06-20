/**
 */
package carisma.profile.umlsec.extension4ids.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Interaction;
import org.eclipse.uml2.uml.Message;

import carisma.profile.umlsec.extension4ids.Extension4idsPackage;
import carisma.profile.umlsec.extension4ids.datatransfer;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>datatransfer</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.extension4ids.impl.datatransferImpl#getType <em>Type</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.impl.datatransferImpl#getTransfer_req_step <em>Transfer req step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.impl.datatransferImpl#getTransfer_start_step <em>Transfer start step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.impl.datatransferImpl#getPush_pull_step <em>Push pull step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.impl.datatransferImpl#getTransfer_complete_step <em>Transfer complete step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.impl.datatransferImpl#getTransfer_suspend_step <em>Transfer suspend step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.impl.datatransferImpl#getTransfer_terminate_step <em>Transfer terminate step</em>}</li>
 *   <li>{@link carisma.profile.umlsec.extension4ids.impl.datatransferImpl#getBase_Interaction <em>Base Interaction</em>}</li>
 * </ul>
 *
 * @generated
 */
public class datatransferImpl extends MinimalEObjectImpl.Container implements datatransfer {
	/**
	 * The default value of the '{@link #getType() <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected static final String TYPE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getType() <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected String type = TYPE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getTransfer_req_step() <em>Transfer req step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTransfer_req_step()
	 * @generated
	 * @ordered
	 */
	protected Message transfer_req_step;

	/**
	 * The cached value of the '{@link #getTransfer_start_step() <em>Transfer start step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTransfer_start_step()
	 * @generated
	 * @ordered
	 */
	protected Message transfer_start_step;

	/**
	 * The cached value of the '{@link #getPush_pull_step() <em>Push pull step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPush_pull_step()
	 * @generated
	 * @ordered
	 */
	protected Message push_pull_step;

	/**
	 * The cached value of the '{@link #getTransfer_complete_step() <em>Transfer complete step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTransfer_complete_step()
	 * @generated
	 * @ordered
	 */
	protected Message transfer_complete_step;

	/**
	 * The cached value of the '{@link #getTransfer_suspend_step() <em>Transfer suspend step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTransfer_suspend_step()
	 * @generated
	 * @ordered
	 */
	protected Message transfer_suspend_step;

	/**
	 * The cached value of the '{@link #getTransfer_terminate_step() <em>Transfer terminate step</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTransfer_terminate_step()
	 * @generated
	 * @ordered
	 */
	protected Message transfer_terminate_step;

	/**
	 * The cached value of the '{@link #getBase_Interaction() <em>Base Interaction</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Interaction()
	 * @generated
	 * @ordered
	 */
	protected Interaction base_Interaction;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected datatransferImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Extension4idsPackage.Literals.DATATRANSFER;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getType() {
		return type;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setType(String newType) {
		String oldType = type;
		type = newType;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Extension4idsPackage.DATATRANSFER__TYPE, oldType, type));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Message getTransfer_req_step() {
		if (transfer_req_step != null && transfer_req_step.eIsProxy()) {
			InternalEObject oldTransfer_req_step = (InternalEObject)transfer_req_step;
			transfer_req_step = (Message)eResolveProxy(oldTransfer_req_step);
			if (transfer_req_step != oldTransfer_req_step) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Extension4idsPackage.DATATRANSFER__TRANSFER_REQ_STEP, oldTransfer_req_step, transfer_req_step));
			}
		}
		return transfer_req_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Message basicGetTransfer_req_step() {
		return transfer_req_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setTransfer_req_step(Message newTransfer_req_step) {
		Message oldTransfer_req_step = transfer_req_step;
		transfer_req_step = newTransfer_req_step;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Extension4idsPackage.DATATRANSFER__TRANSFER_REQ_STEP, oldTransfer_req_step, transfer_req_step));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Message getTransfer_start_step() {
		if (transfer_start_step != null && transfer_start_step.eIsProxy()) {
			InternalEObject oldTransfer_start_step = (InternalEObject)transfer_start_step;
			transfer_start_step = (Message)eResolveProxy(oldTransfer_start_step);
			if (transfer_start_step != oldTransfer_start_step) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Extension4idsPackage.DATATRANSFER__TRANSFER_START_STEP, oldTransfer_start_step, transfer_start_step));
			}
		}
		return transfer_start_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Message basicGetTransfer_start_step() {
		return transfer_start_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setTransfer_start_step(Message newTransfer_start_step) {
		Message oldTransfer_start_step = transfer_start_step;
		transfer_start_step = newTransfer_start_step;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Extension4idsPackage.DATATRANSFER__TRANSFER_START_STEP, oldTransfer_start_step, transfer_start_step));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Message getPush_pull_step() {
		if (push_pull_step != null && push_pull_step.eIsProxy()) {
			InternalEObject oldPush_pull_step = (InternalEObject)push_pull_step;
			push_pull_step = (Message)eResolveProxy(oldPush_pull_step);
			if (push_pull_step != oldPush_pull_step) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Extension4idsPackage.DATATRANSFER__PUSH_PULL_STEP, oldPush_pull_step, push_pull_step));
			}
		}
		return push_pull_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Message basicGetPush_pull_step() {
		return push_pull_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setPush_pull_step(Message newPush_pull_step) {
		Message oldPush_pull_step = push_pull_step;
		push_pull_step = newPush_pull_step;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Extension4idsPackage.DATATRANSFER__PUSH_PULL_STEP, oldPush_pull_step, push_pull_step));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Message getTransfer_complete_step() {
		if (transfer_complete_step != null && transfer_complete_step.eIsProxy()) {
			InternalEObject oldTransfer_complete_step = (InternalEObject)transfer_complete_step;
			transfer_complete_step = (Message)eResolveProxy(oldTransfer_complete_step);
			if (transfer_complete_step != oldTransfer_complete_step) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Extension4idsPackage.DATATRANSFER__TRANSFER_COMPLETE_STEP, oldTransfer_complete_step, transfer_complete_step));
			}
		}
		return transfer_complete_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Message basicGetTransfer_complete_step() {
		return transfer_complete_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setTransfer_complete_step(Message newTransfer_complete_step) {
		Message oldTransfer_complete_step = transfer_complete_step;
		transfer_complete_step = newTransfer_complete_step;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Extension4idsPackage.DATATRANSFER__TRANSFER_COMPLETE_STEP, oldTransfer_complete_step, transfer_complete_step));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Message getTransfer_suspend_step() {
		if (transfer_suspend_step != null && transfer_suspend_step.eIsProxy()) {
			InternalEObject oldTransfer_suspend_step = (InternalEObject)transfer_suspend_step;
			transfer_suspend_step = (Message)eResolveProxy(oldTransfer_suspend_step);
			if (transfer_suspend_step != oldTransfer_suspend_step) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Extension4idsPackage.DATATRANSFER__TRANSFER_SUSPEND_STEP, oldTransfer_suspend_step, transfer_suspend_step));
			}
		}
		return transfer_suspend_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Message basicGetTransfer_suspend_step() {
		return transfer_suspend_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setTransfer_suspend_step(Message newTransfer_suspend_step) {
		Message oldTransfer_suspend_step = transfer_suspend_step;
		transfer_suspend_step = newTransfer_suspend_step;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Extension4idsPackage.DATATRANSFER__TRANSFER_SUSPEND_STEP, oldTransfer_suspend_step, transfer_suspend_step));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Message getTransfer_terminate_step() {
		if (transfer_terminate_step != null && transfer_terminate_step.eIsProxy()) {
			InternalEObject oldTransfer_terminate_step = (InternalEObject)transfer_terminate_step;
			transfer_terminate_step = (Message)eResolveProxy(oldTransfer_terminate_step);
			if (transfer_terminate_step != oldTransfer_terminate_step) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Extension4idsPackage.DATATRANSFER__TRANSFER_TERMINATE_STEP, oldTransfer_terminate_step, transfer_terminate_step));
			}
		}
		return transfer_terminate_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Message basicGetTransfer_terminate_step() {
		return transfer_terminate_step;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setTransfer_terminate_step(Message newTransfer_terminate_step) {
		Message oldTransfer_terminate_step = transfer_terminate_step;
		transfer_terminate_step = newTransfer_terminate_step;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Extension4idsPackage.DATATRANSFER__TRANSFER_TERMINATE_STEP, oldTransfer_terminate_step, transfer_terminate_step));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Interaction getBase_Interaction() {
		if (base_Interaction != null && base_Interaction.eIsProxy()) {
			InternalEObject oldBase_Interaction = (InternalEObject)base_Interaction;
			base_Interaction = (Interaction)eResolveProxy(oldBase_Interaction);
			if (base_Interaction != oldBase_Interaction) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Extension4idsPackage.DATATRANSFER__BASE_INTERACTION, oldBase_Interaction, base_Interaction));
			}
		}
		return base_Interaction;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Interaction basicGetBase_Interaction() {
		return base_Interaction;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_Interaction(Interaction newBase_Interaction) {
		Interaction oldBase_Interaction = base_Interaction;
		base_Interaction = newBase_Interaction;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Extension4idsPackage.DATATRANSFER__BASE_INTERACTION, oldBase_Interaction, base_Interaction));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Extension4idsPackage.DATATRANSFER__TYPE:
				return getType();
			case Extension4idsPackage.DATATRANSFER__TRANSFER_REQ_STEP:
				if (resolve) return getTransfer_req_step();
				return basicGetTransfer_req_step();
			case Extension4idsPackage.DATATRANSFER__TRANSFER_START_STEP:
				if (resolve) return getTransfer_start_step();
				return basicGetTransfer_start_step();
			case Extension4idsPackage.DATATRANSFER__PUSH_PULL_STEP:
				if (resolve) return getPush_pull_step();
				return basicGetPush_pull_step();
			case Extension4idsPackage.DATATRANSFER__TRANSFER_COMPLETE_STEP:
				if (resolve) return getTransfer_complete_step();
				return basicGetTransfer_complete_step();
			case Extension4idsPackage.DATATRANSFER__TRANSFER_SUSPEND_STEP:
				if (resolve) return getTransfer_suspend_step();
				return basicGetTransfer_suspend_step();
			case Extension4idsPackage.DATATRANSFER__TRANSFER_TERMINATE_STEP:
				if (resolve) return getTransfer_terminate_step();
				return basicGetTransfer_terminate_step();
			case Extension4idsPackage.DATATRANSFER__BASE_INTERACTION:
				if (resolve) return getBase_Interaction();
				return basicGetBase_Interaction();
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
			case Extension4idsPackage.DATATRANSFER__TYPE:
				setType((String)newValue);
				return;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_REQ_STEP:
				setTransfer_req_step((Message)newValue);
				return;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_START_STEP:
				setTransfer_start_step((Message)newValue);
				return;
			case Extension4idsPackage.DATATRANSFER__PUSH_PULL_STEP:
				setPush_pull_step((Message)newValue);
				return;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_COMPLETE_STEP:
				setTransfer_complete_step((Message)newValue);
				return;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_SUSPEND_STEP:
				setTransfer_suspend_step((Message)newValue);
				return;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_TERMINATE_STEP:
				setTransfer_terminate_step((Message)newValue);
				return;
			case Extension4idsPackage.DATATRANSFER__BASE_INTERACTION:
				setBase_Interaction((Interaction)newValue);
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
			case Extension4idsPackage.DATATRANSFER__TYPE:
				setType(TYPE_EDEFAULT);
				return;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_REQ_STEP:
				setTransfer_req_step((Message)null);
				return;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_START_STEP:
				setTransfer_start_step((Message)null);
				return;
			case Extension4idsPackage.DATATRANSFER__PUSH_PULL_STEP:
				setPush_pull_step((Message)null);
				return;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_COMPLETE_STEP:
				setTransfer_complete_step((Message)null);
				return;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_SUSPEND_STEP:
				setTransfer_suspend_step((Message)null);
				return;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_TERMINATE_STEP:
				setTransfer_terminate_step((Message)null);
				return;
			case Extension4idsPackage.DATATRANSFER__BASE_INTERACTION:
				setBase_Interaction((Interaction)null);
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
			case Extension4idsPackage.DATATRANSFER__TYPE:
				return TYPE_EDEFAULT == null ? type != null : !TYPE_EDEFAULT.equals(type);
			case Extension4idsPackage.DATATRANSFER__TRANSFER_REQ_STEP:
				return transfer_req_step != null;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_START_STEP:
				return transfer_start_step != null;
			case Extension4idsPackage.DATATRANSFER__PUSH_PULL_STEP:
				return push_pull_step != null;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_COMPLETE_STEP:
				return transfer_complete_step != null;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_SUSPEND_STEP:
				return transfer_suspend_step != null;
			case Extension4idsPackage.DATATRANSFER__TRANSFER_TERMINATE_STEP:
				return transfer_terminate_step != null;
			case Extension4idsPackage.DATATRANSFER__BASE_INTERACTION:
				return base_Interaction != null;
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
		result.append(" (type: ");
		result.append(type);
		result.append(')');
		return result.toString();
	}

} //datatransferImpl
