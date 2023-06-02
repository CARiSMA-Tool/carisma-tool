/**
 */
package carisma.profile.umlsec.impl;

import carisma.profile.umlsec.UmlsecPackage;
import carisma.profile.umlsec.requires;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectResolvingEList;

import org.eclipse.uml2.uml.Action;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>requires</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.impl.requiresImpl#getBase_Action <em>Base Action</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.requiresImpl#getActions <em>Actions</em>}</li>
 * </ul>
 *
 * @generated
 */
public class requiresImpl extends MinimalEObjectImpl.Container implements requires {
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
	 * The cached value of the '{@link #getActions() <em>Actions</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getActions()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> actions;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected requiresImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmlsecPackage.Literals.REQUIRES;
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UmlsecPackage.REQUIRES__BASE_ACTION, oldBase_Action, base_Action));
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
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.REQUIRES__BASE_ACTION, oldBase_Action, base_Action));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Action> getActions() {
		if (actions == null) {
			actions = new EObjectResolvingEList<Action>(Action.class, this, UmlsecPackage.REQUIRES__ACTIONS);
		}
		return actions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action getActions(String name) {
		return getActions(name, false, null);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action getActions(String name, boolean ignoreCase, EClass eClass) {
		actionsLoop: for (Action actions : getActions()) {
			if (eClass != null && !eClass.isInstance(actions))
				continue actionsLoop;
			if (name != null && !(ignoreCase ? name.equalsIgnoreCase(actions.getName()) : name.equals(actions.getName())))
				continue actionsLoop;
			return actions;
		}
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmlsecPackage.REQUIRES__BASE_ACTION:
				if (resolve) return getBase_Action();
				return basicGetBase_Action();
			case UmlsecPackage.REQUIRES__ACTIONS:
				return getActions();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case UmlsecPackage.REQUIRES__BASE_ACTION:
				setBase_Action((Action)newValue);
				return;
			case UmlsecPackage.REQUIRES__ACTIONS:
				getActions().clear();
				getActions().addAll((Collection<? extends Action>)newValue);
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
			case UmlsecPackage.REQUIRES__BASE_ACTION:
				setBase_Action((Action)null);
				return;
			case UmlsecPackage.REQUIRES__ACTIONS:
				getActions().clear();
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
			case UmlsecPackage.REQUIRES__BASE_ACTION:
				return base_Action != null;
			case UmlsecPackage.REQUIRES__ACTIONS:
				return actions != null && !actions.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //requiresImpl
