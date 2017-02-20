/**
 */
package carisma.profile.umlsec.impl;

import carisma.profile.umlsec.UmlsecPackage;
import carisma.profile.umlsec.rbac;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EDataTypeEList;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

import org.eclipse.uml2.uml.Action;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>rbac</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.impl.rbacImpl#getProtectedactions <em>Protectedactions</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.rbacImpl#getRole <em>Role</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.rbacImpl#getRight <em>Right</em>}</li>
 *   <li>{@link carisma.profile.umlsec.impl.rbacImpl#getBase_Package <em>Base Package</em>}</li>
 * </ul>
 *
 * @generated
 */
public class rbacImpl extends MinimalEObjectImpl.Container implements rbac {
	/**
	 * The cached value of the '{@link #getProtectedactions() <em>Protectedactions</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProtectedactions()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> protectedactions;

	/**
	 * The cached value of the '{@link #getRole() <em>Role</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRole()
	 * @generated
	 * @ordered
	 */
	protected EList<String> role;

	/**
	 * The cached value of the '{@link #getRight() <em>Right</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRight()
	 * @generated
	 * @ordered
	 */
	protected EList<String> right;

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
	protected rbacImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return UmlsecPackage.Literals.RBAC;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Action> getProtectedactions() {
		if (protectedactions == null) {
			protectedactions = new EObjectResolvingEList<Action>(Action.class, this, UmlsecPackage.RBAC__PROTECTEDACTIONS);
		}
		return protectedactions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action getProtectedactions(String name) {
		return getProtectedactions(name, false, null);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action getProtectedactions(String name, boolean ignoreCase, EClass eClass) {
		protectedactionsLoop: for (Action protectedactions : getProtectedactions()) {
			if (eClass != null && !eClass.isInstance(protectedactions))
				continue protectedactionsLoop;
			if (name != null && !(ignoreCase ? name.equalsIgnoreCase(protectedactions.getName()) : name.equals(protectedactions.getName())))
				continue protectedactionsLoop;
			return protectedactions;
		}
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<String> getRole() {
		if (role == null) {
			role = new EDataTypeEList<String>(String.class, this, UmlsecPackage.RBAC__ROLE);
		}
		return role;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<String> getRight() {
		if (right == null) {
			right = new EDataTypeEList<String>(String.class, this, UmlsecPackage.RBAC__RIGHT);
		}
		return right;
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, UmlsecPackage.RBAC__BASE_PACKAGE, oldBase_Package, base_Package));
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
			eNotify(new ENotificationImpl(this, Notification.SET, UmlsecPackage.RBAC__BASE_PACKAGE, oldBase_Package, base_Package));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case UmlsecPackage.RBAC__PROTECTEDACTIONS:
				return getProtectedactions();
			case UmlsecPackage.RBAC__ROLE:
				return getRole();
			case UmlsecPackage.RBAC__RIGHT:
				return getRight();
			case UmlsecPackage.RBAC__BASE_PACKAGE:
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
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case UmlsecPackage.RBAC__PROTECTEDACTIONS:
				getProtectedactions().clear();
				getProtectedactions().addAll((Collection<? extends Action>)newValue);
				return;
			case UmlsecPackage.RBAC__ROLE:
				getRole().clear();
				getRole().addAll((Collection<? extends String>)newValue);
				return;
			case UmlsecPackage.RBAC__RIGHT:
				getRight().clear();
				getRight().addAll((Collection<? extends String>)newValue);
				return;
			case UmlsecPackage.RBAC__BASE_PACKAGE:
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
			case UmlsecPackage.RBAC__PROTECTEDACTIONS:
				getProtectedactions().clear();
				return;
			case UmlsecPackage.RBAC__ROLE:
				getRole().clear();
				return;
			case UmlsecPackage.RBAC__RIGHT:
				getRight().clear();
				return;
			case UmlsecPackage.RBAC__BASE_PACKAGE:
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
			case UmlsecPackage.RBAC__PROTECTEDACTIONS:
				return protectedactions != null && !protectedactions.isEmpty();
			case UmlsecPackage.RBAC__ROLE:
				return role != null && !role.isEmpty();
			case UmlsecPackage.RBAC__RIGHT:
				return right != null && !right.isEmpty();
			case UmlsecPackage.RBAC__BASE_PACKAGE:
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
		result.append(" (role: ");
		result.append(role);
		result.append(", right: ");
		result.append(right);
		result.append(')');
		return result.toString();
	}

} //rbacImpl
