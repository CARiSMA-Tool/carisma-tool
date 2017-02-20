/**
 */
package carisma.profile.umlsec.rabac.impl;

import carisma.profile.umlsec.rabac.RabacPackage;
import carisma.profile.umlsec.rabac.abacRequire;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Transition;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>abac Require</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.rabac.impl.abacRequireImpl#getRight <em>Right</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.impl.abacRequireImpl#getFilters <em>Filters</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.impl.abacRequireImpl#getBase_Transition <em>Base Transition</em>}</li>
 *   <li>{@link carisma.profile.umlsec.rabac.impl.abacRequireImpl#getBase_Operation <em>Base Operation</em>}</li>
 * </ul>
 *
 * @generated
 */
public class abacRequireImpl extends MinimalEObjectImpl.Container implements abacRequire {
	/**
	 * The default value of the '{@link #getRight() <em>Right</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRight()
	 * @generated
	 * @ordered
	 */
	protected static final String RIGHT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getRight() <em>Right</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRight()
	 * @generated
	 * @ordered
	 */
	protected String right = RIGHT_EDEFAULT;

	/**
	 * The default value of the '{@link #getFilters() <em>Filters</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFilters()
	 * @generated
	 * @ordered
	 */
	protected static final String FILTERS_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getFilters() <em>Filters</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFilters()
	 * @generated
	 * @ordered
	 */
	protected String filters = FILTERS_EDEFAULT;

	/**
	 * The cached value of the '{@link #getBase_Transition() <em>Base Transition</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Transition()
	 * @generated
	 * @ordered
	 */
	protected Transition base_Transition;

	/**
	 * The cached value of the '{@link #getBase_Operation() <em>Base Operation</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Operation()
	 * @generated
	 * @ordered
	 */
	protected Operation base_Operation;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected abacRequireImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return RabacPackage.Literals.ABAC_REQUIRE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getRight() {
		return right;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setRight(String newRight) {
		String oldRight = right;
		right = newRight;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RabacPackage.ABAC_REQUIRE__RIGHT, oldRight, right));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getFilters() {
		return filters;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setFilters(String newFilters) {
		String oldFilters = filters;
		filters = newFilters;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RabacPackage.ABAC_REQUIRE__FILTERS, oldFilters, filters));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Transition getBase_Transition() {
		if (base_Transition != null && base_Transition.eIsProxy()) {
			InternalEObject oldBase_Transition = (InternalEObject)base_Transition;
			base_Transition = (Transition)eResolveProxy(oldBase_Transition);
			if (base_Transition != oldBase_Transition) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, RabacPackage.ABAC_REQUIRE__BASE_TRANSITION, oldBase_Transition, base_Transition));
			}
		}
		return base_Transition;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Transition basicGetBase_Transition() {
		return base_Transition;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBase_Transition(Transition newBase_Transition) {
		Transition oldBase_Transition = base_Transition;
		base_Transition = newBase_Transition;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RabacPackage.ABAC_REQUIRE__BASE_TRANSITION, oldBase_Transition, base_Transition));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Operation getBase_Operation() {
		if (base_Operation != null && base_Operation.eIsProxy()) {
			InternalEObject oldBase_Operation = (InternalEObject)base_Operation;
			base_Operation = (Operation)eResolveProxy(oldBase_Operation);
			if (base_Operation != oldBase_Operation) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, RabacPackage.ABAC_REQUIRE__BASE_OPERATION, oldBase_Operation, base_Operation));
			}
		}
		return base_Operation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Operation basicGetBase_Operation() {
		return base_Operation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setBase_Operation(Operation newBase_Operation) {
		Operation oldBase_Operation = base_Operation;
		base_Operation = newBase_Operation;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, RabacPackage.ABAC_REQUIRE__BASE_OPERATION, oldBase_Operation, base_Operation));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case RabacPackage.ABAC_REQUIRE__RIGHT:
				return getRight();
			case RabacPackage.ABAC_REQUIRE__FILTERS:
				return getFilters();
			case RabacPackage.ABAC_REQUIRE__BASE_TRANSITION:
				if (resolve) return getBase_Transition();
				return basicGetBase_Transition();
			case RabacPackage.ABAC_REQUIRE__BASE_OPERATION:
				if (resolve) return getBase_Operation();
				return basicGetBase_Operation();
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
			case RabacPackage.ABAC_REQUIRE__RIGHT:
				setRight((String)newValue);
				return;
			case RabacPackage.ABAC_REQUIRE__FILTERS:
				setFilters((String)newValue);
				return;
			case RabacPackage.ABAC_REQUIRE__BASE_TRANSITION:
				setBase_Transition((Transition)newValue);
				return;
			case RabacPackage.ABAC_REQUIRE__BASE_OPERATION:
				setBase_Operation((Operation)newValue);
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
			case RabacPackage.ABAC_REQUIRE__RIGHT:
				setRight(RIGHT_EDEFAULT);
				return;
			case RabacPackage.ABAC_REQUIRE__FILTERS:
				setFilters(FILTERS_EDEFAULT);
				return;
			case RabacPackage.ABAC_REQUIRE__BASE_TRANSITION:
				setBase_Transition((Transition)null);
				return;
			case RabacPackage.ABAC_REQUIRE__BASE_OPERATION:
				setBase_Operation((Operation)null);
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
			case RabacPackage.ABAC_REQUIRE__RIGHT:
				return RIGHT_EDEFAULT == null ? right != null : !RIGHT_EDEFAULT.equals(right);
			case RabacPackage.ABAC_REQUIRE__FILTERS:
				return FILTERS_EDEFAULT == null ? filters != null : !FILTERS_EDEFAULT.equals(filters);
			case RabacPackage.ABAC_REQUIRE__BASE_TRANSITION:
				return base_Transition != null;
			case RabacPackage.ABAC_REQUIRE__BASE_OPERATION:
				return base_Operation != null;
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
		result.append(" (right: ");
		result.append(right);
		result.append(", filters: ");
		result.append(filters);
		result.append(')');
		return result.toString();
	}

} //abacRequireImpl
