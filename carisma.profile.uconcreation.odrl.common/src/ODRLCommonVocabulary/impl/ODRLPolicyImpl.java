/**
 */
package ODRLCommonVocabulary.impl;

import ODRLCommonVocabulary.ConflictStrategy;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.ODRLPolicy;
import ODRLCommonVocabulary.PolicyType;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;

import org.eclipse.uml2.uml.Activity;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>ODRL Policy</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.impl.ODRLPolicyImpl#getUid <em>Uid</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ODRLPolicyImpl#getBase_Activity <em>Base Activity</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ODRLPolicyImpl#getConflictStrategy <em>Conflict Strategy</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ODRLPolicyImpl#getPolicyType <em>Policy Type</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ODRLPolicyImpl#getProfiles <em>Profiles</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ODRLPolicyImpl#getInheritsFrom <em>Inherits From</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ODRLPolicyImpl extends MinimalEObjectImpl.Container implements ODRLPolicy {
	/**
	 * The default value of the '{@link #getUid() <em>Uid</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUid()
	 * @generated
	 * @ordered
	 */
	protected static final String UID_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getUid() <em>Uid</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUid()
	 * @generated
	 * @ordered
	 */
	protected String uid = UID_EDEFAULT;

	/**
	 * The cached value of the '{@link #getBase_Activity() <em>Base Activity</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Activity()
	 * @generated
	 * @ordered
	 */
	protected Activity base_Activity;

	/**
	 * The default value of the '{@link #getConflictStrategy() <em>Conflict Strategy</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getConflictStrategy()
	 * @generated
	 * @ordered
	 */
	protected static final ConflictStrategy CONFLICT_STRATEGY_EDEFAULT = ConflictStrategy.NULL;

	/**
	 * The cached value of the '{@link #getConflictStrategy() <em>Conflict Strategy</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getConflictStrategy()
	 * @generated
	 * @ordered
	 */
	protected ConflictStrategy conflictStrategy = CONFLICT_STRATEGY_EDEFAULT;

	/**
	 * The default value of the '{@link #getPolicyType() <em>Policy Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPolicyType()
	 * @generated
	 * @ordered
	 */
	protected static final PolicyType POLICY_TYPE_EDEFAULT = PolicyType.NULL;

	/**
	 * The cached value of the '{@link #getPolicyType() <em>Policy Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPolicyType()
	 * @generated
	 * @ordered
	 */
	protected PolicyType policyType = POLICY_TYPE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getProfiles() <em>Profiles</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProfiles()
	 * @generated
	 * @ordered
	 */
	protected EList<String> profiles;

	/**
	 * The cached value of the '{@link #getInheritsFrom() <em>Inherits From</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getInheritsFrom()
	 * @generated
	 * @ordered
	 */
	protected EList<String> inheritsFrom;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ODRLPolicyImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ODRLCommonVocabularyPackage.Literals.ODRL_POLICY;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getUid() {
		return uid;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setUid(String newUid) {
		String oldUid = uid;
		uid = newUid;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.ODRL_POLICY__UID, oldUid, uid));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Activity getBase_Activity() {
		if (base_Activity != null && base_Activity.eIsProxy()) {
			InternalEObject oldBase_Activity = (InternalEObject)base_Activity;
			base_Activity = (Activity)eResolveProxy(oldBase_Activity);
			if (base_Activity != oldBase_Activity) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, ODRLCommonVocabularyPackage.ODRL_POLICY__BASE_ACTIVITY, oldBase_Activity, base_Activity));
			}
		}
		return base_Activity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Activity basicGetBase_Activity() {
		return base_Activity;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_Activity(Activity newBase_Activity) {
		Activity oldBase_Activity = base_Activity;
		base_Activity = newBase_Activity;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.ODRL_POLICY__BASE_ACTIVITY, oldBase_Activity, base_Activity));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ConflictStrategy getConflictStrategy() {
		return conflictStrategy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setConflictStrategy(ConflictStrategy newConflictStrategy) {
		ConflictStrategy oldConflictStrategy = conflictStrategy;
		conflictStrategy = newConflictStrategy == null ? CONFLICT_STRATEGY_EDEFAULT : newConflictStrategy;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.ODRL_POLICY__CONFLICT_STRATEGY, oldConflictStrategy, conflictStrategy));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public PolicyType getPolicyType() {
		return policyType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setPolicyType(PolicyType newPolicyType) {
		PolicyType oldPolicyType = policyType;
		policyType = newPolicyType == null ? POLICY_TYPE_EDEFAULT : newPolicyType;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.ODRL_POLICY__POLICY_TYPE, oldPolicyType, policyType));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getProfiles() {
		if (profiles == null) {
			profiles = new EDataTypeUniqueEList<String>(String.class, this, ODRLCommonVocabularyPackage.ODRL_POLICY__PROFILES);
		}
		return profiles;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getInheritsFrom() {
		if (inheritsFrom == null) {
			inheritsFrom = new EDataTypeUniqueEList<String>(String.class, this, ODRLCommonVocabularyPackage.ODRL_POLICY__INHERITS_FROM);
		}
		return inheritsFrom;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ODRLCommonVocabularyPackage.ODRL_POLICY__UID:
				return getUid();
			case ODRLCommonVocabularyPackage.ODRL_POLICY__BASE_ACTIVITY:
				if (resolve) return getBase_Activity();
				return basicGetBase_Activity();
			case ODRLCommonVocabularyPackage.ODRL_POLICY__CONFLICT_STRATEGY:
				return getConflictStrategy();
			case ODRLCommonVocabularyPackage.ODRL_POLICY__POLICY_TYPE:
				return getPolicyType();
			case ODRLCommonVocabularyPackage.ODRL_POLICY__PROFILES:
				return getProfiles();
			case ODRLCommonVocabularyPackage.ODRL_POLICY__INHERITS_FROM:
				return getInheritsFrom();
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
			case ODRLCommonVocabularyPackage.ODRL_POLICY__UID:
				setUid((String)newValue);
				return;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__BASE_ACTIVITY:
				setBase_Activity((Activity)newValue);
				return;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__CONFLICT_STRATEGY:
				setConflictStrategy((ConflictStrategy)newValue);
				return;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__POLICY_TYPE:
				setPolicyType((PolicyType)newValue);
				return;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__PROFILES:
				getProfiles().clear();
				getProfiles().addAll((Collection<? extends String>)newValue);
				return;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__INHERITS_FROM:
				getInheritsFrom().clear();
				getInheritsFrom().addAll((Collection<? extends String>)newValue);
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
			case ODRLCommonVocabularyPackage.ODRL_POLICY__UID:
				setUid(UID_EDEFAULT);
				return;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__BASE_ACTIVITY:
				setBase_Activity((Activity)null);
				return;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__CONFLICT_STRATEGY:
				setConflictStrategy(CONFLICT_STRATEGY_EDEFAULT);
				return;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__POLICY_TYPE:
				setPolicyType(POLICY_TYPE_EDEFAULT);
				return;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__PROFILES:
				getProfiles().clear();
				return;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__INHERITS_FROM:
				getInheritsFrom().clear();
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
			case ODRLCommonVocabularyPackage.ODRL_POLICY__UID:
				return UID_EDEFAULT == null ? uid != null : !UID_EDEFAULT.equals(uid);
			case ODRLCommonVocabularyPackage.ODRL_POLICY__BASE_ACTIVITY:
				return base_Activity != null;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__CONFLICT_STRATEGY:
				return conflictStrategy != CONFLICT_STRATEGY_EDEFAULT;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__POLICY_TYPE:
				return policyType != POLICY_TYPE_EDEFAULT;
			case ODRLCommonVocabularyPackage.ODRL_POLICY__PROFILES:
				return profiles != null && !profiles.isEmpty();
			case ODRLCommonVocabularyPackage.ODRL_POLICY__INHERITS_FROM:
				return inheritsFrom != null && !inheritsFrom.isEmpty();
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
		result.append(" (uid: ");
		result.append(uid);
		result.append(", conflictStrategy: ");
		result.append(conflictStrategy);
		result.append(", policyType: ");
		result.append(policyType);
		result.append(", profiles: ");
		result.append(profiles);
		result.append(", inheritsFrom: ");
		result.append(inheritsFrom);
		result.append(')');
		return result.toString();
	}

} //ODRLPolicyImpl
