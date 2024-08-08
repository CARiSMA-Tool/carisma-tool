/**
 */
package ODRLCommonVocabulary.impl;

import ODRLCommonVocabulary.LogicalConstraint;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.PartyFunction;
import ODRLCommonVocabulary.RefinableElement;
import ODRLCommonVocabulary.Rule;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.uml2.uml.Action;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.impl.RuleImpl#getRefinement <em>Refinement</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.RuleImpl#getBase_Action <em>Base Action</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.RuleImpl#getUid <em>Uid</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.RuleImpl#getInvolvedParties <em>Involved Parties</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.RuleImpl#getAction <em>Action</em>}</li>
 * </ul>
 *
 * @generated
 */
public abstract class RuleImpl extends ConstrainableElementImpl implements Rule {
	/**
	 * The cached value of the '{@link #getRefinement() <em>Refinement</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRefinement()
	 * @generated
	 * @ordered
	 */
	protected LogicalConstraint refinement;

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
	 * The cached value of the '{@link #getInvolvedParties() <em>Involved Parties</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getInvolvedParties()
	 * @generated
	 * @ordered
	 */
	protected EList<PartyFunction> involvedParties;

	/**
	 * The default value of the '{@link #getAction() <em>Action</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAction()
	 * @generated
	 * @ordered
	 */
	protected static final ODRLCommonVocabulary.Action ACTION_EDEFAULT = ODRLCommonVocabulary.Action.NULL;

	/**
	 * The cached value of the '{@link #getAction() <em>Action</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAction()
	 * @generated
	 * @ordered
	 */
	protected ODRLCommonVocabulary.Action action = ACTION_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected RuleImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ODRLCommonVocabularyPackage.Literals.RULE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public LogicalConstraint getRefinement() {
		return refinement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetRefinement(LogicalConstraint newRefinement, NotificationChain msgs) {
		LogicalConstraint oldRefinement = refinement;
		refinement = newRefinement;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.RULE__REFINEMENT, oldRefinement, newRefinement);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setRefinement(LogicalConstraint newRefinement) {
		if (newRefinement != refinement) {
			NotificationChain msgs = null;
			if (refinement != null)
				msgs = ((InternalEObject)refinement).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ODRLCommonVocabularyPackage.RULE__REFINEMENT, null, msgs);
			if (newRefinement != null)
				msgs = ((InternalEObject)newRefinement).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ODRLCommonVocabularyPackage.RULE__REFINEMENT, null, msgs);
			msgs = basicSetRefinement(newRefinement, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.RULE__REFINEMENT, newRefinement, newRefinement));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Action getBase_Action() {
		if (base_Action != null && base_Action.eIsProxy()) {
			InternalEObject oldBase_Action = (InternalEObject)base_Action;
			base_Action = (Action)eResolveProxy(oldBase_Action);
			if (base_Action != oldBase_Action) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, ODRLCommonVocabularyPackage.RULE__BASE_ACTION, oldBase_Action, base_Action));
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
	@Override
	public void setBase_Action(Action newBase_Action) {
		Action oldBase_Action = base_Action;
		base_Action = newBase_Action;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.RULE__BASE_ACTION, oldBase_Action, base_Action));
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
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.RULE__UID, oldUid, uid));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<PartyFunction> getInvolvedParties() {
		if (involvedParties == null) {
			involvedParties = new EObjectContainmentEList<PartyFunction>(PartyFunction.class, this, ODRLCommonVocabularyPackage.RULE__INVOLVED_PARTIES);
		}
		return involvedParties;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ODRLCommonVocabulary.Action getAction() {
		return action;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setAction(ODRLCommonVocabulary.Action newAction) {
		ODRLCommonVocabulary.Action oldAction = action;
		action = newAction == null ? ACTION_EDEFAULT : newAction;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.RULE__ACTION, oldAction, action));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ODRLCommonVocabularyPackage.RULE__REFINEMENT:
				return basicSetRefinement(null, msgs);
			case ODRLCommonVocabularyPackage.RULE__INVOLVED_PARTIES:
				return ((InternalEList<?>)getInvolvedParties()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ODRLCommonVocabularyPackage.RULE__REFINEMENT:
				return getRefinement();
			case ODRLCommonVocabularyPackage.RULE__BASE_ACTION:
				if (resolve) return getBase_Action();
				return basicGetBase_Action();
			case ODRLCommonVocabularyPackage.RULE__UID:
				return getUid();
			case ODRLCommonVocabularyPackage.RULE__INVOLVED_PARTIES:
				return getInvolvedParties();
			case ODRLCommonVocabularyPackage.RULE__ACTION:
				return getAction();
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
			case ODRLCommonVocabularyPackage.RULE__REFINEMENT:
				setRefinement((LogicalConstraint)newValue);
				return;
			case ODRLCommonVocabularyPackage.RULE__BASE_ACTION:
				setBase_Action((Action)newValue);
				return;
			case ODRLCommonVocabularyPackage.RULE__UID:
				setUid((String)newValue);
				return;
			case ODRLCommonVocabularyPackage.RULE__INVOLVED_PARTIES:
				getInvolvedParties().clear();
				getInvolvedParties().addAll((Collection<? extends PartyFunction>)newValue);
				return;
			case ODRLCommonVocabularyPackage.RULE__ACTION:
				setAction((ODRLCommonVocabulary.Action)newValue);
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
			case ODRLCommonVocabularyPackage.RULE__REFINEMENT:
				setRefinement((LogicalConstraint)null);
				return;
			case ODRLCommonVocabularyPackage.RULE__BASE_ACTION:
				setBase_Action((Action)null);
				return;
			case ODRLCommonVocabularyPackage.RULE__UID:
				setUid(UID_EDEFAULT);
				return;
			case ODRLCommonVocabularyPackage.RULE__INVOLVED_PARTIES:
				getInvolvedParties().clear();
				return;
			case ODRLCommonVocabularyPackage.RULE__ACTION:
				setAction(ACTION_EDEFAULT);
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
			case ODRLCommonVocabularyPackage.RULE__REFINEMENT:
				return refinement != null;
			case ODRLCommonVocabularyPackage.RULE__BASE_ACTION:
				return base_Action != null;
			case ODRLCommonVocabularyPackage.RULE__UID:
				return UID_EDEFAULT == null ? uid != null : !UID_EDEFAULT.equals(uid);
			case ODRLCommonVocabularyPackage.RULE__INVOLVED_PARTIES:
				return involvedParties != null && !involvedParties.isEmpty();
			case ODRLCommonVocabularyPackage.RULE__ACTION:
				return action != ACTION_EDEFAULT;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public int eBaseStructuralFeatureID(int derivedFeatureID, Class<?> baseClass) {
		if (baseClass == RefinableElement.class) {
			switch (derivedFeatureID) {
				case ODRLCommonVocabularyPackage.RULE__REFINEMENT: return ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT;
				default: return -1;
			}
		}
		return super.eBaseStructuralFeatureID(derivedFeatureID, baseClass);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public int eDerivedStructuralFeatureID(int baseFeatureID, Class<?> baseClass) {
		if (baseClass == RefinableElement.class) {
			switch (baseFeatureID) {
				case ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT: return ODRLCommonVocabularyPackage.RULE__REFINEMENT;
				default: return -1;
			}
		}
		return super.eDerivedStructuralFeatureID(baseFeatureID, baseClass);
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
		result.append(", action: ");
		result.append(action);
		result.append(')');
		return result.toString();
	}

} //RuleImpl
