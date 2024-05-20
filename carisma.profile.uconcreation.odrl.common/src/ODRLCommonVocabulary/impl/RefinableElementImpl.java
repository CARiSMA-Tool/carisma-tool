/**
 */
package ODRLCommonVocabulary.impl;

import ODRLCommonVocabulary.LogicalConstraint;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.RefinableElement;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Refinable Element</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.impl.RefinableElementImpl#getRefinement <em>Refinement</em>}</li>
 * </ul>
 *
 * @generated
 */
public abstract class RefinableElementImpl extends MinimalEObjectImpl.Container implements RefinableElement {
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
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected RefinableElementImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ODRLCommonVocabularyPackage.Literals.REFINABLE_ELEMENT;
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
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT, oldRefinement, newRefinement);
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
				msgs = ((InternalEObject)refinement).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT, null, msgs);
			if (newRefinement != null)
				msgs = ((InternalEObject)newRefinement).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT, null, msgs);
			msgs = basicSetRefinement(newRefinement, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT, newRefinement, newRefinement));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT:
				return basicSetRefinement(null, msgs);
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
			case ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT:
				return getRefinement();
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
			case ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT:
				setRefinement((LogicalConstraint)newValue);
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
			case ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT:
				setRefinement((LogicalConstraint)null);
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
			case ODRLCommonVocabularyPackage.REFINABLE_ELEMENT__REFINEMENT:
				return refinement != null;
		}
		return super.eIsSet(featureID);
	}

} //RefinableElementImpl
