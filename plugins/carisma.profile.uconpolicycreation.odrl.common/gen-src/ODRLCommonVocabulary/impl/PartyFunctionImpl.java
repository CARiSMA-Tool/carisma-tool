/**
 */
package ODRLCommonVocabulary.impl;

import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.Party;
import ODRLCommonVocabulary.PartyFunction;
import ODRLCommonVocabulary.PartyFunctionType;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Party Function</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.impl.PartyFunctionImpl#getType <em>Type</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.PartyFunctionImpl#getParty <em>Party</em>}</li>
 * </ul>
 *
 * @generated
 */
public class PartyFunctionImpl extends MinimalEObjectImpl.Container implements PartyFunction {
	/**
	 * The default value of the '{@link #getType() <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected static final PartyFunctionType TYPE_EDEFAULT = PartyFunctionType.NULL;

	/**
	 * The cached value of the '{@link #getType() <em>Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getType()
	 * @generated
	 * @ordered
	 */
	protected PartyFunctionType type = TYPE_EDEFAULT;

	/**
	 * The cached value of the '{@link #getParty() <em>Party</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getParty()
	 * @generated
	 * @ordered
	 */
	protected Party party;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected PartyFunctionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ODRLCommonVocabularyPackage.Literals.PARTY_FUNCTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public PartyFunctionType getType() {
		return type;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setType(PartyFunctionType newType) {
		PartyFunctionType oldType = type;
		type = newType == null ? TYPE_EDEFAULT : newType;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.PARTY_FUNCTION__TYPE, oldType, type));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Party getParty() {
		if (party != null && party.eIsProxy()) {
			InternalEObject oldParty = (InternalEObject)party;
			party = (Party)eResolveProxy(oldParty);
			if (party != oldParty) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, ODRLCommonVocabularyPackage.PARTY_FUNCTION__PARTY, oldParty, party));
			}
		}
		return party;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Party basicGetParty() {
		return party;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setParty(Party newParty) {
		Party oldParty = party;
		party = newParty;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.PARTY_FUNCTION__PARTY, oldParty, party));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION__TYPE:
				return getType();
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION__PARTY:
				if (resolve) return getParty();
				return basicGetParty();
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
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION__TYPE:
				setType((PartyFunctionType)newValue);
				return;
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION__PARTY:
				setParty((Party)newValue);
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
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION__TYPE:
				setType(TYPE_EDEFAULT);
				return;
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION__PARTY:
				setParty((Party)null);
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
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION__TYPE:
				return type != TYPE_EDEFAULT;
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION__PARTY:
				return party != null;
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

} //PartyFunctionImpl
