/**
 */
package ODRLCommonVocabulary.impl;

import ODRLCommonVocabulary.Duty;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.Permission;

import java.util.Collection;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.util.EObjectResolvingEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Permission</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.impl.PermissionImpl#getDuties <em>Duties</em>}</li>
 * </ul>
 *
 * @generated
 */
public class PermissionImpl extends RuleImpl implements Permission {
	/**
	 * The cached value of the '{@link #getDuties() <em>Duties</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDuties()
	 * @generated
	 * @ordered
	 */
	protected EList<Duty> duties;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected PermissionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ODRLCommonVocabularyPackage.Literals.PERMISSION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Duty> getDuties() {
		if (duties == null) {
			duties = new EObjectResolvingEList<Duty>(Duty.class, this, ODRLCommonVocabularyPackage.PERMISSION__DUTIES);
		}
		return duties;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ODRLCommonVocabularyPackage.PERMISSION__DUTIES:
				return getDuties();
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
			case ODRLCommonVocabularyPackage.PERMISSION__DUTIES:
				getDuties().clear();
				getDuties().addAll((Collection<? extends Duty>)newValue);
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
			case ODRLCommonVocabularyPackage.PERMISSION__DUTIES:
				getDuties().clear();
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
			case ODRLCommonVocabularyPackage.PERMISSION__DUTIES:
				return duties != null && !duties.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //PermissionImpl
