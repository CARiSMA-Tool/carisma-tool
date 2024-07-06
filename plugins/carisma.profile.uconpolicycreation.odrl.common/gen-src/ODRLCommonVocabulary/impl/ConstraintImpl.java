/**
 */
package ODRLCommonVocabulary.impl;

import ODRLCommonVocabulary.Constraint;
import ODRLCommonVocabulary.ConstraintOperator;
import ODRLCommonVocabulary.LeftOperand;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Constraint</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.impl.ConstraintImpl#getUid <em>Uid</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ConstraintImpl#getLeftOperand <em>Left Operand</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ConstraintImpl#getStatus <em>Status</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ConstraintImpl#getOperator <em>Operator</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ConstraintImpl#getRightOperand <em>Right Operand</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ConstraintImpl#getRightOperandReference <em>Right Operand Reference</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ConstraintImpl#getDataType <em>Data Type</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.impl.ConstraintImpl#getUnit <em>Unit</em>}</li>
 * </ul>
 *
 * @generated
 */
public class ConstraintImpl extends MinimalEObjectImpl.Container implements Constraint {
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
	 * The default value of the '{@link #getLeftOperand() <em>Left Operand</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLeftOperand()
	 * @generated
	 * @ordered
	 */
	protected static final LeftOperand LEFT_OPERAND_EDEFAULT = LeftOperand.NULL;

	/**
	 * The cached value of the '{@link #getLeftOperand() <em>Left Operand</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLeftOperand()
	 * @generated
	 * @ordered
	 */
	protected LeftOperand leftOperand = LEFT_OPERAND_EDEFAULT;

	/**
	 * The default value of the '{@link #getStatus() <em>Status</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStatus()
	 * @generated
	 * @ordered
	 */
	protected static final String STATUS_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getStatus() <em>Status</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStatus()
	 * @generated
	 * @ordered
	 */
	protected String status = STATUS_EDEFAULT;

	/**
	 * The default value of the '{@link #getOperator() <em>Operator</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOperator()
	 * @generated
	 * @ordered
	 */
	protected static final ConstraintOperator OPERATOR_EDEFAULT = ConstraintOperator.NULL;

	/**
	 * The cached value of the '{@link #getOperator() <em>Operator</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOperator()
	 * @generated
	 * @ordered
	 */
	protected ConstraintOperator operator = OPERATOR_EDEFAULT;

	/**
	 * The cached value of the '{@link #getRightOperand() <em>Right Operand</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRightOperand()
	 * @generated
	 * @ordered
	 */
	protected EList<String> rightOperand;

	/**
	 * The cached value of the '{@link #getRightOperandReference() <em>Right Operand Reference</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRightOperandReference()
	 * @generated
	 * @ordered
	 */
	protected EList<String> rightOperandReference;

	/**
	 * The default value of the '{@link #getDataType() <em>Data Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDataType()
	 * @generated
	 * @ordered
	 */
	protected static final String DATA_TYPE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDataType() <em>Data Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDataType()
	 * @generated
	 * @ordered
	 */
	protected String dataType = DATA_TYPE_EDEFAULT;

	/**
	 * The default value of the '{@link #getUnit() <em>Unit</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUnit()
	 * @generated
	 * @ordered
	 */
	protected static final String UNIT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getUnit() <em>Unit</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUnit()
	 * @generated
	 * @ordered
	 */
	protected String unit = UNIT_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ConstraintImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ODRLCommonVocabularyPackage.Literals.CONSTRAINT;
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
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.CONSTRAINT__UID, oldUid, uid));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public LeftOperand getLeftOperand() {
		return leftOperand;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setLeftOperand(LeftOperand newLeftOperand) {
		LeftOperand oldLeftOperand = leftOperand;
		leftOperand = newLeftOperand == null ? LEFT_OPERAND_EDEFAULT : newLeftOperand;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.CONSTRAINT__LEFT_OPERAND, oldLeftOperand, leftOperand));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getStatus() {
		return status;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setStatus(String newStatus) {
		String oldStatus = status;
		status = newStatus;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.CONSTRAINT__STATUS, oldStatus, status));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ConstraintOperator getOperator() {
		return operator;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setOperator(ConstraintOperator newOperator) {
		ConstraintOperator oldOperator = operator;
		operator = newOperator == null ? OPERATOR_EDEFAULT : newOperator;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.CONSTRAINT__OPERATOR, oldOperator, operator));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getRightOperand() {
		if (rightOperand == null) {
			rightOperand = new EDataTypeUniqueEList<String>(String.class, this, ODRLCommonVocabularyPackage.CONSTRAINT__RIGHT_OPERAND);
		}
		return rightOperand;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getRightOperandReference() {
		if (rightOperandReference == null) {
			rightOperandReference = new EDataTypeUniqueEList<String>(String.class, this, ODRLCommonVocabularyPackage.CONSTRAINT__RIGHT_OPERAND_REFERENCE);
		}
		return rightOperandReference;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getDataType() {
		return dataType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setDataType(String newDataType) {
		String oldDataType = dataType;
		dataType = newDataType;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.CONSTRAINT__DATA_TYPE, oldDataType, dataType));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getUnit() {
		return unit;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setUnit(String newUnit) {
		String oldUnit = unit;
		unit = newUnit;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ODRLCommonVocabularyPackage.CONSTRAINT__UNIT, oldUnit, unit));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ODRLCommonVocabularyPackage.CONSTRAINT__UID:
				return getUid();
			case ODRLCommonVocabularyPackage.CONSTRAINT__LEFT_OPERAND:
				return getLeftOperand();
			case ODRLCommonVocabularyPackage.CONSTRAINT__STATUS:
				return getStatus();
			case ODRLCommonVocabularyPackage.CONSTRAINT__OPERATOR:
				return getOperator();
			case ODRLCommonVocabularyPackage.CONSTRAINT__RIGHT_OPERAND:
				return getRightOperand();
			case ODRLCommonVocabularyPackage.CONSTRAINT__RIGHT_OPERAND_REFERENCE:
				return getRightOperandReference();
			case ODRLCommonVocabularyPackage.CONSTRAINT__DATA_TYPE:
				return getDataType();
			case ODRLCommonVocabularyPackage.CONSTRAINT__UNIT:
				return getUnit();
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
			case ODRLCommonVocabularyPackage.CONSTRAINT__UID:
				setUid((String)newValue);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__LEFT_OPERAND:
				setLeftOperand((LeftOperand)newValue);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__STATUS:
				setStatus((String)newValue);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__OPERATOR:
				setOperator((ConstraintOperator)newValue);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__RIGHT_OPERAND:
				getRightOperand().clear();
				getRightOperand().addAll((Collection<? extends String>)newValue);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__RIGHT_OPERAND_REFERENCE:
				getRightOperandReference().clear();
				getRightOperandReference().addAll((Collection<? extends String>)newValue);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__DATA_TYPE:
				setDataType((String)newValue);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__UNIT:
				setUnit((String)newValue);
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
			case ODRLCommonVocabularyPackage.CONSTRAINT__UID:
				setUid(UID_EDEFAULT);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__LEFT_OPERAND:
				setLeftOperand(LEFT_OPERAND_EDEFAULT);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__STATUS:
				setStatus(STATUS_EDEFAULT);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__OPERATOR:
				setOperator(OPERATOR_EDEFAULT);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__RIGHT_OPERAND:
				getRightOperand().clear();
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__RIGHT_OPERAND_REFERENCE:
				getRightOperandReference().clear();
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__DATA_TYPE:
				setDataType(DATA_TYPE_EDEFAULT);
				return;
			case ODRLCommonVocabularyPackage.CONSTRAINT__UNIT:
				setUnit(UNIT_EDEFAULT);
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
			case ODRLCommonVocabularyPackage.CONSTRAINT__UID:
				return UID_EDEFAULT == null ? uid != null : !UID_EDEFAULT.equals(uid);
			case ODRLCommonVocabularyPackage.CONSTRAINT__LEFT_OPERAND:
				return leftOperand != LEFT_OPERAND_EDEFAULT;
			case ODRLCommonVocabularyPackage.CONSTRAINT__STATUS:
				return STATUS_EDEFAULT == null ? status != null : !STATUS_EDEFAULT.equals(status);
			case ODRLCommonVocabularyPackage.CONSTRAINT__OPERATOR:
				return operator != OPERATOR_EDEFAULT;
			case ODRLCommonVocabularyPackage.CONSTRAINT__RIGHT_OPERAND:
				return rightOperand != null && !rightOperand.isEmpty();
			case ODRLCommonVocabularyPackage.CONSTRAINT__RIGHT_OPERAND_REFERENCE:
				return rightOperandReference != null && !rightOperandReference.isEmpty();
			case ODRLCommonVocabularyPackage.CONSTRAINT__DATA_TYPE:
				return DATA_TYPE_EDEFAULT == null ? dataType != null : !DATA_TYPE_EDEFAULT.equals(dataType);
			case ODRLCommonVocabularyPackage.CONSTRAINT__UNIT:
				return UNIT_EDEFAULT == null ? unit != null : !UNIT_EDEFAULT.equals(unit);
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
		result.append(", leftOperand: ");
		result.append(leftOperand);
		result.append(", status: ");
		result.append(status);
		result.append(", operator: ");
		result.append(operator);
		result.append(", rightOperand: ");
		result.append(rightOperand);
		result.append(", rightOperandReference: ");
		result.append(rightOperandReference);
		result.append(", dataType: ");
		result.append(dataType);
		result.append(", unit: ");
		result.append(unit);
		result.append(')');
		return result.toString();
	}

} //ConstraintImpl
