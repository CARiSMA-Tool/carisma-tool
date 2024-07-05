/**
 */
package ODRLCommonVocabulary.impl;

import ODRLCommonVocabulary.*;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class ODRLCommonVocabularyFactoryImpl extends EFactoryImpl implements ODRLCommonVocabularyFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static ODRLCommonVocabularyFactory init() {
		try {
			ODRLCommonVocabularyFactory theODRLCommonVocabularyFactory = (ODRLCommonVocabularyFactory)EPackage.Registry.INSTANCE.getEFactory(ODRLCommonVocabularyPackage.eNS_URI);
			if (theODRLCommonVocabularyFactory != null) {
				return theODRLCommonVocabularyFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new ODRLCommonVocabularyFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ODRLCommonVocabularyFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case ODRLCommonVocabularyPackage.ODRL_POLICY: return createODRLPolicy();
			case ODRLCommonVocabularyPackage.LOGICAL_CONSTRAINT: return createLogicalConstraint();
			case ODRLCommonVocabularyPackage.CONSTRAINT: return createConstraint();
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION: return createPartyFunction();
			case ODRLCommonVocabularyPackage.PARTY: return createParty();
			case ODRLCommonVocabularyPackage.ASSET_RELATION: return createAssetRelation();
			case ODRLCommonVocabularyPackage.ASSET: return createAsset();
			case ODRLCommonVocabularyPackage.PERMISSION: return createPermission();
			case ODRLCommonVocabularyPackage.DUTY: return createDuty();
			case ODRLCommonVocabularyPackage.PROHIBITION: return createProhibition();
			case ODRLCommonVocabularyPackage.ASSET_COLLECTION: return createAssetCollection();
			case ODRLCommonVocabularyPackage.PARTY_COLLECTION: return createPartyCollection();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
		switch (eDataType.getClassifierID()) {
			case ODRLCommonVocabularyPackage.CONFLICT_STRATEGY:
				return createConflictStrategyFromString(eDataType, initialValue);
			case ODRLCommonVocabularyPackage.POLICY_TYPE:
				return createPolicyTypeFromString(eDataType, initialValue);
			case ODRLCommonVocabularyPackage.LOGICAL_OPERATOR:
				return createLogicalOperatorFromString(eDataType, initialValue);
			case ODRLCommonVocabularyPackage.LEFT_OPERAND:
				return createLeftOperandFromString(eDataType, initialValue);
			case ODRLCommonVocabularyPackage.CONSTRAINT_OPERATOR:
				return createConstraintOperatorFromString(eDataType, initialValue);
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION_TYPE:
				return createPartyFunctionTypeFromString(eDataType, initialValue);
			case ODRLCommonVocabularyPackage.ASSET_RELATION_TYPE:
				return createAssetRelationTypeFromString(eDataType, initialValue);
			case ODRLCommonVocabularyPackage.ACTION:
				return createActionFromString(eDataType, initialValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		switch (eDataType.getClassifierID()) {
			case ODRLCommonVocabularyPackage.CONFLICT_STRATEGY:
				return convertConflictStrategyToString(eDataType, instanceValue);
			case ODRLCommonVocabularyPackage.POLICY_TYPE:
				return convertPolicyTypeToString(eDataType, instanceValue);
			case ODRLCommonVocabularyPackage.LOGICAL_OPERATOR:
				return convertLogicalOperatorToString(eDataType, instanceValue);
			case ODRLCommonVocabularyPackage.LEFT_OPERAND:
				return convertLeftOperandToString(eDataType, instanceValue);
			case ODRLCommonVocabularyPackage.CONSTRAINT_OPERATOR:
				return convertConstraintOperatorToString(eDataType, instanceValue);
			case ODRLCommonVocabularyPackage.PARTY_FUNCTION_TYPE:
				return convertPartyFunctionTypeToString(eDataType, instanceValue);
			case ODRLCommonVocabularyPackage.ASSET_RELATION_TYPE:
				return convertAssetRelationTypeToString(eDataType, instanceValue);
			case ODRLCommonVocabularyPackage.ACTION:
				return convertActionToString(eDataType, instanceValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ODRLPolicy createODRLPolicy() {
		ODRLPolicyImpl odrlPolicy = new ODRLPolicyImpl();
		return odrlPolicy;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public LogicalConstraint createLogicalConstraint() {
		LogicalConstraintImpl logicalConstraint = new LogicalConstraintImpl();
		return logicalConstraint;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Constraint createConstraint() {
		ConstraintImpl constraint = new ConstraintImpl();
		return constraint;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public PartyFunction createPartyFunction() {
		PartyFunctionImpl partyFunction = new PartyFunctionImpl();
		return partyFunction;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Party createParty() {
		PartyImpl party = new PartyImpl();
		return party;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Permission createPermission() {
		PermissionImpl permission = new PermissionImpl();
		return permission;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Duty createDuty() {
		DutyImpl duty = new DutyImpl();
		return duty;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Prohibition createProhibition() {
		ProhibitionImpl prohibition = new ProhibitionImpl();
		return prohibition;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Asset createAsset() {
		AssetImpl asset = new AssetImpl();
		return asset;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public AssetCollection createAssetCollection() {
		AssetCollectionImpl assetCollection = new AssetCollectionImpl();
		return assetCollection;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public PartyCollection createPartyCollection() {
		PartyCollectionImpl partyCollection = new PartyCollectionImpl();
		return partyCollection;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public AssetRelation createAssetRelation() {
		AssetRelationImpl assetRelation = new AssetRelationImpl();
		return assetRelation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ConflictStrategy createConflictStrategyFromString(EDataType eDataType, String initialValue) {
		ConflictStrategy result = ConflictStrategy.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertConflictStrategyToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public PolicyType createPolicyTypeFromString(EDataType eDataType, String initialValue) {
		PolicyType result = PolicyType.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertPolicyTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public LogicalOperator createLogicalOperatorFromString(EDataType eDataType, String initialValue) {
		LogicalOperator result = LogicalOperator.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertLogicalOperatorToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public LeftOperand createLeftOperandFromString(EDataType eDataType, String initialValue) {
		LeftOperand result = LeftOperand.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertLeftOperandToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ConstraintOperator createConstraintOperatorFromString(EDataType eDataType, String initialValue) {
		ConstraintOperator result = ConstraintOperator.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertConstraintOperatorToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public PartyFunctionType createPartyFunctionTypeFromString(EDataType eDataType, String initialValue) {
		PartyFunctionType result = PartyFunctionType.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertPartyFunctionTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public AssetRelationType createAssetRelationTypeFromString(EDataType eDataType, String initialValue) {
		AssetRelationType result = AssetRelationType.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertAssetRelationTypeToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Action createActionFromString(EDataType eDataType, String initialValue) {
		Action result = Action.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertActionToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ODRLCommonVocabularyPackage getODRLCommonVocabularyPackage() {
		return (ODRLCommonVocabularyPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static ODRLCommonVocabularyPackage getPackage() {
		return ODRLCommonVocabularyPackage.eINSTANCE;
	}

} //ODRLCommonVocabularyFactoryImpl
