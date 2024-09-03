/**
 */
package ODRLCommonVocabulary.impl;

import ODRLCommonVocabulary.Action;
import ODRLCommonVocabulary.Asset;
import ODRLCommonVocabulary.AssetCollection;
import ODRLCommonVocabulary.ConflictStrategy;
import ODRLCommonVocabulary.ConstrainableElement;
import ODRLCommonVocabulary.Constraint;
import ODRLCommonVocabulary.ConstraintOperator;
import ODRLCommonVocabulary.Duty;
import ODRLCommonVocabulary.LeftOperand;
import ODRLCommonVocabulary.LogicalConstraint;
import ODRLCommonVocabulary.LogicalOperator;
import ODRLCommonVocabulary.ODRLCommonVocabularyFactory;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.ODRLPolicy;
import ODRLCommonVocabulary.Party;
import ODRLCommonVocabulary.PartyCollection;
import ODRLCommonVocabulary.PartyFunction;
import ODRLCommonVocabulary.PartyFunctionType;
import ODRLCommonVocabulary.Permission;
import ODRLCommonVocabulary.PolicyType;
import ODRLCommonVocabulary.Prohibition;
import ODRLCommonVocabulary.RefinableElement;
import ODRLCommonVocabulary.Rule;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcorePackage;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.uml2.types.TypesPackage;

import org.eclipse.uml2.uml.UMLPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class ODRLCommonVocabularyPackageImpl extends EPackageImpl implements ODRLCommonVocabularyPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass odrlPolicyEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass ruleEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass refinableElementEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass logicalConstraintEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass constraintEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass constrainableElementEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass partyFunctionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass partyEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass permissionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass dutyEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass prohibitionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass assetEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass assetCollectionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass partyCollectionEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum conflictStrategyEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum policyTypeEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum logicalOperatorEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum leftOperandEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum constraintOperatorEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum partyFunctionTypeEEnum = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum actionEEnum = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private ODRLCommonVocabularyPackageImpl() {
		super(eNS_URI, ODRLCommonVocabularyFactory.eINSTANCE);
	}
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 *
	 * <p>This method is used to initialize {@link ODRLCommonVocabularyPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static ODRLCommonVocabularyPackage init() {
		if (isInited) return (ODRLCommonVocabularyPackage)EPackage.Registry.INSTANCE.getEPackage(ODRLCommonVocabularyPackage.eNS_URI);

		// Obtain or create and register package
		Object registeredODRLCommonVocabularyPackage = EPackage.Registry.INSTANCE.get(eNS_URI);
		ODRLCommonVocabularyPackageImpl theODRLCommonVocabularyPackage = registeredODRLCommonVocabularyPackage instanceof ODRLCommonVocabularyPackageImpl ? (ODRLCommonVocabularyPackageImpl)registeredODRLCommonVocabularyPackage : new ODRLCommonVocabularyPackageImpl();

		isInited = true;

		// Initialize simple dependencies
		EcorePackage.eINSTANCE.eClass();
		TypesPackage.eINSTANCE.eClass();
		UMLPackage.eINSTANCE.eClass();

		// Create package meta-data objects
		theODRLCommonVocabularyPackage.createPackageContents();

		// Initialize created meta-data
		theODRLCommonVocabularyPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theODRLCommonVocabularyPackage.freeze();

		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(ODRLCommonVocabularyPackage.eNS_URI, theODRLCommonVocabularyPackage);
		return theODRLCommonVocabularyPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getODRLPolicy() {
		return odrlPolicyEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getODRLPolicy_Uid() {
		return (EAttribute)odrlPolicyEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getODRLPolicy_Base_Activity() {
		return (EReference)odrlPolicyEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getODRLPolicy_ConflictStrategy() {
		return (EAttribute)odrlPolicyEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getODRLPolicy_PolicyType() {
		return (EAttribute)odrlPolicyEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getODRLPolicy_Profiles() {
		return (EAttribute)odrlPolicyEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getODRLPolicy_InheritsFrom() {
		return (EAttribute)odrlPolicyEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getRule() {
		return ruleEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getRule_Base_Action() {
		return (EReference)ruleEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getRule_Uid() {
		return (EAttribute)ruleEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getRule_InvolvedParties() {
		return (EReference)ruleEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getRule_Action() {
		return (EAttribute)ruleEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getRefinableElement() {
		return refinableElementEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getRefinableElement_Refinement() {
		return (EReference)refinableElementEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getLogicalConstraint() {
		return logicalConstraintEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getLogicalConstraint_Uid() {
		return (EAttribute)logicalConstraintEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getLogicalConstraint_LogicalOperator() {
		return (EAttribute)logicalConstraintEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getLogicalConstraint_Constraints() {
		return (EReference)logicalConstraintEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getConstraint() {
		return constraintEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getConstraint_Uid() {
		return (EAttribute)constraintEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getConstraint_LeftOperand() {
		return (EAttribute)constraintEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getConstraint_Status() {
		return (EAttribute)constraintEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getConstraint_Operator() {
		return (EAttribute)constraintEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getConstraint_RightOperand() {
		return (EAttribute)constraintEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getConstraint_RightOperandReference() {
		return (EAttribute)constraintEClass.getEStructuralFeatures().get(5);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getConstraint_DataType() {
		return (EAttribute)constraintEClass.getEStructuralFeatures().get(6);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getConstraint_Unit() {
		return (EAttribute)constraintEClass.getEStructuralFeatures().get(7);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getConstrainableElement() {
		return constrainableElementEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getConstrainableElement_Constraint() {
		return (EReference)constrainableElementEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getPartyFunction() {
		return partyFunctionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getPartyFunction_Type() {
		return (EAttribute)partyFunctionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getPartyFunction_Party() {
		return (EReference)partyFunctionEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getParty() {
		return partyEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getParty_Uid() {
		return (EAttribute)partyEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getParty_Base_ActivityPartition() {
		return (EReference)partyEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getPermission() {
		return permissionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getPermission_Duties() {
		return (EReference)permissionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getDuty() {
		return dutyEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getDuty_Consequences() {
		return (EReference)dutyEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getProhibition() {
		return prohibitionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getProhibition_Remedies() {
		return (EReference)prohibitionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getAsset() {
		return assetEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EReference getAsset_Base_Pin() {
		return (EReference)assetEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getAsset_Uid() {
		return (EAttribute)assetEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getAssetCollection() {
		return assetCollectionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getAssetCollection_Source() {
		return (EAttribute)assetCollectionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EClass getPartyCollection() {
		return partyCollectionEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EAttribute getPartyCollection_Source() {
		return (EAttribute)partyCollectionEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EEnum getConflictStrategy() {
		return conflictStrategyEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EEnum getPolicyType() {
		return policyTypeEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EEnum getLogicalOperator() {
		return logicalOperatorEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EEnum getLeftOperand() {
		return leftOperandEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EEnum getConstraintOperator() {
		return constraintOperatorEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EEnum getPartyFunctionType() {
		return partyFunctionTypeEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EEnum getAction() {
		return actionEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ODRLCommonVocabularyFactory getODRLCommonVocabularyFactory() {
		return (ODRLCommonVocabularyFactory)getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated) return;
		isCreated = true;

		// Create classes and their features
		odrlPolicyEClass = createEClass(ODRL_POLICY);
		createEAttribute(odrlPolicyEClass, ODRL_POLICY__UID);
		createEReference(odrlPolicyEClass, ODRL_POLICY__BASE_ACTIVITY);
		createEAttribute(odrlPolicyEClass, ODRL_POLICY__CONFLICT_STRATEGY);
		createEAttribute(odrlPolicyEClass, ODRL_POLICY__POLICY_TYPE);
		createEAttribute(odrlPolicyEClass, ODRL_POLICY__PROFILES);
		createEAttribute(odrlPolicyEClass, ODRL_POLICY__INHERITS_FROM);

		ruleEClass = createEClass(RULE);
		createEReference(ruleEClass, RULE__BASE_ACTION);
		createEAttribute(ruleEClass, RULE__UID);
		createEReference(ruleEClass, RULE__INVOLVED_PARTIES);
		createEAttribute(ruleEClass, RULE__ACTION);

		refinableElementEClass = createEClass(REFINABLE_ELEMENT);
		createEReference(refinableElementEClass, REFINABLE_ELEMENT__REFINEMENT);

		logicalConstraintEClass = createEClass(LOGICAL_CONSTRAINT);
		createEAttribute(logicalConstraintEClass, LOGICAL_CONSTRAINT__UID);
		createEAttribute(logicalConstraintEClass, LOGICAL_CONSTRAINT__LOGICAL_OPERATOR);
		createEReference(logicalConstraintEClass, LOGICAL_CONSTRAINT__CONSTRAINTS);

		constraintEClass = createEClass(CONSTRAINT);
		createEAttribute(constraintEClass, CONSTRAINT__UID);
		createEAttribute(constraintEClass, CONSTRAINT__LEFT_OPERAND);
		createEAttribute(constraintEClass, CONSTRAINT__STATUS);
		createEAttribute(constraintEClass, CONSTRAINT__OPERATOR);
		createEAttribute(constraintEClass, CONSTRAINT__RIGHT_OPERAND);
		createEAttribute(constraintEClass, CONSTRAINT__RIGHT_OPERAND_REFERENCE);
		createEAttribute(constraintEClass, CONSTRAINT__DATA_TYPE);
		createEAttribute(constraintEClass, CONSTRAINT__UNIT);

		constrainableElementEClass = createEClass(CONSTRAINABLE_ELEMENT);
		createEReference(constrainableElementEClass, CONSTRAINABLE_ELEMENT__CONSTRAINT);

		partyFunctionEClass = createEClass(PARTY_FUNCTION);
		createEAttribute(partyFunctionEClass, PARTY_FUNCTION__TYPE);
		createEReference(partyFunctionEClass, PARTY_FUNCTION__PARTY);

		partyEClass = createEClass(PARTY);
		createEAttribute(partyEClass, PARTY__UID);
		createEReference(partyEClass, PARTY__BASE_ACTIVITY_PARTITION);

		permissionEClass = createEClass(PERMISSION);
		createEReference(permissionEClass, PERMISSION__DUTIES);

		dutyEClass = createEClass(DUTY);
		createEReference(dutyEClass, DUTY__CONSEQUENCES);

		prohibitionEClass = createEClass(PROHIBITION);
		createEReference(prohibitionEClass, PROHIBITION__REMEDIES);

		assetEClass = createEClass(ASSET);
		createEReference(assetEClass, ASSET__BASE_PIN);
		createEAttribute(assetEClass, ASSET__UID);

		assetCollectionEClass = createEClass(ASSET_COLLECTION);
		createEAttribute(assetCollectionEClass, ASSET_COLLECTION__SOURCE);

		partyCollectionEClass = createEClass(PARTY_COLLECTION);
		createEAttribute(partyCollectionEClass, PARTY_COLLECTION__SOURCE);

		// Create enums
		conflictStrategyEEnum = createEEnum(CONFLICT_STRATEGY);
		policyTypeEEnum = createEEnum(POLICY_TYPE);
		logicalOperatorEEnum = createEEnum(LOGICAL_OPERATOR);
		leftOperandEEnum = createEEnum(LEFT_OPERAND);
		constraintOperatorEEnum = createEEnum(CONSTRAINT_OPERATOR);
		partyFunctionTypeEEnum = createEEnum(PARTY_FUNCTION_TYPE);
		actionEEnum = createEEnum(ACTION);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized) return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Obtain other dependent packages
		TypesPackage theTypesPackage = (TypesPackage)EPackage.Registry.INSTANCE.getEPackage(TypesPackage.eNS_URI);
		UMLPackage theUMLPackage = (UMLPackage)EPackage.Registry.INSTANCE.getEPackage(UMLPackage.eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		ruleEClass.getESuperTypes().add(this.getConstrainableElement());
		ruleEClass.getESuperTypes().add(this.getRefinableElement());
		permissionEClass.getESuperTypes().add(this.getRule());
		dutyEClass.getESuperTypes().add(this.getRule());
		prohibitionEClass.getESuperTypes().add(this.getRule());
		assetCollectionEClass.getESuperTypes().add(this.getAsset());
		assetCollectionEClass.getESuperTypes().add(this.getRefinableElement());
		partyCollectionEClass.getESuperTypes().add(this.getParty());
		partyCollectionEClass.getESuperTypes().add(this.getRefinableElement());

		// Initialize classes, features, and operations; add parameters
		initEClass(odrlPolicyEClass, ODRLPolicy.class, "ODRLPolicy", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getODRLPolicy_Uid(), theTypesPackage.getString(), "uid", null, 1, 1, ODRLPolicy.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getODRLPolicy_Base_Activity(), theUMLPackage.getActivity(), null, "base_Activity", null, 0, 1, ODRLPolicy.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getODRLPolicy_ConflictStrategy(), this.getConflictStrategy(), "conflictStrategy", "Null", 0, 1, ODRLPolicy.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getODRLPolicy_PolicyType(), this.getPolicyType(), "policyType", "Null", 0, 1, ODRLPolicy.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getODRLPolicy_Profiles(), theTypesPackage.getString(), "profiles", null, 0, -1, ODRLPolicy.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getODRLPolicy_InheritsFrom(), theTypesPackage.getString(), "inheritsFrom", null, 0, -1, ODRLPolicy.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(ruleEClass, Rule.class, "Rule", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getRule_Base_Action(), theUMLPackage.getAction(), null, "base_Action", null, 0, 1, Rule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getRule_Uid(), theTypesPackage.getString(), "uid", null, 0, 1, Rule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getRule_InvolvedParties(), this.getPartyFunction(), null, "involvedParties", null, 0, -1, Rule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getRule_Action(), this.getAction(), "action", "Null", 1, 1, Rule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(refinableElementEClass, RefinableElement.class, "RefinableElement", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getRefinableElement_Refinement(), this.getLogicalConstraint(), null, "refinement", null, 0, 1, RefinableElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(logicalConstraintEClass, LogicalConstraint.class, "LogicalConstraint", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getLogicalConstraint_Uid(), theTypesPackage.getString(), "uid", null, 0, 1, LogicalConstraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getLogicalConstraint_LogicalOperator(), this.getLogicalOperator(), "logicalOperator", "Null", 0, 1, LogicalConstraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getLogicalConstraint_Constraints(), this.getConstraint(), null, "constraints", null, 1, -1, LogicalConstraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(constraintEClass, Constraint.class, "Constraint", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getConstraint_Uid(), theTypesPackage.getString(), "uid", null, 0, 1, Constraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getConstraint_LeftOperand(), this.getLeftOperand(), "leftOperand", "Null", 1, 1, Constraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getConstraint_Status(), theTypesPackage.getString(), "status", null, 0, 1, Constraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getConstraint_Operator(), this.getConstraintOperator(), "operator", "Null", 1, 1, Constraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getConstraint_RightOperand(), theTypesPackage.getString(), "rightOperand", null, 0, -1, Constraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getConstraint_RightOperandReference(), theTypesPackage.getString(), "rightOperandReference", null, 0, -1, Constraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getConstraint_DataType(), theTypesPackage.getString(), "dataType", null, 0, 1, Constraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getConstraint_Unit(), theTypesPackage.getString(), "unit", null, 0, 1, Constraint.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(constrainableElementEClass, ConstrainableElement.class, "ConstrainableElement", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getConstrainableElement_Constraint(), this.getLogicalConstraint(), null, "constraint", null, 0, 1, ConstrainableElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(partyFunctionEClass, PartyFunction.class, "PartyFunction", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getPartyFunction_Type(), this.getPartyFunctionType(), "type", "Null", 1, 1, PartyFunction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getPartyFunction_Party(), this.getParty(), null, "party", null, 1, 1, PartyFunction.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(partyEClass, Party.class, "Party", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getParty_Uid(), theTypesPackage.getString(), "uid", null, 0, 1, Party.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getParty_Base_ActivityPartition(), theUMLPackage.getActivityPartition(), null, "base_ActivityPartition", null, 0, 1, Party.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(permissionEClass, Permission.class, "Permission", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getPermission_Duties(), this.getDuty(), null, "duties", null, 0, -1, Permission.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(dutyEClass, Duty.class, "Duty", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getDuty_Consequences(), this.getDuty(), null, "consequences", null, 0, -1, Duty.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(prohibitionEClass, Prohibition.class, "Prohibition", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getProhibition_Remedies(), this.getDuty(), null, "remedies", null, 0, -1, Prohibition.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(assetEClass, Asset.class, "Asset", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getAsset_Base_Pin(), theUMLPackage.getPin(), null, "base_Pin", null, 0, 1, Asset.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEAttribute(getAsset_Uid(), theTypesPackage.getString(), "uid", null, 0, 1, Asset.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(assetCollectionEClass, AssetCollection.class, "AssetCollection", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getAssetCollection_Source(), theTypesPackage.getString(), "source", null, 0, 1, AssetCollection.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		initEClass(partyCollectionEClass, PartyCollection.class, "PartyCollection", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getPartyCollection_Source(), theTypesPackage.getString(), "source", null, 0, 1, PartyCollection.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(conflictStrategyEEnum, ConflictStrategy.class, "ConflictStrategy");
		addEEnumLiteral(conflictStrategyEEnum, ConflictStrategy.NULL);
		addEEnumLiteral(conflictStrategyEEnum, ConflictStrategy.PERMIT);
		addEEnumLiteral(conflictStrategyEEnum, ConflictStrategy.PROHIBIT);
		addEEnumLiteral(conflictStrategyEEnum, ConflictStrategy.VOID_POLICY);

		initEEnum(policyTypeEEnum, PolicyType.class, "PolicyType");
		addEEnumLiteral(policyTypeEEnum, PolicyType.NULL);
		addEEnumLiteral(policyTypeEEnum, PolicyType.AGREEMENT);
		addEEnumLiteral(policyTypeEEnum, PolicyType.ASSERTION);
		addEEnumLiteral(policyTypeEEnum, PolicyType.OFFER);
		addEEnumLiteral(policyTypeEEnum, PolicyType.PRIVACY);
		addEEnumLiteral(policyTypeEEnum, PolicyType.REQUEST);
		addEEnumLiteral(policyTypeEEnum, PolicyType.SET);
		addEEnumLiteral(policyTypeEEnum, PolicyType.TICKET);

		initEEnum(logicalOperatorEEnum, LogicalOperator.class, "LogicalOperator");
		addEEnumLiteral(logicalOperatorEEnum, LogicalOperator.NULL);
		addEEnumLiteral(logicalOperatorEEnum, LogicalOperator.AND);
		addEEnumLiteral(logicalOperatorEEnum, LogicalOperator.AND_SEQUENCE);
		addEEnumLiteral(logicalOperatorEEnum, LogicalOperator.OR);
		addEEnumLiteral(logicalOperatorEEnum, LogicalOperator.XONE);

		initEEnum(leftOperandEEnum, LeftOperand.class, "LeftOperand");
		addEEnumLiteral(leftOperandEEnum, LeftOperand.NULL);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.ABSOLUTE_SIZE);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.ABSOLUTE_SPATIAL_POSITION);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.ABSOLUTE_TEMPORAL_POSITION);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.COUNT);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.DATE_TIME);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.DELAY_PERIOD);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.DELIVERY_CHANNEL);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.DEVICE);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.ELAPSED_TIME);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.EVENT);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.FILE_FORMAT);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.INDUSTRY);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.LANGUAGE);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.MEDIA);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.METERED_TIME);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.PAY_AMOUNT);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.PERCENTAGE);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.PRODUCT);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.PURPOSE);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.RECIPIENT);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.RELATIVE_POSITION);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.RELATIVE_SIZE);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.RELATIVE_SPATIAL_POSITION);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.RELATIVE_TEMPORAL_POSITION);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.RESOLUTION);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.SPATIAL);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.SPATIAL_COORDINATES);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.SYSTEM);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.SYSTEM_DEVICE);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.TIME_INTERVAL);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.UNIT_OF_COUNT);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.VERSION);
		addEEnumLiteral(leftOperandEEnum, LeftOperand.VIRTUAL_LOCATION);

		initEEnum(constraintOperatorEEnum, ConstraintOperator.class, "ConstraintOperator");
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.NULL);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.EQ);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.GT);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.GTEQ);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.HAS_PART);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.IS_A);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.IS_ALL_OF);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.IS_ANY_OF);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.IS_NONE_OF);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.IS_PART_OF);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.LT);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.LTEQ);
		addEEnumLiteral(constraintOperatorEEnum, ConstraintOperator.NEQ);

		initEEnum(partyFunctionTypeEEnum, PartyFunctionType.class, "PartyFunctionType");
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.NULL);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.ASSIGNEE);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.ASSIGNER);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.ATTRIBUTED_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.ATTRIBUTING_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.COMPENSATED_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.COMPENSATING_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.CONSENTED_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.CONSENTING_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.CONTRACTED_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.CONTRACTING_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.INFORMED_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.INFORMING_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.TRACKED_PARTY);
		addEEnumLiteral(partyFunctionTypeEEnum, PartyFunctionType.TRACKING_PARTY);

		initEEnum(actionEEnum, Action.class, "Action");
		addEEnumLiteral(actionEEnum, Action.NULL);
		addEEnumLiteral(actionEEnum, Action.CC_ATTRIBUTION);
		addEEnumLiteral(actionEEnum, Action.CC_COMMERCIAL_USE);
		addEEnumLiteral(actionEEnum, Action.CC_DERIVATIVE_WORKS);
		addEEnumLiteral(actionEEnum, Action.CC_DISTRIBUTION);
		addEEnumLiteral(actionEEnum, Action.CC_NOTICE);
		addEEnumLiteral(actionEEnum, Action.CC_REPRODUCTION);
		addEEnumLiteral(actionEEnum, Action.CC_SHARE_ALIKE);
		addEEnumLiteral(actionEEnum, Action.CC_SHARING);
		addEEnumLiteral(actionEEnum, Action.CC_SOURCE_CODE);
		addEEnumLiteral(actionEEnum, Action.ACCEPT_TRACKING);
		addEEnumLiteral(actionEEnum, Action.AGGREGATE);
		addEEnumLiteral(actionEEnum, Action.ANNOTATE);
		addEEnumLiteral(actionEEnum, Action.ANONYMIZE);
		addEEnumLiteral(actionEEnum, Action.ARCHIVE);
		addEEnumLiteral(actionEEnum, Action.ATTRIBUTE);
		addEEnumLiteral(actionEEnum, Action.COMPENSATE);
		addEEnumLiteral(actionEEnum, Action.CONCURRENT_USE);
		addEEnumLiteral(actionEEnum, Action.DELETE);
		addEEnumLiteral(actionEEnum, Action.DERIVE);
		addEEnumLiteral(actionEEnum, Action.DIGITIZE);
		addEEnumLiteral(actionEEnum, Action.DISPLAY);
		addEEnumLiteral(actionEEnum, Action.DISTRIBUTE);
		addEEnumLiteral(actionEEnum, Action.ENSURE_EXCLUSIVITY);
		addEEnumLiteral(actionEEnum, Action.EXECUTE);
		addEEnumLiteral(actionEEnum, Action.EXTRACT);
		addEEnumLiteral(actionEEnum, Action.GIVE);
		addEEnumLiteral(actionEEnum, Action.GRANT_USE);
		addEEnumLiteral(actionEEnum, Action.INCLUDE);
		addEEnumLiteral(actionEEnum, Action.INDEX);
		addEEnumLiteral(actionEEnum, Action.INFORM);
		addEEnumLiteral(actionEEnum, Action.INSTALL);
		addEEnumLiteral(actionEEnum, Action.MODIFY);
		addEEnumLiteral(actionEEnum, Action.MOVE);
		addEEnumLiteral(actionEEnum, Action.NEXT_POLICY);
		addEEnumLiteral(actionEEnum, Action.OBTAIN_CONSENT);
		addEEnumLiteral(actionEEnum, Action.PLAY);
		addEEnumLiteral(actionEEnum, Action.PRESENT);
		addEEnumLiteral(actionEEnum, Action.PRINT);
		addEEnumLiteral(actionEEnum, Action.READ);
		addEEnumLiteral(actionEEnum, Action.REPRODUCE);
		addEEnumLiteral(actionEEnum, Action.REVIEW_POLICY);
		addEEnumLiteral(actionEEnum, Action.SELL);
		addEEnumLiteral(actionEEnum, Action.STREAM);
		addEEnumLiteral(actionEEnum, Action.SYNCHRONIZE);
		addEEnumLiteral(actionEEnum, Action.TEXT_TO_SPEECH);
		addEEnumLiteral(actionEEnum, Action.TRANSFER);
		addEEnumLiteral(actionEEnum, Action.TRANSFORM);
		addEEnumLiteral(actionEEnum, Action.TRANSLATE);
		addEEnumLiteral(actionEEnum, Action.UNINSTALL);
		addEEnumLiteral(actionEEnum, Action.USE);
		addEEnumLiteral(actionEEnum, Action.WATERMARK);

		// Create resource
		createResource(eNS_URI);

		// Create annotations
		// http://www.eclipse.org/uml2/2.0.0/UML
		createUMLAnnotations();
	}

	/**
	 * Initializes the annotations for <b>http://www.eclipse.org/uml2/2.0.0/UML</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void createUMLAnnotations() {
		String source = "http://www.eclipse.org/uml2/2.0.0/UML";
		addAnnotation
		  (odrlPolicyEClass,
		   source,
		   new String[] {
			   "originalName", "ODRL-Policy"
		   });
	}

} //ODRLCommonVocabularyPackageImpl
