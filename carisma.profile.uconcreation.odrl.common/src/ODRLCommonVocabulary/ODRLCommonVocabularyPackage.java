/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyFactory
 * @model kind="package"
 * @generated
 */
public interface ODRLCommonVocabularyPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "ODRLCommonVocabulary";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http:///ODRLCommonVocabulary.ecore";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "ODRLCommonVocabulary";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	ODRLCommonVocabularyPackage eINSTANCE = ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl.init();

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.ODRLPolicyImpl <em>ODRL Policy</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.ODRLPolicyImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getODRLPolicy()
	 * @generated
	 */
	int ODRL_POLICY = 0;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ODRL_POLICY__UID = 0;

	/**
	 * The feature id for the '<em><b>Base Activity</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ODRL_POLICY__BASE_ACTIVITY = 1;

	/**
	 * The feature id for the '<em><b>Conflict Strategy</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ODRL_POLICY__CONFLICT_STRATEGY = 2;

	/**
	 * The feature id for the '<em><b>Policy Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ODRL_POLICY__POLICY_TYPE = 3;

	/**
	 * The feature id for the '<em><b>Profiles</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ODRL_POLICY__PROFILES = 4;

	/**
	 * The feature id for the '<em><b>Inherits From</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ODRL_POLICY__INHERITS_FROM = 5;

	/**
	 * The number of structural features of the '<em>ODRL Policy</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ODRL_POLICY_FEATURE_COUNT = 6;

	/**
	 * The number of operations of the '<em>ODRL Policy</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ODRL_POLICY_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.ConstrainableElementImpl <em>Constrainable Element</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.ConstrainableElementImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getConstrainableElement()
	 * @generated
	 */
	int CONSTRAINABLE_ELEMENT = 5;

	/**
	 * The feature id for the '<em><b>Constraint</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINABLE_ELEMENT__CONSTRAINT = 0;

	/**
	 * The number of structural features of the '<em>Constrainable Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINABLE_ELEMENT_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Constrainable Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINABLE_ELEMENT_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.RuleImpl <em>Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.RuleImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getRule()
	 * @generated
	 */
	int RULE = 1;

	/**
	 * The feature id for the '<em><b>Constraint</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE__CONSTRAINT = CONSTRAINABLE_ELEMENT__CONSTRAINT;

	/**
	 * The feature id for the '<em><b>Refinement</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE__REFINEMENT = CONSTRAINABLE_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE__BASE_ACTION = CONSTRAINABLE_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE__UID = CONSTRAINABLE_ELEMENT_FEATURE_COUNT + 2;

	/**
	 * The feature id for the '<em><b>Involved Parties</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE__INVOLVED_PARTIES = CONSTRAINABLE_ELEMENT_FEATURE_COUNT + 3;

	/**
	 * The feature id for the '<em><b>Involved Assets</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE__INVOLVED_ASSETS = CONSTRAINABLE_ELEMENT_FEATURE_COUNT + 4;

	/**
	 * The feature id for the '<em><b>Action</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE__ACTION = CONSTRAINABLE_ELEMENT_FEATURE_COUNT + 5;

	/**
	 * The number of structural features of the '<em>Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_FEATURE_COUNT = CONSTRAINABLE_ELEMENT_FEATURE_COUNT + 6;

	/**
	 * The number of operations of the '<em>Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_OPERATION_COUNT = CONSTRAINABLE_ELEMENT_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.RefinableElementImpl <em>Refinable Element</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.RefinableElementImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getRefinableElement()
	 * @generated
	 */
	int REFINABLE_ELEMENT = 2;

	/**
	 * The feature id for the '<em><b>Refinement</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFINABLE_ELEMENT__REFINEMENT = 0;

	/**
	 * The number of structural features of the '<em>Refinable Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFINABLE_ELEMENT_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Refinable Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REFINABLE_ELEMENT_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.LogicalConstraintImpl <em>Logical Constraint</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.LogicalConstraintImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getLogicalConstraint()
	 * @generated
	 */
	int LOGICAL_CONSTRAINT = 3;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOGICAL_CONSTRAINT__UID = 0;

	/**
	 * The feature id for the '<em><b>Logical Operator</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOGICAL_CONSTRAINT__LOGICAL_OPERATOR = 1;

	/**
	 * The feature id for the '<em><b>Constraints</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOGICAL_CONSTRAINT__CONSTRAINTS = 2;

	/**
	 * The number of structural features of the '<em>Logical Constraint</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOGICAL_CONSTRAINT_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>Logical Constraint</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOGICAL_CONSTRAINT_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.ConstraintImpl <em>Constraint</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.ConstraintImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getConstraint()
	 * @generated
	 */
	int CONSTRAINT = 4;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINT__UID = 0;

	/**
	 * The feature id for the '<em><b>Left Operand</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINT__LEFT_OPERAND = 1;

	/**
	 * The feature id for the '<em><b>Status</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINT__STATUS = 2;

	/**
	 * The feature id for the '<em><b>Operator</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINT__OPERATOR = 3;

	/**
	 * The feature id for the '<em><b>Right Operand</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINT__RIGHT_OPERAND = 4;

	/**
	 * The feature id for the '<em><b>Right Operand Reference</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINT__RIGHT_OPERAND_REFERENCE = 5;

	/**
	 * The feature id for the '<em><b>Data Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINT__DATA_TYPE = 6;

	/**
	 * The feature id for the '<em><b>Unit</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINT__UNIT = 7;

	/**
	 * The number of structural features of the '<em>Constraint</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINT_FEATURE_COUNT = 8;

	/**
	 * The number of operations of the '<em>Constraint</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSTRAINT_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.PartyFunctionImpl <em>Party Function</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.PartyFunctionImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getPartyFunction()
	 * @generated
	 */
	int PARTY_FUNCTION = 6;

	/**
	 * The feature id for the '<em><b>Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_FUNCTION__TYPE = 0;

	/**
	 * The feature id for the '<em><b>Party</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_FUNCTION__PARTY = 1;

	/**
	 * The number of structural features of the '<em>Party Function</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_FUNCTION_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>Party Function</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_FUNCTION_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.PartyImpl <em>Party</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.PartyImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getParty()
	 * @generated
	 */
	int PARTY = 7;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY__UID = 0;

	/**
	 * The feature id for the '<em><b>Base Activity Partition</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY__BASE_ACTIVITY_PARTITION = 1;

	/**
	 * The feature id for the '<em><b>Base Data Store Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY__BASE_DATA_STORE_NODE = 2;

	/**
	 * The number of structural features of the '<em>Party</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>Party</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.PermissionImpl <em>Permission</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.PermissionImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getPermission()
	 * @generated
	 */
	int PERMISSION = 8;

	/**
	 * The feature id for the '<em><b>Constraint</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERMISSION__CONSTRAINT = RULE__CONSTRAINT;

	/**
	 * The feature id for the '<em><b>Refinement</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERMISSION__REFINEMENT = RULE__REFINEMENT;

	/**
	 * The feature id for the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERMISSION__BASE_ACTION = RULE__BASE_ACTION;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERMISSION__UID = RULE__UID;

	/**
	 * The feature id for the '<em><b>Involved Parties</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERMISSION__INVOLVED_PARTIES = RULE__INVOLVED_PARTIES;

	/**
	 * The feature id for the '<em><b>Involved Assets</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERMISSION__INVOLVED_ASSETS = RULE__INVOLVED_ASSETS;

	/**
	 * The feature id for the '<em><b>Action</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERMISSION__ACTION = RULE__ACTION;

	/**
	 * The feature id for the '<em><b>Duties</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERMISSION__DUTIES = RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Permission</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERMISSION_FEATURE_COUNT = RULE_FEATURE_COUNT + 1;

	/**
	 * The number of operations of the '<em>Permission</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERMISSION_OPERATION_COUNT = RULE_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.DutyImpl <em>Duty</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.DutyImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getDuty()
	 * @generated
	 */
	int DUTY = 9;

	/**
	 * The feature id for the '<em><b>Constraint</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DUTY__CONSTRAINT = RULE__CONSTRAINT;

	/**
	 * The feature id for the '<em><b>Refinement</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DUTY__REFINEMENT = RULE__REFINEMENT;

	/**
	 * The feature id for the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DUTY__BASE_ACTION = RULE__BASE_ACTION;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DUTY__UID = RULE__UID;

	/**
	 * The feature id for the '<em><b>Involved Parties</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DUTY__INVOLVED_PARTIES = RULE__INVOLVED_PARTIES;

	/**
	 * The feature id for the '<em><b>Involved Assets</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DUTY__INVOLVED_ASSETS = RULE__INVOLVED_ASSETS;

	/**
	 * The feature id for the '<em><b>Action</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DUTY__ACTION = RULE__ACTION;

	/**
	 * The feature id for the '<em><b>Consequences</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DUTY__CONSEQUENCES = RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Duty</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DUTY_FEATURE_COUNT = RULE_FEATURE_COUNT + 1;

	/**
	 * The number of operations of the '<em>Duty</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DUTY_OPERATION_COUNT = RULE_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.ProhibitionImpl <em>Prohibition</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.ProhibitionImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getProhibition()
	 * @generated
	 */
	int PROHIBITION = 10;

	/**
	 * The feature id for the '<em><b>Constraint</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROHIBITION__CONSTRAINT = RULE__CONSTRAINT;

	/**
	 * The feature id for the '<em><b>Refinement</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROHIBITION__REFINEMENT = RULE__REFINEMENT;

	/**
	 * The feature id for the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROHIBITION__BASE_ACTION = RULE__BASE_ACTION;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROHIBITION__UID = RULE__UID;

	/**
	 * The feature id for the '<em><b>Involved Parties</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROHIBITION__INVOLVED_PARTIES = RULE__INVOLVED_PARTIES;

	/**
	 * The feature id for the '<em><b>Involved Assets</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROHIBITION__INVOLVED_ASSETS = RULE__INVOLVED_ASSETS;

	/**
	 * The feature id for the '<em><b>Action</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROHIBITION__ACTION = RULE__ACTION;

	/**
	 * The feature id for the '<em><b>Remedies</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROHIBITION__REMEDIES = RULE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Prohibition</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROHIBITION_FEATURE_COUNT = RULE_FEATURE_COUNT + 1;

	/**
	 * The number of operations of the '<em>Prohibition</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROHIBITION_OPERATION_COUNT = RULE_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.AssetImpl <em>Asset</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.AssetImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getAsset()
	 * @generated
	 */
	int ASSET = 11;

	/**
	 * The feature id for the '<em><b>Base Data Store Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET__BASE_DATA_STORE_NODE = 0;

	/**
	 * The feature id for the '<em><b>Base Pin</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET__BASE_PIN = 1;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET__UID = 2;

	/**
	 * The number of structural features of the '<em>Asset</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>Asset</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.AssetCollectionImpl <em>Asset Collection</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.AssetCollectionImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getAssetCollection()
	 * @generated
	 */
	int ASSET_COLLECTION = 12;

	/**
	 * The feature id for the '<em><b>Base Data Store Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_COLLECTION__BASE_DATA_STORE_NODE = ASSET__BASE_DATA_STORE_NODE;

	/**
	 * The feature id for the '<em><b>Base Pin</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_COLLECTION__BASE_PIN = ASSET__BASE_PIN;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_COLLECTION__UID = ASSET__UID;

	/**
	 * The feature id for the '<em><b>Refinement</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_COLLECTION__REFINEMENT = ASSET_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Source</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_COLLECTION__SOURCE = ASSET_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Asset Collection</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_COLLECTION_FEATURE_COUNT = ASSET_FEATURE_COUNT + 2;

	/**
	 * The number of operations of the '<em>Asset Collection</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_COLLECTION_OPERATION_COUNT = ASSET_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.PartyCollectionImpl <em>Party Collection</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.PartyCollectionImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getPartyCollection()
	 * @generated
	 */
	int PARTY_COLLECTION = 13;

	/**
	 * The feature id for the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_COLLECTION__UID = PARTY__UID;

	/**
	 * The feature id for the '<em><b>Base Activity Partition</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_COLLECTION__BASE_ACTIVITY_PARTITION = PARTY__BASE_ACTIVITY_PARTITION;

	/**
	 * The feature id for the '<em><b>Base Data Store Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_COLLECTION__BASE_DATA_STORE_NODE = PARTY__BASE_DATA_STORE_NODE;

	/**
	 * The feature id for the '<em><b>Refinement</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_COLLECTION__REFINEMENT = PARTY_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Source</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_COLLECTION__SOURCE = PARTY_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Party Collection</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_COLLECTION_FEATURE_COUNT = PARTY_FEATURE_COUNT + 2;

	/**
	 * The number of operations of the '<em>Party Collection</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PARTY_COLLECTION_OPERATION_COUNT = PARTY_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.impl.AssetRelationImpl <em>Asset Relation</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.impl.AssetRelationImpl
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getAssetRelation()
	 * @generated
	 */
	int ASSET_RELATION = 14;

	/**
	 * The feature id for the '<em><b>Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_RELATION__TYPE = 0;

	/**
	 * The feature id for the '<em><b>Asset</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_RELATION__ASSET = 1;

	/**
	 * The number of structural features of the '<em>Asset Relation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_RELATION_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>Asset Relation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ASSET_RELATION_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.ConflictStrategy <em>Conflict Strategy</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.ConflictStrategy
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getConflictStrategy()
	 * @generated
	 */
	int CONFLICT_STRATEGY = 15;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.PolicyType <em>Policy Type</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.PolicyType
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getPolicyType()
	 * @generated
	 */
	int POLICY_TYPE = 16;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.LogicalOperator <em>Logical Operator</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.LogicalOperator
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getLogicalOperator()
	 * @generated
	 */
	int LOGICAL_OPERATOR = 17;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.LeftOperand <em>Left Operand</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.LeftOperand
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getLeftOperand()
	 * @generated
	 */
	int LEFT_OPERAND = 18;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.ConstraintOperator <em>Constraint Operator</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.ConstraintOperator
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getConstraintOperator()
	 * @generated
	 */
	int CONSTRAINT_OPERATOR = 19;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.PartyFunctionType <em>Party Function Type</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.PartyFunctionType
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getPartyFunctionType()
	 * @generated
	 */
	int PARTY_FUNCTION_TYPE = 20;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.AssetRelationType <em>Asset Relation Type</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.AssetRelationType
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getAssetRelationType()
	 * @generated
	 */
	int ASSET_RELATION_TYPE = 21;

	/**
	 * The meta object id for the '{@link ODRLCommonVocabulary.Action <em>Action</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see ODRLCommonVocabulary.Action
	 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getAction()
	 * @generated
	 */
	int ACTION = 22;


	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.ODRLPolicy <em>ODRL Policy</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>ODRL Policy</em>'.
	 * @see ODRLCommonVocabulary.ODRLPolicy
	 * @generated
	 */
	EClass getODRLPolicy();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.ODRLPolicy#getUid <em>Uid</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Uid</em>'.
	 * @see ODRLCommonVocabulary.ODRLPolicy#getUid()
	 * @see #getODRLPolicy()
	 * @generated
	 */
	EAttribute getODRLPolicy_Uid();

	/**
	 * Returns the meta object for the reference '{@link ODRLCommonVocabulary.ODRLPolicy#getBase_Activity <em>Base Activity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Activity</em>'.
	 * @see ODRLCommonVocabulary.ODRLPolicy#getBase_Activity()
	 * @see #getODRLPolicy()
	 * @generated
	 */
	EReference getODRLPolicy_Base_Activity();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.ODRLPolicy#getConflictStrategy <em>Conflict Strategy</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Conflict Strategy</em>'.
	 * @see ODRLCommonVocabulary.ODRLPolicy#getConflictStrategy()
	 * @see #getODRLPolicy()
	 * @generated
	 */
	EAttribute getODRLPolicy_ConflictStrategy();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.ODRLPolicy#getPolicyType <em>Policy Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Policy Type</em>'.
	 * @see ODRLCommonVocabulary.ODRLPolicy#getPolicyType()
	 * @see #getODRLPolicy()
	 * @generated
	 */
	EAttribute getODRLPolicy_PolicyType();

	/**
	 * Returns the meta object for the attribute list '{@link ODRLCommonVocabulary.ODRLPolicy#getProfiles <em>Profiles</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Profiles</em>'.
	 * @see ODRLCommonVocabulary.ODRLPolicy#getProfiles()
	 * @see #getODRLPolicy()
	 * @generated
	 */
	EAttribute getODRLPolicy_Profiles();

	/**
	 * Returns the meta object for the attribute list '{@link ODRLCommonVocabulary.ODRLPolicy#getInheritsFrom <em>Inherits From</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Inherits From</em>'.
	 * @see ODRLCommonVocabulary.ODRLPolicy#getInheritsFrom()
	 * @see #getODRLPolicy()
	 * @generated
	 */
	EAttribute getODRLPolicy_InheritsFrom();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.Rule <em>Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Rule</em>'.
	 * @see ODRLCommonVocabulary.Rule
	 * @generated
	 */
	EClass getRule();

	/**
	 * Returns the meta object for the reference '{@link ODRLCommonVocabulary.Rule#getBase_Action <em>Base Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Action</em>'.
	 * @see ODRLCommonVocabulary.Rule#getBase_Action()
	 * @see #getRule()
	 * @generated
	 */
	EReference getRule_Base_Action();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.Rule#getUid <em>Uid</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Uid</em>'.
	 * @see ODRLCommonVocabulary.Rule#getUid()
	 * @see #getRule()
	 * @generated
	 */
	EAttribute getRule_Uid();

	/**
	 * Returns the meta object for the containment reference list '{@link ODRLCommonVocabulary.Rule#getInvolvedParties <em>Involved Parties</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Involved Parties</em>'.
	 * @see ODRLCommonVocabulary.Rule#getInvolvedParties()
	 * @see #getRule()
	 * @generated
	 */
	EReference getRule_InvolvedParties();

	/**
	 * Returns the meta object for the attribute list '{@link ODRLCommonVocabulary.Rule#getInvolvedAssets <em>Involved Assets</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Involved Assets</em>'.
	 * @see ODRLCommonVocabulary.Rule#getInvolvedAssets()
	 * @see #getRule()
	 * @generated
	 */
	EAttribute getRule_InvolvedAssets();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.Rule#getAction <em>Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Action</em>'.
	 * @see ODRLCommonVocabulary.Rule#getAction()
	 * @see #getRule()
	 * @generated
	 */
	EAttribute getRule_Action();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.RefinableElement <em>Refinable Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Refinable Element</em>'.
	 * @see ODRLCommonVocabulary.RefinableElement
	 * @generated
	 */
	EClass getRefinableElement();

	/**
	 * Returns the meta object for the containment reference '{@link ODRLCommonVocabulary.RefinableElement#getRefinement <em>Refinement</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Refinement</em>'.
	 * @see ODRLCommonVocabulary.RefinableElement#getRefinement()
	 * @see #getRefinableElement()
	 * @generated
	 */
	EReference getRefinableElement_Refinement();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.LogicalConstraint <em>Logical Constraint</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Logical Constraint</em>'.
	 * @see ODRLCommonVocabulary.LogicalConstraint
	 * @generated
	 */
	EClass getLogicalConstraint();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.LogicalConstraint#getUid <em>Uid</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Uid</em>'.
	 * @see ODRLCommonVocabulary.LogicalConstraint#getUid()
	 * @see #getLogicalConstraint()
	 * @generated
	 */
	EAttribute getLogicalConstraint_Uid();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.LogicalConstraint#getLogicalOperator <em>Logical Operator</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Logical Operator</em>'.
	 * @see ODRLCommonVocabulary.LogicalConstraint#getLogicalOperator()
	 * @see #getLogicalConstraint()
	 * @generated
	 */
	EAttribute getLogicalConstraint_LogicalOperator();

	/**
	 * Returns the meta object for the containment reference list '{@link ODRLCommonVocabulary.LogicalConstraint#getConstraints <em>Constraints</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Constraints</em>'.
	 * @see ODRLCommonVocabulary.LogicalConstraint#getConstraints()
	 * @see #getLogicalConstraint()
	 * @generated
	 */
	EReference getLogicalConstraint_Constraints();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.Constraint <em>Constraint</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Constraint</em>'.
	 * @see ODRLCommonVocabulary.Constraint
	 * @generated
	 */
	EClass getConstraint();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.Constraint#getUid <em>Uid</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Uid</em>'.
	 * @see ODRLCommonVocabulary.Constraint#getUid()
	 * @see #getConstraint()
	 * @generated
	 */
	EAttribute getConstraint_Uid();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.Constraint#getLeftOperand <em>Left Operand</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Left Operand</em>'.
	 * @see ODRLCommonVocabulary.Constraint#getLeftOperand()
	 * @see #getConstraint()
	 * @generated
	 */
	EAttribute getConstraint_LeftOperand();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.Constraint#getStatus <em>Status</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Status</em>'.
	 * @see ODRLCommonVocabulary.Constraint#getStatus()
	 * @see #getConstraint()
	 * @generated
	 */
	EAttribute getConstraint_Status();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.Constraint#getOperator <em>Operator</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Operator</em>'.
	 * @see ODRLCommonVocabulary.Constraint#getOperator()
	 * @see #getConstraint()
	 * @generated
	 */
	EAttribute getConstraint_Operator();

	/**
	 * Returns the meta object for the attribute list '{@link ODRLCommonVocabulary.Constraint#getRightOperand <em>Right Operand</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Right Operand</em>'.
	 * @see ODRLCommonVocabulary.Constraint#getRightOperand()
	 * @see #getConstraint()
	 * @generated
	 */
	EAttribute getConstraint_RightOperand();

	/**
	 * Returns the meta object for the attribute list '{@link ODRLCommonVocabulary.Constraint#getRightOperandReference <em>Right Operand Reference</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Right Operand Reference</em>'.
	 * @see ODRLCommonVocabulary.Constraint#getRightOperandReference()
	 * @see #getConstraint()
	 * @generated
	 */
	EAttribute getConstraint_RightOperandReference();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.Constraint#getDataType <em>Data Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Data Type</em>'.
	 * @see ODRLCommonVocabulary.Constraint#getDataType()
	 * @see #getConstraint()
	 * @generated
	 */
	EAttribute getConstraint_DataType();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.Constraint#getUnit <em>Unit</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Unit</em>'.
	 * @see ODRLCommonVocabulary.Constraint#getUnit()
	 * @see #getConstraint()
	 * @generated
	 */
	EAttribute getConstraint_Unit();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.ConstrainableElement <em>Constrainable Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Constrainable Element</em>'.
	 * @see ODRLCommonVocabulary.ConstrainableElement
	 * @generated
	 */
	EClass getConstrainableElement();

	/**
	 * Returns the meta object for the containment reference '{@link ODRLCommonVocabulary.ConstrainableElement#getConstraint <em>Constraint</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Constraint</em>'.
	 * @see ODRLCommonVocabulary.ConstrainableElement#getConstraint()
	 * @see #getConstrainableElement()
	 * @generated
	 */
	EReference getConstrainableElement_Constraint();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.PartyFunction <em>Party Function</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Party Function</em>'.
	 * @see ODRLCommonVocabulary.PartyFunction
	 * @generated
	 */
	EClass getPartyFunction();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.PartyFunction#getType <em>Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Type</em>'.
	 * @see ODRLCommonVocabulary.PartyFunction#getType()
	 * @see #getPartyFunction()
	 * @generated
	 */
	EAttribute getPartyFunction_Type();

	/**
	 * Returns the meta object for the reference '{@link ODRLCommonVocabulary.PartyFunction#getParty <em>Party</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Party</em>'.
	 * @see ODRLCommonVocabulary.PartyFunction#getParty()
	 * @see #getPartyFunction()
	 * @generated
	 */
	EReference getPartyFunction_Party();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.Party <em>Party</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Party</em>'.
	 * @see ODRLCommonVocabulary.Party
	 * @generated
	 */
	EClass getParty();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.Party#getUid <em>Uid</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Uid</em>'.
	 * @see ODRLCommonVocabulary.Party#getUid()
	 * @see #getParty()
	 * @generated
	 */
	EAttribute getParty_Uid();

	/**
	 * Returns the meta object for the reference '{@link ODRLCommonVocabulary.Party#getBase_ActivityPartition <em>Base Activity Partition</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Activity Partition</em>'.
	 * @see ODRLCommonVocabulary.Party#getBase_ActivityPartition()
	 * @see #getParty()
	 * @generated
	 */
	EReference getParty_Base_ActivityPartition();

	/**
	 * Returns the meta object for the reference '{@link ODRLCommonVocabulary.Party#getBase_DataStoreNode <em>Base Data Store Node</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Data Store Node</em>'.
	 * @see ODRLCommonVocabulary.Party#getBase_DataStoreNode()
	 * @see #getParty()
	 * @generated
	 */
	EReference getParty_Base_DataStoreNode();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.Permission <em>Permission</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Permission</em>'.
	 * @see ODRLCommonVocabulary.Permission
	 * @generated
	 */
	EClass getPermission();

	/**
	 * Returns the meta object for the reference list '{@link ODRLCommonVocabulary.Permission#getDuties <em>Duties</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Duties</em>'.
	 * @see ODRLCommonVocabulary.Permission#getDuties()
	 * @see #getPermission()
	 * @generated
	 */
	EReference getPermission_Duties();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.Duty <em>Duty</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Duty</em>'.
	 * @see ODRLCommonVocabulary.Duty
	 * @generated
	 */
	EClass getDuty();

	/**
	 * Returns the meta object for the reference list '{@link ODRLCommonVocabulary.Duty#getConsequences <em>Consequences</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Consequences</em>'.
	 * @see ODRLCommonVocabulary.Duty#getConsequences()
	 * @see #getDuty()
	 * @generated
	 */
	EReference getDuty_Consequences();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.Prohibition <em>Prohibition</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Prohibition</em>'.
	 * @see ODRLCommonVocabulary.Prohibition
	 * @generated
	 */
	EClass getProhibition();

	/**
	 * Returns the meta object for the reference list '{@link ODRLCommonVocabulary.Prohibition#getRemedies <em>Remedies</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Remedies</em>'.
	 * @see ODRLCommonVocabulary.Prohibition#getRemedies()
	 * @see #getProhibition()
	 * @generated
	 */
	EReference getProhibition_Remedies();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.Asset <em>Asset</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Asset</em>'.
	 * @see ODRLCommonVocabulary.Asset
	 * @generated
	 */
	EClass getAsset();

	/**
	 * Returns the meta object for the reference '{@link ODRLCommonVocabulary.Asset#getBase_DataStoreNode <em>Base Data Store Node</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Data Store Node</em>'.
	 * @see ODRLCommonVocabulary.Asset#getBase_DataStoreNode()
	 * @see #getAsset()
	 * @generated
	 */
	EReference getAsset_Base_DataStoreNode();

	/**
	 * Returns the meta object for the reference '{@link ODRLCommonVocabulary.Asset#getBase_Pin <em>Base Pin</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Pin</em>'.
	 * @see ODRLCommonVocabulary.Asset#getBase_Pin()
	 * @see #getAsset()
	 * @generated
	 */
	EReference getAsset_Base_Pin();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.Asset#getUid <em>Uid</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Uid</em>'.
	 * @see ODRLCommonVocabulary.Asset#getUid()
	 * @see #getAsset()
	 * @generated
	 */
	EAttribute getAsset_Uid();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.AssetCollection <em>Asset Collection</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Asset Collection</em>'.
	 * @see ODRLCommonVocabulary.AssetCollection
	 * @generated
	 */
	EClass getAssetCollection();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.AssetCollection#getSource <em>Source</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Source</em>'.
	 * @see ODRLCommonVocabulary.AssetCollection#getSource()
	 * @see #getAssetCollection()
	 * @generated
	 */
	EAttribute getAssetCollection_Source();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.PartyCollection <em>Party Collection</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Party Collection</em>'.
	 * @see ODRLCommonVocabulary.PartyCollection
	 * @generated
	 */
	EClass getPartyCollection();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.PartyCollection#getSource <em>Source</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Source</em>'.
	 * @see ODRLCommonVocabulary.PartyCollection#getSource()
	 * @see #getPartyCollection()
	 * @generated
	 */
	EAttribute getPartyCollection_Source();

	/**
	 * Returns the meta object for class '{@link ODRLCommonVocabulary.AssetRelation <em>Asset Relation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Asset Relation</em>'.
	 * @see ODRLCommonVocabulary.AssetRelation
	 * @generated
	 */
	EClass getAssetRelation();

	/**
	 * Returns the meta object for the attribute '{@link ODRLCommonVocabulary.AssetRelation#getType <em>Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Type</em>'.
	 * @see ODRLCommonVocabulary.AssetRelation#getType()
	 * @see #getAssetRelation()
	 * @generated
	 */
	EAttribute getAssetRelation_Type();

	/**
	 * Returns the meta object for the reference '{@link ODRLCommonVocabulary.AssetRelation#getAsset <em>Asset</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Asset</em>'.
	 * @see ODRLCommonVocabulary.AssetRelation#getAsset()
	 * @see #getAssetRelation()
	 * @generated
	 */
	EReference getAssetRelation_Asset();

	/**
	 * Returns the meta object for enum '{@link ODRLCommonVocabulary.ConflictStrategy <em>Conflict Strategy</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Conflict Strategy</em>'.
	 * @see ODRLCommonVocabulary.ConflictStrategy
	 * @generated
	 */
	EEnum getConflictStrategy();

	/**
	 * Returns the meta object for enum '{@link ODRLCommonVocabulary.PolicyType <em>Policy Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Policy Type</em>'.
	 * @see ODRLCommonVocabulary.PolicyType
	 * @generated
	 */
	EEnum getPolicyType();

	/**
	 * Returns the meta object for enum '{@link ODRLCommonVocabulary.LogicalOperator <em>Logical Operator</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Logical Operator</em>'.
	 * @see ODRLCommonVocabulary.LogicalOperator
	 * @generated
	 */
	EEnum getLogicalOperator();

	/**
	 * Returns the meta object for enum '{@link ODRLCommonVocabulary.LeftOperand <em>Left Operand</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Left Operand</em>'.
	 * @see ODRLCommonVocabulary.LeftOperand
	 * @generated
	 */
	EEnum getLeftOperand();

	/**
	 * Returns the meta object for enum '{@link ODRLCommonVocabulary.ConstraintOperator <em>Constraint Operator</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Constraint Operator</em>'.
	 * @see ODRLCommonVocabulary.ConstraintOperator
	 * @generated
	 */
	EEnum getConstraintOperator();

	/**
	 * Returns the meta object for enum '{@link ODRLCommonVocabulary.PartyFunctionType <em>Party Function Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Party Function Type</em>'.
	 * @see ODRLCommonVocabulary.PartyFunctionType
	 * @generated
	 */
	EEnum getPartyFunctionType();

	/**
	 * Returns the meta object for enum '{@link ODRLCommonVocabulary.AssetRelationType <em>Asset Relation Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Asset Relation Type</em>'.
	 * @see ODRLCommonVocabulary.AssetRelationType
	 * @generated
	 */
	EEnum getAssetRelationType();

	/**
	 * Returns the meta object for enum '{@link ODRLCommonVocabulary.Action <em>Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Action</em>'.
	 * @see ODRLCommonVocabulary.Action
	 * @generated
	 */
	EEnum getAction();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	ODRLCommonVocabularyFactory getODRLCommonVocabularyFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each operation of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.ODRLPolicyImpl <em>ODRL Policy</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.ODRLPolicyImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getODRLPolicy()
		 * @generated
		 */
		EClass ODRL_POLICY = eINSTANCE.getODRLPolicy();

		/**
		 * The meta object literal for the '<em><b>Uid</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ODRL_POLICY__UID = eINSTANCE.getODRLPolicy_Uid();

		/**
		 * The meta object literal for the '<em><b>Base Activity</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ODRL_POLICY__BASE_ACTIVITY = eINSTANCE.getODRLPolicy_Base_Activity();

		/**
		 * The meta object literal for the '<em><b>Conflict Strategy</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ODRL_POLICY__CONFLICT_STRATEGY = eINSTANCE.getODRLPolicy_ConflictStrategy();

		/**
		 * The meta object literal for the '<em><b>Policy Type</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ODRL_POLICY__POLICY_TYPE = eINSTANCE.getODRLPolicy_PolicyType();

		/**
		 * The meta object literal for the '<em><b>Profiles</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ODRL_POLICY__PROFILES = eINSTANCE.getODRLPolicy_Profiles();

		/**
		 * The meta object literal for the '<em><b>Inherits From</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ODRL_POLICY__INHERITS_FROM = eINSTANCE.getODRLPolicy_InheritsFrom();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.RuleImpl <em>Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.RuleImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getRule()
		 * @generated
		 */
		EClass RULE = eINSTANCE.getRule();

		/**
		 * The meta object literal for the '<em><b>Base Action</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RULE__BASE_ACTION = eINSTANCE.getRule_Base_Action();

		/**
		 * The meta object literal for the '<em><b>Uid</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RULE__UID = eINSTANCE.getRule_Uid();

		/**
		 * The meta object literal for the '<em><b>Involved Parties</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RULE__INVOLVED_PARTIES = eINSTANCE.getRule_InvolvedParties();

		/**
		 * The meta object literal for the '<em><b>Involved Assets</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RULE__INVOLVED_ASSETS = eINSTANCE.getRule_InvolvedAssets();

		/**
		 * The meta object literal for the '<em><b>Action</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RULE__ACTION = eINSTANCE.getRule_Action();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.RefinableElementImpl <em>Refinable Element</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.RefinableElementImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getRefinableElement()
		 * @generated
		 */
		EClass REFINABLE_ELEMENT = eINSTANCE.getRefinableElement();

		/**
		 * The meta object literal for the '<em><b>Refinement</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference REFINABLE_ELEMENT__REFINEMENT = eINSTANCE.getRefinableElement_Refinement();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.LogicalConstraintImpl <em>Logical Constraint</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.LogicalConstraintImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getLogicalConstraint()
		 * @generated
		 */
		EClass LOGICAL_CONSTRAINT = eINSTANCE.getLogicalConstraint();

		/**
		 * The meta object literal for the '<em><b>Uid</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LOGICAL_CONSTRAINT__UID = eINSTANCE.getLogicalConstraint_Uid();

		/**
		 * The meta object literal for the '<em><b>Logical Operator</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LOGICAL_CONSTRAINT__LOGICAL_OPERATOR = eINSTANCE.getLogicalConstraint_LogicalOperator();

		/**
		 * The meta object literal for the '<em><b>Constraints</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference LOGICAL_CONSTRAINT__CONSTRAINTS = eINSTANCE.getLogicalConstraint_Constraints();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.ConstraintImpl <em>Constraint</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.ConstraintImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getConstraint()
		 * @generated
		 */
		EClass CONSTRAINT = eINSTANCE.getConstraint();

		/**
		 * The meta object literal for the '<em><b>Uid</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONSTRAINT__UID = eINSTANCE.getConstraint_Uid();

		/**
		 * The meta object literal for the '<em><b>Left Operand</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONSTRAINT__LEFT_OPERAND = eINSTANCE.getConstraint_LeftOperand();

		/**
		 * The meta object literal for the '<em><b>Status</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONSTRAINT__STATUS = eINSTANCE.getConstraint_Status();

		/**
		 * The meta object literal for the '<em><b>Operator</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONSTRAINT__OPERATOR = eINSTANCE.getConstraint_Operator();

		/**
		 * The meta object literal for the '<em><b>Right Operand</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONSTRAINT__RIGHT_OPERAND = eINSTANCE.getConstraint_RightOperand();

		/**
		 * The meta object literal for the '<em><b>Right Operand Reference</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONSTRAINT__RIGHT_OPERAND_REFERENCE = eINSTANCE.getConstraint_RightOperandReference();

		/**
		 * The meta object literal for the '<em><b>Data Type</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONSTRAINT__DATA_TYPE = eINSTANCE.getConstraint_DataType();

		/**
		 * The meta object literal for the '<em><b>Unit</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CONSTRAINT__UNIT = eINSTANCE.getConstraint_Unit();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.ConstrainableElementImpl <em>Constrainable Element</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.ConstrainableElementImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getConstrainableElement()
		 * @generated
		 */
		EClass CONSTRAINABLE_ELEMENT = eINSTANCE.getConstrainableElement();

		/**
		 * The meta object literal for the '<em><b>Constraint</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONSTRAINABLE_ELEMENT__CONSTRAINT = eINSTANCE.getConstrainableElement_Constraint();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.PartyFunctionImpl <em>Party Function</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.PartyFunctionImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getPartyFunction()
		 * @generated
		 */
		EClass PARTY_FUNCTION = eINSTANCE.getPartyFunction();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PARTY_FUNCTION__TYPE = eINSTANCE.getPartyFunction_Type();

		/**
		 * The meta object literal for the '<em><b>Party</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PARTY_FUNCTION__PARTY = eINSTANCE.getPartyFunction_Party();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.PartyImpl <em>Party</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.PartyImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getParty()
		 * @generated
		 */
		EClass PARTY = eINSTANCE.getParty();

		/**
		 * The meta object literal for the '<em><b>Uid</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PARTY__UID = eINSTANCE.getParty_Uid();

		/**
		 * The meta object literal for the '<em><b>Base Activity Partition</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PARTY__BASE_ACTIVITY_PARTITION = eINSTANCE.getParty_Base_ActivityPartition();

		/**
		 * The meta object literal for the '<em><b>Base Data Store Node</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PARTY__BASE_DATA_STORE_NODE = eINSTANCE.getParty_Base_DataStoreNode();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.PermissionImpl <em>Permission</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.PermissionImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getPermission()
		 * @generated
		 */
		EClass PERMISSION = eINSTANCE.getPermission();

		/**
		 * The meta object literal for the '<em><b>Duties</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PERMISSION__DUTIES = eINSTANCE.getPermission_Duties();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.DutyImpl <em>Duty</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.DutyImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getDuty()
		 * @generated
		 */
		EClass DUTY = eINSTANCE.getDuty();

		/**
		 * The meta object literal for the '<em><b>Consequences</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DUTY__CONSEQUENCES = eINSTANCE.getDuty_Consequences();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.ProhibitionImpl <em>Prohibition</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.ProhibitionImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getProhibition()
		 * @generated
		 */
		EClass PROHIBITION = eINSTANCE.getProhibition();

		/**
		 * The meta object literal for the '<em><b>Remedies</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PROHIBITION__REMEDIES = eINSTANCE.getProhibition_Remedies();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.AssetImpl <em>Asset</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.AssetImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getAsset()
		 * @generated
		 */
		EClass ASSET = eINSTANCE.getAsset();

		/**
		 * The meta object literal for the '<em><b>Base Data Store Node</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ASSET__BASE_DATA_STORE_NODE = eINSTANCE.getAsset_Base_DataStoreNode();

		/**
		 * The meta object literal for the '<em><b>Base Pin</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ASSET__BASE_PIN = eINSTANCE.getAsset_Base_Pin();

		/**
		 * The meta object literal for the '<em><b>Uid</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ASSET__UID = eINSTANCE.getAsset_Uid();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.AssetCollectionImpl <em>Asset Collection</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.AssetCollectionImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getAssetCollection()
		 * @generated
		 */
		EClass ASSET_COLLECTION = eINSTANCE.getAssetCollection();

		/**
		 * The meta object literal for the '<em><b>Source</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ASSET_COLLECTION__SOURCE = eINSTANCE.getAssetCollection_Source();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.PartyCollectionImpl <em>Party Collection</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.PartyCollectionImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getPartyCollection()
		 * @generated
		 */
		EClass PARTY_COLLECTION = eINSTANCE.getPartyCollection();

		/**
		 * The meta object literal for the '<em><b>Source</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PARTY_COLLECTION__SOURCE = eINSTANCE.getPartyCollection_Source();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.impl.AssetRelationImpl <em>Asset Relation</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.impl.AssetRelationImpl
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getAssetRelation()
		 * @generated
		 */
		EClass ASSET_RELATION = eINSTANCE.getAssetRelation();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ASSET_RELATION__TYPE = eINSTANCE.getAssetRelation_Type();

		/**
		 * The meta object literal for the '<em><b>Asset</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ASSET_RELATION__ASSET = eINSTANCE.getAssetRelation_Asset();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.ConflictStrategy <em>Conflict Strategy</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.ConflictStrategy
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getConflictStrategy()
		 * @generated
		 */
		EEnum CONFLICT_STRATEGY = eINSTANCE.getConflictStrategy();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.PolicyType <em>Policy Type</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.PolicyType
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getPolicyType()
		 * @generated
		 */
		EEnum POLICY_TYPE = eINSTANCE.getPolicyType();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.LogicalOperator <em>Logical Operator</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.LogicalOperator
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getLogicalOperator()
		 * @generated
		 */
		EEnum LOGICAL_OPERATOR = eINSTANCE.getLogicalOperator();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.LeftOperand <em>Left Operand</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.LeftOperand
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getLeftOperand()
		 * @generated
		 */
		EEnum LEFT_OPERAND = eINSTANCE.getLeftOperand();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.ConstraintOperator <em>Constraint Operator</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.ConstraintOperator
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getConstraintOperator()
		 * @generated
		 */
		EEnum CONSTRAINT_OPERATOR = eINSTANCE.getConstraintOperator();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.PartyFunctionType <em>Party Function Type</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.PartyFunctionType
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getPartyFunctionType()
		 * @generated
		 */
		EEnum PARTY_FUNCTION_TYPE = eINSTANCE.getPartyFunctionType();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.AssetRelationType <em>Asset Relation Type</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.AssetRelationType
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getAssetRelationType()
		 * @generated
		 */
		EEnum ASSET_RELATION_TYPE = eINSTANCE.getAssetRelationType();

		/**
		 * The meta object literal for the '{@link ODRLCommonVocabulary.Action <em>Action</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see ODRLCommonVocabulary.Action
		 * @see ODRLCommonVocabulary.impl.ODRLCommonVocabularyPackageImpl#getAction()
		 * @generated
		 */
		EEnum ACTION = eINSTANCE.getAction();

	}

} //ODRLCommonVocabularyPackage
