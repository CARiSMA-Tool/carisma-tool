/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelFactory
 * @model kind="package"
 * @generated
 */
public interface DatamodelPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "carisma.regulatory.ruleallocator.datamodel";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://carisma.regulatory.ruleallocator.datamodel/1.0";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "carisma.regulatory.ruleallocator.datamodel";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	DatamodelPackage eINSTANCE = carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl.init();

	/**
	 * The meta object id for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementImpl <em>Rule Element</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.RuleElementImpl
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getRuleElement()
	 * @generated
	 */
	int RULE_ELEMENT = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT__NAME = 0;

	/**
	 * The feature id for the '<em><b>Type</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT__TYPE = 1;

	/**
	 * The feature id for the '<em><b>Belongs To Situation</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT__BELONGS_TO_SITUATION = 2;

	/**
	 * The number of structural features of the '<em>Rule Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_FEATURE_COUNT = 3;

	/**
	 * The meta object id for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementTypeImpl <em>Rule Element Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.RuleElementTypeImpl
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getRuleElementType()
	 * @generated
	 */
	int RULE_ELEMENT_TYPE = 1;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_TYPE__NAME = 0;

	/**
	 * The number of structural features of the '<em>Rule Element Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_TYPE_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.SituationImpl <em>Situation</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.SituationImpl
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getSituation()
	 * @generated
	 */
	int SITUATION = 2;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SITUATION__NAME = 0;

	/**
	 * The feature id for the '<em><b>Has</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SITUATION__HAS = 1;

	/**
	 * The number of structural features of the '<em>Situation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SITUATION_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.AllocationImpl <em>Allocation</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.AllocationImpl
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getAllocation()
	 * @generated
	 */
	int ALLOCATION = 3;

	/**
	 * The feature id for the '<em><b>Rule Element</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ALLOCATION__RULE_ELEMENT = 0;

	/**
	 * The feature id for the '<em><b>Bpmn Element</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ALLOCATION__BPMN_ELEMENT = 1;

	/**
	 * The number of structural features of the '<em>Allocation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ALLOCATION_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.BPMNElementImpl <em>BPMN Element</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.BPMNElementImpl
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getBPMNElement()
	 * @generated
	 */
	int BPMN_ELEMENT = 4;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BPMN_ELEMENT__NAME = 0;

	/**
	 * The feature id for the '<em><b>ID</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BPMN_ELEMENT__ID = 1;

	/**
	 * The feature id for the '<em><b>Type</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BPMN_ELEMENT__TYPE = 2;

	/**
	 * The feature id for the '<em><b>Incoming</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BPMN_ELEMENT__INCOMING = 3;

	/**
	 * The feature id for the '<em><b>Outgoing</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BPMN_ELEMENT__OUTGOING = 4;

	/**
	 * The feature id for the '<em><b>Process Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BPMN_ELEMENT__PROCESS_ID = 5;

	/**
	 * The number of structural features of the '<em>BPMN Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BPMN_ELEMENT_FEATURE_COUNT = 6;

	/**
	 * The meta object id for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationTypeImpl <em>Rule Element Assosiation Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationTypeImpl
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getRuleElementAssosiationType()
	 * @generated
	 */
	int RULE_ELEMENT_ASSOSIATION_TYPE = 5;

	/**
	 * The feature id for the '<em><b>Src</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_ASSOSIATION_TYPE__SRC = 0;

	/**
	 * The feature id for the '<em><b>Target</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_ASSOSIATION_TYPE__TARGET = 1;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_ASSOSIATION_TYPE__NAME = 2;

	/**
	 * The number of structural features of the '<em>Rule Element Assosiation Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_ASSOSIATION_TYPE_FEATURE_COUNT = 3;

	/**
	 * The meta object id for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationImpl <em>Rule Element Assosation</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationImpl
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getRuleElementAssosation()
	 * @generated
	 */
	int RULE_ELEMENT_ASSOSATION = 6;

	/**
	 * The feature id for the '<em><b>Src1</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_ASSOSATION__SRC1 = 0;

	/**
	 * The feature id for the '<em><b>Target1</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_ASSOSATION__TARGET1 = 1;

	/**
	 * The feature id for the '<em><b>Type1</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_ASSOSATION__TYPE1 = 2;

	/**
	 * The number of structural features of the '<em>Rule Element Assosation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RULE_ELEMENT_ASSOSATION_FEATURE_COUNT = 3;

	/**
	 * The meta object id for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl <em>Container</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getContainer()
	 * @generated
	 */
	int CONTAINER = 7;

	/**
	 * The feature id for the '<em><b>Contains Allocation</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTAINER__CONTAINS_ALLOCATION = 0;

	/**
	 * The feature id for the '<em><b>Contains Situation</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTAINER__CONTAINS_SITUATION = 1;

	/**
	 * The feature id for the '<em><b>Contains BPMN Element</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTAINER__CONTAINS_BPMN_ELEMENT = 2;

	/**
	 * The feature id for the '<em><b>Contains Rule Element Association</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION = 3;

	/**
	 * The feature id for the '<em><b>Contains Rule Element</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTAINER__CONTAINS_RULE_ELEMENT = 4;

	/**
	 * The feature id for the '<em><b>Contains Rule Elemnt Type</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTAINER__CONTAINS_RULE_ELEMNT_TYPE = 5;

	/**
	 * The feature id for the '<em><b>Contains Rule Element Association Type</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION_TYPE = 6;

	/**
	 * The feature id for the '<em><b>Contains Model Type</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTAINER__CONTAINS_MODEL_TYPE = 7;

	/**
	 * The number of structural features of the '<em>Container</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONTAINER_FEATURE_COUNT = 8;


	/**
	 * The meta object id for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.ModelElementTypeImpl <em>Model Element Type</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.ModelElementTypeImpl
	 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getModelElementType()
	 * @generated
	 */
	int MODEL_ELEMENT_TYPE = 8;

	/**
	 * The feature id for the '<em><b>Associats With</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_ELEMENT_TYPE__ASSOCIATS_WITH = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_ELEMENT_TYPE__NAME = 1;

	/**
	 * The number of structural features of the '<em>Model Element Type</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int MODEL_ELEMENT_TYPE_FEATURE_COUNT = 2;


	/**
	 * Returns the meta object for class '{@link carisma.regulatory.ruleallocator.datamodel.RuleElement <em>Rule Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Rule Element</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElement
	 * @generated
	 */
	EClass getRuleElement();

	/**
	 * Returns the meta object for the attribute '{@link carisma.regulatory.ruleallocator.datamodel.RuleElement#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElement#getName()
	 * @see #getRuleElement()
	 * @generated
	 */
	EAttribute getRuleElement_Name();

	/**
	 * Returns the meta object for the reference '{@link carisma.regulatory.ruleallocator.datamodel.RuleElement#getType <em>Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Type</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElement#getType()
	 * @see #getRuleElement()
	 * @generated
	 */
	EReference getRuleElement_Type();

	/**
	 * Returns the meta object for the reference list '{@link carisma.regulatory.ruleallocator.datamodel.RuleElement#getBelongsToSituation <em>Belongs To Situation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Belongs To Situation</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElement#getBelongsToSituation()
	 * @see #getRuleElement()
	 * @generated
	 */
	EReference getRuleElement_BelongsToSituation();

	/**
	 * Returns the meta object for class '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementType <em>Rule Element Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Rule Element Type</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementType
	 * @generated
	 */
	EClass getRuleElementType();

	/**
	 * Returns the meta object for the attribute '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementType#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementType#getName()
	 * @see #getRuleElementType()
	 * @generated
	 */
	EAttribute getRuleElementType_Name();

	/**
	 * Returns the meta object for class '{@link carisma.regulatory.ruleallocator.datamodel.Situation <em>Situation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Situation</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Situation
	 * @generated
	 */
	EClass getSituation();

	/**
	 * Returns the meta object for the attribute '{@link carisma.regulatory.ruleallocator.datamodel.Situation#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Situation#getName()
	 * @see #getSituation()
	 * @generated
	 */
	EAttribute getSituation_Name();

	/**
	 * Returns the meta object for the reference list '{@link carisma.regulatory.ruleallocator.datamodel.Situation#getHas <em>Has</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Has</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Situation#getHas()
	 * @see #getSituation()
	 * @generated
	 */
	EReference getSituation_Has();

	/**
	 * Returns the meta object for class '{@link carisma.regulatory.ruleallocator.datamodel.Allocation <em>Allocation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Allocation</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Allocation
	 * @generated
	 */
	EClass getAllocation();

	/**
	 * Returns the meta object for the reference '{@link carisma.regulatory.ruleallocator.datamodel.Allocation#getRuleElement <em>Rule Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Rule Element</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Allocation#getRuleElement()
	 * @see #getAllocation()
	 * @generated
	 */
	EReference getAllocation_RuleElement();

	/**
	 * Returns the meta object for the reference '{@link carisma.regulatory.ruleallocator.datamodel.Allocation#getBpmnElement <em>Bpmn Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Bpmn Element</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Allocation#getBpmnElement()
	 * @see #getAllocation()
	 * @generated
	 */
	EReference getAllocation_BpmnElement();

	/**
	 * Returns the meta object for class '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement <em>BPMN Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>BPMN Element</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.BPMNElement
	 * @generated
	 */
	EClass getBPMNElement();

	/**
	 * Returns the meta object for the attribute '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.BPMNElement#getName()
	 * @see #getBPMNElement()
	 * @generated
	 */
	EAttribute getBPMNElement_Name();

	/**
	 * Returns the meta object for the attribute '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getID <em>ID</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>ID</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.BPMNElement#getID()
	 * @see #getBPMNElement()
	 * @generated
	 */
	EAttribute getBPMNElement_ID();

	/**
	 * Returns the meta object for the reference '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getType <em>Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Type</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.BPMNElement#getType()
	 * @see #getBPMNElement()
	 * @generated
	 */
	EReference getBPMNElement_Type();

	/**
	 * Returns the meta object for the attribute list '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getIncoming <em>Incoming</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Incoming</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.BPMNElement#getIncoming()
	 * @see #getBPMNElement()
	 * @generated
	 */
	EAttribute getBPMNElement_Incoming();

	/**
	 * Returns the meta object for the attribute list '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getOutgoing <em>Outgoing</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Outgoing</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.BPMNElement#getOutgoing()
	 * @see #getBPMNElement()
	 * @generated
	 */
	EAttribute getBPMNElement_Outgoing();

	/**
	 * Returns the meta object for the attribute '{@link carisma.regulatory.ruleallocator.datamodel.BPMNElement#getProcessId <em>Process Id</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Process Id</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.BPMNElement#getProcessId()
	 * @see #getBPMNElement()
	 * @generated
	 */
	EAttribute getBPMNElement_ProcessId();

	/**
	 * Returns the meta object for class '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType <em>Rule Element Assosiation Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Rule Element Assosiation Type</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType
	 * @generated
	 */
	EClass getRuleElementAssosiationType();

	/**
	 * Returns the meta object for the reference '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getSrc <em>Src</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Src</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getSrc()
	 * @see #getRuleElementAssosiationType()
	 * @generated
	 */
	EReference getRuleElementAssosiationType_Src();

	/**
	 * Returns the meta object for the reference '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getTarget <em>Target</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Target</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getTarget()
	 * @see #getRuleElementAssosiationType()
	 * @generated
	 */
	EReference getRuleElementAssosiationType_Target();

	/**
	 * Returns the meta object for the attribute '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getName()
	 * @see #getRuleElementAssosiationType()
	 * @generated
	 */
	EAttribute getRuleElementAssosiationType_Name();

	/**
	 * Returns the meta object for class '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation <em>Rule Element Assosation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Rule Element Assosation</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation
	 * @generated
	 */
	EClass getRuleElementAssosation();

	/**
	 * Returns the meta object for the reference '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getSrc1 <em>Src1</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Src1</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getSrc1()
	 * @see #getRuleElementAssosation()
	 * @generated
	 */
	EReference getRuleElementAssosation_Src1();

	/**
	 * Returns the meta object for the reference '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getTarget1 <em>Target1</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Target1</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getTarget1()
	 * @see #getRuleElementAssosation()
	 * @generated
	 */
	EReference getRuleElementAssosation_Target1();

	/**
	 * Returns the meta object for the reference '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getType1 <em>Type1</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Type1</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getType1()
	 * @see #getRuleElementAssosation()
	 * @generated
	 */
	EReference getRuleElementAssosation_Type1();

	/**
	 * Returns the meta object for class '{@link carisma.regulatory.ruleallocator.datamodel.Container <em>Container</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Container</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Container
	 * @generated
	 */
	EClass getContainer();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsAllocation <em>Contains Allocation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Contains Allocation</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Container#getContainsAllocation()
	 * @see #getContainer()
	 * @generated
	 */
	EReference getContainer_ContainsAllocation();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsSituation <em>Contains Situation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Contains Situation</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Container#getContainsSituation()
	 * @see #getContainer()
	 * @generated
	 */
	EReference getContainer_ContainsSituation();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsBPMNElement <em>Contains BPMN Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Contains BPMN Element</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Container#getContainsBPMNElement()
	 * @see #getContainer()
	 * @generated
	 */
	EReference getContainer_ContainsBPMNElement();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElementAssociation <em>Contains Rule Element Association</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Contains Rule Element Association</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElementAssociation()
	 * @see #getContainer()
	 * @generated
	 */
	EReference getContainer_ContainsRuleElementAssociation();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElement <em>Contains Rule Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Contains Rule Element</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElement()
	 * @see #getContainer()
	 * @generated
	 */
	EReference getContainer_ContainsRuleElement();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElementType <em>Contains Rule Elemnt Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Contains Rule Elemnt Type</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElementType()
	 * @see #getContainer()
	 * @generated
	 */
	EReference getContainer_ContainsRuleElementType();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElementAssociationType <em>Contains Rule Element Association Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Contains Rule Element Association Type</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElementAssociationType()
	 * @see #getContainer()
	 * @generated
	 */
	EReference getContainer_ContainsRuleElementAssociationType();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsModelType <em>Contains Model Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Contains Model Type</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.Container#getContainsModelType()
	 * @see #getContainer()
	 * @generated
	 */
	EReference getContainer_ContainsModelType();

	/**
	 * Returns the meta object for class '{@link carisma.regulatory.ruleallocator.datamodel.ModelElementType <em>Model Element Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Model Element Type</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.ModelElementType
	 * @generated
	 */
	EClass getModelElementType();

	/**
	 * Returns the meta object for the reference list '{@link carisma.regulatory.ruleallocator.datamodel.ModelElementType#getAssociatsWith <em>Associats With</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Associats With</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.ModelElementType#getAssociatsWith()
	 * @see #getModelElementType()
	 * @generated
	 */
	EReference getModelElementType_AssociatsWith();

	/**
	 * Returns the meta object for the attribute '{@link carisma.regulatory.ruleallocator.datamodel.ModelElementType#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.regulatory.ruleallocator.datamodel.ModelElementType#getName()
	 * @see #getModelElementType()
	 * @generated
	 */
	EAttribute getModelElementType_Name();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	DatamodelFactory getModel2Factory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementImpl <em>Rule Element</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.RuleElementImpl
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getRuleElement()
		 * @generated
		 */
		EClass RULE_ELEMENT = eINSTANCE.getRuleElement();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RULE_ELEMENT__NAME = eINSTANCE.getRuleElement_Name();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RULE_ELEMENT__TYPE = eINSTANCE.getRuleElement_Type();

		/**
		 * The meta object literal for the '<em><b>Belongs To Situation</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RULE_ELEMENT__BELONGS_TO_SITUATION = eINSTANCE.getRuleElement_BelongsToSituation();

		/**
		 * The meta object literal for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementTypeImpl <em>Rule Element Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.RuleElementTypeImpl
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getRuleElementType()
		 * @generated
		 */
		EClass RULE_ELEMENT_TYPE = eINSTANCE.getRuleElementType();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RULE_ELEMENT_TYPE__NAME = eINSTANCE.getRuleElementType_Name();

		/**
		 * The meta object literal for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.SituationImpl <em>Situation</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.SituationImpl
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getSituation()
		 * @generated
		 */
		EClass SITUATION = eINSTANCE.getSituation();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SITUATION__NAME = eINSTANCE.getSituation_Name();

		/**
		 * The meta object literal for the '<em><b>Has</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SITUATION__HAS = eINSTANCE.getSituation_Has();

		/**
		 * The meta object literal for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.AllocationImpl <em>Allocation</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.AllocationImpl
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getAllocation()
		 * @generated
		 */
		EClass ALLOCATION = eINSTANCE.getAllocation();

		/**
		 * The meta object literal for the '<em><b>Rule Element</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ALLOCATION__RULE_ELEMENT = eINSTANCE.getAllocation_RuleElement();

		/**
		 * The meta object literal for the '<em><b>Bpmn Element</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ALLOCATION__BPMN_ELEMENT = eINSTANCE.getAllocation_BpmnElement();

		/**
		 * The meta object literal for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.BPMNElementImpl <em>BPMN Element</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.BPMNElementImpl
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getBPMNElement()
		 * @generated
		 */
		EClass BPMN_ELEMENT = eINSTANCE.getBPMNElement();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute BPMN_ELEMENT__NAME = eINSTANCE.getBPMNElement_Name();

		/**
		 * The meta object literal for the '<em><b>ID</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute BPMN_ELEMENT__ID = eINSTANCE.getBPMNElement_ID();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference BPMN_ELEMENT__TYPE = eINSTANCE.getBPMNElement_Type();

		/**
		 * The meta object literal for the '<em><b>Incoming</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute BPMN_ELEMENT__INCOMING = eINSTANCE.getBPMNElement_Incoming();

		/**
		 * The meta object literal for the '<em><b>Outgoing</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute BPMN_ELEMENT__OUTGOING = eINSTANCE.getBPMNElement_Outgoing();

		/**
		 * The meta object literal for the '<em><b>Process Id</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute BPMN_ELEMENT__PROCESS_ID = eINSTANCE.getBPMNElement_ProcessId();

		/**
		 * The meta object literal for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationTypeImpl <em>Rule Element Assosiation Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationTypeImpl
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getRuleElementAssosiationType()
		 * @generated
		 */
		EClass RULE_ELEMENT_ASSOSIATION_TYPE = eINSTANCE.getRuleElementAssosiationType();

		/**
		 * The meta object literal for the '<em><b>Src</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RULE_ELEMENT_ASSOSIATION_TYPE__SRC = eINSTANCE.getRuleElementAssosiationType_Src();

		/**
		 * The meta object literal for the '<em><b>Target</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RULE_ELEMENT_ASSOSIATION_TYPE__TARGET = eINSTANCE.getRuleElementAssosiationType_Target();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RULE_ELEMENT_ASSOSIATION_TYPE__NAME = eINSTANCE.getRuleElementAssosiationType_Name();

		/**
		 * The meta object literal for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationImpl <em>Rule Element Assosation</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationImpl
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getRuleElementAssosation()
		 * @generated
		 */
		EClass RULE_ELEMENT_ASSOSATION = eINSTANCE.getRuleElementAssosation();

		/**
		 * The meta object literal for the '<em><b>Src1</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RULE_ELEMENT_ASSOSATION__SRC1 = eINSTANCE.getRuleElementAssosation_Src1();

		/**
		 * The meta object literal for the '<em><b>Target1</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RULE_ELEMENT_ASSOSATION__TARGET1 = eINSTANCE.getRuleElementAssosation_Target1();

		/**
		 * The meta object literal for the '<em><b>Type1</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RULE_ELEMENT_ASSOSATION__TYPE1 = eINSTANCE.getRuleElementAssosation_Type1();

		/**
		 * The meta object literal for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl <em>Container</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getContainer()
		 * @generated
		 */
		EClass CONTAINER = eINSTANCE.getContainer();

		/**
		 * The meta object literal for the '<em><b>Contains Allocation</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONTAINER__CONTAINS_ALLOCATION = eINSTANCE.getContainer_ContainsAllocation();

		/**
		 * The meta object literal for the '<em><b>Contains Situation</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONTAINER__CONTAINS_SITUATION = eINSTANCE.getContainer_ContainsSituation();

		/**
		 * The meta object literal for the '<em><b>Contains BPMN Element</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONTAINER__CONTAINS_BPMN_ELEMENT = eINSTANCE.getContainer_ContainsBPMNElement();

		/**
		 * The meta object literal for the '<em><b>Contains Rule Element Association</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION = eINSTANCE.getContainer_ContainsRuleElementAssociation();

		/**
		 * The meta object literal for the '<em><b>Contains Rule Element</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONTAINER__CONTAINS_RULE_ELEMENT = eINSTANCE.getContainer_ContainsRuleElement();

		/**
		 * The meta object literal for the '<em><b>Contains Rule Elemnt Type</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONTAINER__CONTAINS_RULE_ELEMNT_TYPE = eINSTANCE.getContainer_ContainsRuleElementType();

		/**
		 * The meta object literal for the '<em><b>Contains Rule Element Association Type</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION_TYPE = eINSTANCE.getContainer_ContainsRuleElementAssociationType();

		/**
		 * The meta object literal for the '<em><b>Contains Model Type</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONTAINER__CONTAINS_MODEL_TYPE = eINSTANCE.getContainer_ContainsModelType();

		/**
		 * The meta object literal for the '{@link carisma.regulatory.ruleallocator.datamodel.impl.ModelElementTypeImpl <em>Model Element Type</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.ModelElementTypeImpl
		 * @see carisma.regulatory.ruleallocator.datamodel.impl.DatamodelPackageImpl#getModelElementType()
		 * @generated
		 */
		EClass MODEL_ELEMENT_TYPE = eINSTANCE.getModelElementType();

		/**
		 * The meta object literal for the '<em><b>Associats With</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference MODEL_ELEMENT_TYPE__ASSOCIATS_WITH = eINSTANCE.getModelElementType_AssociatsWith();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute MODEL_ELEMENT_TYPE__NAME = eINSTANCE.getModelElementType_Name();

	}

} //Model2Package
