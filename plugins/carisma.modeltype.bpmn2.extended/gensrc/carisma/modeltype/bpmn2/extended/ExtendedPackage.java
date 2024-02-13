/**
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 */
package carisma.modeltype.bpmn2.extended;

import org.eclipse.bpmn2.Bpmn2Package;
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
 * @see carisma.modeltype.bpmn2.extended.ExtendedFactory
 * @model kind="package"
 * @generated
 */
public interface ExtendedPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "extended";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://carisma/modeltype/bpmn2/extended";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "extended";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	ExtendedPackage eINSTANCE = carisma.modeltype.bpmn2.extended.impl.ExtendedPackageImpl.init();

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extended.impl.ExtendedDocumentRootImpl <em>Document Root</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedDocumentRootImpl
	 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedPackageImpl#getExtendedDocumentRoot()
	 * @generated
	 */
	int EXTENDED_DOCUMENT_ROOT = 0;

	/**
	 * The feature id for the '<em><b>Extended Definitions</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS = 0;

	/**
	 * The feature id for the '<em><b>Extension Root</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT = 1;

	/**
	 * The number of structural features of the '<em>Document Root</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_DOCUMENT_ROOT_FEATURE_COUNT = 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extended.impl.ExtendedProcessImpl <em>Process</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedProcessImpl
	 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedPackageImpl#getExtendedProcess()
	 * @generated
	 */
	int EXTENDED_PROCESS = 1;

	/**
	 * The feature id for the '<em><b>Extension Values</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__EXTENSION_VALUES = Bpmn2Package.PROCESS__EXTENSION_VALUES;

	/**
	 * The feature id for the '<em><b>Documentation</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__DOCUMENTATION = Bpmn2Package.PROCESS__DOCUMENTATION;

	/**
	 * The feature id for the '<em><b>Extension Definitions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__EXTENSION_DEFINITIONS = Bpmn2Package.PROCESS__EXTENSION_DEFINITIONS;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__ID = Bpmn2Package.PROCESS__ID;

	/**
	 * The feature id for the '<em><b>Any Attribute</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__ANY_ATTRIBUTE = Bpmn2Package.PROCESS__ANY_ATTRIBUTE;

	/**
	 * The feature id for the '<em><b>Supported Interface Refs</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__SUPPORTED_INTERFACE_REFS = Bpmn2Package.PROCESS__SUPPORTED_INTERFACE_REFS;

	/**
	 * The feature id for the '<em><b>Io Specification</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__IO_SPECIFICATION = Bpmn2Package.PROCESS__IO_SPECIFICATION;

	/**
	 * The feature id for the '<em><b>Io Binding</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__IO_BINDING = Bpmn2Package.PROCESS__IO_BINDING;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__NAME = Bpmn2Package.PROCESS__NAME;

	/**
	 * The feature id for the '<em><b>Lane Sets</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__LANE_SETS = Bpmn2Package.PROCESS__LANE_SETS;

	/**
	 * The feature id for the '<em><b>Flow Elements</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__FLOW_ELEMENTS = Bpmn2Package.PROCESS__FLOW_ELEMENTS;

	/**
	 * The feature id for the '<em><b>Auditing</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__AUDITING = Bpmn2Package.PROCESS__AUDITING;

	/**
	 * The feature id for the '<em><b>Monitoring</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__MONITORING = Bpmn2Package.PROCESS__MONITORING;

	/**
	 * The feature id for the '<em><b>Properties</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__PROPERTIES = Bpmn2Package.PROCESS__PROPERTIES;

	/**
	 * The feature id for the '<em><b>Artifacts</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__ARTIFACTS = Bpmn2Package.PROCESS__ARTIFACTS;

	/**
	 * The feature id for the '<em><b>Resources</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__RESOURCES = Bpmn2Package.PROCESS__RESOURCES;

	/**
	 * The feature id for the '<em><b>Correlation Subscriptions</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__CORRELATION_SUBSCRIPTIONS = Bpmn2Package.PROCESS__CORRELATION_SUBSCRIPTIONS;

	/**
	 * The feature id for the '<em><b>Supports</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__SUPPORTS = Bpmn2Package.PROCESS__SUPPORTS;

	/**
	 * The feature id for the '<em><b>Definitional Collaboration Ref</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__DEFINITIONAL_COLLABORATION_REF = Bpmn2Package.PROCESS__DEFINITIONAL_COLLABORATION_REF;

	/**
	 * The feature id for the '<em><b>Is Closed</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__IS_CLOSED = Bpmn2Package.PROCESS__IS_CLOSED;

	/**
	 * The feature id for the '<em><b>Is Executable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__IS_EXECUTABLE = Bpmn2Package.PROCESS__IS_EXECUTABLE;

	/**
	 * The feature id for the '<em><b>Process Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS__PROCESS_TYPE = Bpmn2Package.PROCESS__PROCESS_TYPE;

	/**
	 * The number of structural features of the '<em>Process</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_PROCESS_FEATURE_COUNT = Bpmn2Package.PROCESS_FEATURE_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extended.impl.ExtendedTaskImpl <em>Task</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedTaskImpl
	 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedPackageImpl#getExtendedTask()
	 * @generated
	 */
	int EXTENDED_TASK = 2;

	/**
	 * The feature id for the '<em><b>Extension Values</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__EXTENSION_VALUES = Bpmn2Package.TASK__EXTENSION_VALUES;

	/**
	 * The feature id for the '<em><b>Documentation</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__DOCUMENTATION = Bpmn2Package.TASK__DOCUMENTATION;

	/**
	 * The feature id for the '<em><b>Extension Definitions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__EXTENSION_DEFINITIONS = Bpmn2Package.TASK__EXTENSION_DEFINITIONS;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__ID = Bpmn2Package.TASK__ID;

	/**
	 * The feature id for the '<em><b>Any Attribute</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__ANY_ATTRIBUTE = Bpmn2Package.TASK__ANY_ATTRIBUTE;

	/**
	 * The feature id for the '<em><b>Auditing</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__AUDITING = Bpmn2Package.TASK__AUDITING;

	/**
	 * The feature id for the '<em><b>Monitoring</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__MONITORING = Bpmn2Package.TASK__MONITORING;

	/**
	 * The feature id for the '<em><b>Category Value Ref</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__CATEGORY_VALUE_REF = Bpmn2Package.TASK__CATEGORY_VALUE_REF;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__NAME = Bpmn2Package.TASK__NAME;

	/**
	 * The feature id for the '<em><b>Incoming</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__INCOMING = Bpmn2Package.TASK__INCOMING;

	/**
	 * The feature id for the '<em><b>Lanes</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__LANES = Bpmn2Package.TASK__LANES;

	/**
	 * The feature id for the '<em><b>Outgoing</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__OUTGOING = Bpmn2Package.TASK__OUTGOING;

	/**
	 * The feature id for the '<em><b>Io Specification</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__IO_SPECIFICATION = Bpmn2Package.TASK__IO_SPECIFICATION;

	/**
	 * The feature id for the '<em><b>Boundary Event Refs</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__BOUNDARY_EVENT_REFS = Bpmn2Package.TASK__BOUNDARY_EVENT_REFS;

	/**
	 * The feature id for the '<em><b>Properties</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__PROPERTIES = Bpmn2Package.TASK__PROPERTIES;

	/**
	 * The feature id for the '<em><b>Data Input Associations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__DATA_INPUT_ASSOCIATIONS = Bpmn2Package.TASK__DATA_INPUT_ASSOCIATIONS;

	/**
	 * The feature id for the '<em><b>Data Output Associations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__DATA_OUTPUT_ASSOCIATIONS = Bpmn2Package.TASK__DATA_OUTPUT_ASSOCIATIONS;

	/**
	 * The feature id for the '<em><b>Resources</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__RESOURCES = Bpmn2Package.TASK__RESOURCES;

	/**
	 * The feature id for the '<em><b>Loop Characteristics</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__LOOP_CHARACTERISTICS = Bpmn2Package.TASK__LOOP_CHARACTERISTICS;

	/**
	 * The feature id for the '<em><b>Completion Quantity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__COMPLETION_QUANTITY = Bpmn2Package.TASK__COMPLETION_QUANTITY;

	/**
	 * The feature id for the '<em><b>Default</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__DEFAULT = Bpmn2Package.TASK__DEFAULT;

	/**
	 * The feature id for the '<em><b>Is For Compensation</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__IS_FOR_COMPENSATION = Bpmn2Package.TASK__IS_FOR_COMPENSATION;

	/**
	 * The feature id for the '<em><b>Start Quantity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__START_QUANTITY = Bpmn2Package.TASK__START_QUANTITY;

	/**
	 * The feature id for the '<em><b>Incoming Conversation Links</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__INCOMING_CONVERSATION_LINKS = Bpmn2Package.TASK__INCOMING_CONVERSATION_LINKS;

	/**
	 * The feature id for the '<em><b>Outgoing Conversation Links</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__OUTGOING_CONVERSATION_LINKS = Bpmn2Package.TASK__OUTGOING_CONVERSATION_LINKS;

	/**
	 * The feature id for the '<em><b>Work Item</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK__WORK_ITEM = Bpmn2Package.TASK_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Task</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_TASK_FEATURE_COUNT = Bpmn2Package.TASK_FEATURE_COUNT + 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extended.impl.ExtendedLaneImpl <em>Lane</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedLaneImpl
	 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedPackageImpl#getExtendedLane()
	 * @generated
	 */
	int EXTENDED_LANE = 3;

	/**
	 * The feature id for the '<em><b>Extension Values</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__EXTENSION_VALUES = Bpmn2Package.LANE__EXTENSION_VALUES;

	/**
	 * The feature id for the '<em><b>Documentation</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__DOCUMENTATION = Bpmn2Package.LANE__DOCUMENTATION;

	/**
	 * The feature id for the '<em><b>Extension Definitions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__EXTENSION_DEFINITIONS = Bpmn2Package.LANE__EXTENSION_DEFINITIONS;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__ID = Bpmn2Package.LANE__ID;

	/**
	 * The feature id for the '<em><b>Any Attribute</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__ANY_ATTRIBUTE = Bpmn2Package.LANE__ANY_ATTRIBUTE;

	/**
	 * The feature id for the '<em><b>Partition Element</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__PARTITION_ELEMENT = Bpmn2Package.LANE__PARTITION_ELEMENT;

	/**
	 * The feature id for the '<em><b>Flow Node Refs</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__FLOW_NODE_REFS = Bpmn2Package.LANE__FLOW_NODE_REFS;

	/**
	 * The feature id for the '<em><b>Child Lane Set</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__CHILD_LANE_SET = Bpmn2Package.LANE__CHILD_LANE_SET;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__NAME = Bpmn2Package.LANE__NAME;

	/**
	 * The feature id for the '<em><b>Partition Element Ref</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__PARTITION_ELEMENT_REF = Bpmn2Package.LANE__PARTITION_ELEMENT_REF;

	/**
	 * The feature id for the '<em><b>Role</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE__ROLE = Bpmn2Package.LANE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Lane</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENDED_LANE_FEATURE_COUNT = Bpmn2Package.LANE_FEATURE_COUNT + 1;


	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot <em>Document Root</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Document Root</em>'.
	 * @see carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot
	 * @generated
	 */
	EClass getExtendedDocumentRoot();

	/**
	 * Returns the meta object for the containment reference '{@link carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot#getExtendedDefinitions <em>Extended Definitions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Extended Definitions</em>'.
	 * @see carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot#getExtendedDefinitions()
	 * @see #getExtendedDocumentRoot()
	 * @generated
	 */
	EReference getExtendedDocumentRoot_ExtendedDefinitions();

	/**
	 * Returns the meta object for the containment reference '{@link carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot#getExtensionRoot <em>Extension Root</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Extension Root</em>'.
	 * @see carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot#getExtensionRoot()
	 * @see #getExtendedDocumentRoot()
	 * @generated
	 */
	EReference getExtendedDocumentRoot_ExtensionRoot();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extended.ExtendedProcess <em>Process</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Process</em>'.
	 * @see carisma.modeltype.bpmn2.extended.ExtendedProcess
	 * @generated
	 */
	EClass getExtendedProcess();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extended.ExtendedTask <em>Task</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Task</em>'.
	 * @see carisma.modeltype.bpmn2.extended.ExtendedTask
	 * @generated
	 */
	EClass getExtendedTask();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.bpmn2.extended.ExtendedTask#getWorkItem <em>Work Item</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Work Item</em>'.
	 * @see carisma.modeltype.bpmn2.extended.ExtendedTask#getWorkItem()
	 * @see #getExtendedTask()
	 * @generated
	 */
	EReference getExtendedTask_WorkItem();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extended.ExtendedLane <em>Lane</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Lane</em>'.
	 * @see carisma.modeltype.bpmn2.extended.ExtendedLane
	 * @generated
	 */
	EClass getExtendedLane();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.bpmn2.extended.ExtendedLane#getRole <em>Role</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Role</em>'.
	 * @see carisma.modeltype.bpmn2.extended.ExtendedLane#getRole()
	 * @see #getExtendedLane()
	 * @generated
	 */
	EReference getExtendedLane_Role();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	ExtendedFactory getExtendedFactory();

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
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extended.impl.ExtendedDocumentRootImpl <em>Document Root</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedDocumentRootImpl
		 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedPackageImpl#getExtendedDocumentRoot()
		 * @generated
		 */
		EClass EXTENDED_DOCUMENT_ROOT = eINSTANCE.getExtendedDocumentRoot();

		/**
		 * The meta object literal for the '<em><b>Extended Definitions</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS = eINSTANCE.getExtendedDocumentRoot_ExtendedDefinitions();

		/**
		 * The meta object literal for the '<em><b>Extension Root</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT = eINSTANCE.getExtendedDocumentRoot_ExtensionRoot();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extended.impl.ExtendedProcessImpl <em>Process</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedProcessImpl
		 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedPackageImpl#getExtendedProcess()
		 * @generated
		 */
		EClass EXTENDED_PROCESS = eINSTANCE.getExtendedProcess();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extended.impl.ExtendedTaskImpl <em>Task</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedTaskImpl
		 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedPackageImpl#getExtendedTask()
		 * @generated
		 */
		EClass EXTENDED_TASK = eINSTANCE.getExtendedTask();

		/**
		 * The meta object literal for the '<em><b>Work Item</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EXTENDED_TASK__WORK_ITEM = eINSTANCE.getExtendedTask_WorkItem();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extended.impl.ExtendedLaneImpl <em>Lane</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedLaneImpl
		 * @see carisma.modeltype.bpmn2.extended.impl.ExtendedPackageImpl#getExtendedLane()
		 * @generated
		 */
		EClass EXTENDED_LANE = eINSTANCE.getExtendedLane();

		/**
		 * The meta object literal for the '<em><b>Role</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EXTENDED_LANE__ROLE = eINSTANCE.getExtendedLane_Role();

	}

} //ExtendedPackage
