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
package carisma.modeltype.bpmn2.extension;

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
 * @see carisma.modeltype.bpmn2.extension.ExtensionFactory
 * @model kind="package"
 * @generated
 */
public interface ExtensionPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "extension";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://carisma/modeltype/bpmn2/extension";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "extension";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	ExtensionPackage eINSTANCE = carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl.init();

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extension.impl.ExtensionRootImpl <em>Root</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionRootImpl
	 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getExtensionRoot()
	 * @generated
	 */
	int EXTENSION_ROOT = 0;

	/**
	 * The feature id for the '<em><b>Task</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENSION_ROOT__TASK = 0;

	/**
	 * The feature id for the '<em><b>Work Item</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENSION_ROOT__WORK_ITEM = 1;

	/**
	 * The feature id for the '<em><b>Performer</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENSION_ROOT__PERFORMER = 2;

	/**
	 * The feature id for the '<em><b>Role</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENSION_ROOT__ROLE = 3;

	/**
	 * The feature id for the '<em><b>Lane</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENSION_ROOT__LANE = 4;

	/**
	 * The feature id for the '<em><b>Selection</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENSION_ROOT__SELECTION = 5;

	/**
	 * The number of structural features of the '<em>Root</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int EXTENSION_ROOT_FEATURE_COUNT = 6;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extension.impl.BaseElementImpl <em>Base Element</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extension.impl.BaseElementImpl
	 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getBaseElement()
	 * @generated
	 */
	int BASE_ELEMENT = 8;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BASE_ELEMENT__ID = 0;

	/**
	 * The number of structural features of the '<em>Base Element</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int BASE_ELEMENT_FEATURE_COUNT = 1;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extension.impl.WorkItemImpl <em>Work Item</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extension.impl.WorkItemImpl
	 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getWorkItem()
	 * @generated
	 */
	int WORK_ITEM = 1;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WORK_ITEM__ID = BASE_ELEMENT__ID;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WORK_ITEM__NAME = BASE_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Duration</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WORK_ITEM__DURATION = BASE_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Performer</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WORK_ITEM__PERFORMER = BASE_ELEMENT_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Work Item</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WORK_ITEM_FEATURE_COUNT = BASE_ELEMENT_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extension.impl.PerformerImpl <em>Performer</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extension.impl.PerformerImpl
	 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getPerformer()
	 * @generated
	 */
	int PERFORMER = 2;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERFORMER__ID = BASE_ELEMENT__ID;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERFORMER__NAME = BASE_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Work Item</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERFORMER__WORK_ITEM = BASE_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Role</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERFORMER__ROLE = BASE_ELEMENT_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Performer</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PERFORMER_FEATURE_COUNT = BASE_ELEMENT_FEATURE_COUNT + 3;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extension.impl.RoleImpl <em>Role</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extension.impl.RoleImpl
	 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getRole()
	 * @generated
	 */
	int ROLE = 3;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ROLE__ID = BASE_ELEMENT__ID;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ROLE__NAME = BASE_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Member</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ROLE__MEMBER = BASE_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Super</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ROLE__SUPER = BASE_ELEMENT_FEATURE_COUNT + 2;

	/**
	 * The feature id for the '<em><b>Sub</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ROLE__SUB = BASE_ELEMENT_FEATURE_COUNT + 3;

	/**
	 * The feature id for the '<em><b>Conflict</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ROLE__CONFLICT = BASE_ELEMENT_FEATURE_COUNT + 4;

	/**
	 * The number of structural features of the '<em>Role</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ROLE_FEATURE_COUNT = BASE_ELEMENT_FEATURE_COUNT + 5;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extension.impl.LaneImpl <em>Lane</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extension.impl.LaneImpl
	 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getLane()
	 * @generated
	 */
	int LANE = 4;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LANE__ID = BASE_ELEMENT__ID;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LANE__NAME = BASE_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Role</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LANE__ROLE = BASE_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Lane</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LANE_FEATURE_COUNT = BASE_ELEMENT_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extension.impl.TaskImpl <em>Task</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extension.impl.TaskImpl
	 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getTask()
	 * @generated
	 */
	int TASK = 5;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TASK__ID = BASE_ELEMENT__ID;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TASK__NAME = BASE_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Work Item</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TASK__WORK_ITEM = BASE_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Task</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TASK_FEATURE_COUNT = BASE_ELEMENT_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extension.impl.TaskSetImpl <em>Task Set</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extension.impl.TaskSetImpl
	 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getTaskSet()
	 * @generated
	 */
	int TASK_SET = 6;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TASK_SET__ID = BASE_ELEMENT__ID;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TASK_SET__NAME = BASE_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Selected Tasks</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TASK_SET__SELECTED_TASKS = BASE_ELEMENT_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Task Set</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TASK_SET_FEATURE_COUNT = BASE_ELEMENT_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link carisma.modeltype.bpmn2.extension.impl.SelectionImpl <em>Selection</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.modeltype.bpmn2.extension.impl.SelectionImpl
	 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getSelection()
	 * @generated
	 */
	int SELECTION = 7;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SELECTION__ID = BASE_ELEMENT__ID;

	/**
	 * The feature id for the '<em><b>Task Set</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SELECTION__TASK_SET = BASE_ELEMENT_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Selection</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SELECTION_FEATURE_COUNT = BASE_ELEMENT_FEATURE_COUNT + 1;


	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extension.ExtensionRoot <em>Root</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Root</em>'.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionRoot
	 * @generated
	 */
	EClass getExtensionRoot();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getTask <em>Task</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Task</em>'.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionRoot#getTask()
	 * @see #getExtensionRoot()
	 * @generated
	 */
	EReference getExtensionRoot_Task();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getWorkItem <em>Work Item</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Work Item</em>'.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionRoot#getWorkItem()
	 * @see #getExtensionRoot()
	 * @generated
	 */
	EReference getExtensionRoot_WorkItem();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getPerformer <em>Performer</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Performer</em>'.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionRoot#getPerformer()
	 * @see #getExtensionRoot()
	 * @generated
	 */
	EReference getExtensionRoot_Performer();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getRole <em>Role</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Role</em>'.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionRoot#getRole()
	 * @see #getExtensionRoot()
	 * @generated
	 */
	EReference getExtensionRoot_Role();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getLane <em>Lane</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Lane</em>'.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionRoot#getLane()
	 * @see #getExtensionRoot()
	 * @generated
	 */
	EReference getExtensionRoot_Lane();

	/**
	 * Returns the meta object for the containment reference '{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getSelection <em>Selection</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Selection</em>'.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionRoot#getSelection()
	 * @see #getExtensionRoot()
	 * @generated
	 */
	EReference getExtensionRoot_Selection();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extension.WorkItem <em>Work Item</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Work Item</em>'.
	 * @see carisma.modeltype.bpmn2.extension.WorkItem
	 * @generated
	 */
	EClass getWorkItem();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.bpmn2.extension.WorkItem#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.modeltype.bpmn2.extension.WorkItem#getName()
	 * @see #getWorkItem()
	 * @generated
	 */
	EAttribute getWorkItem_Name();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.bpmn2.extension.WorkItem#getDuration <em>Duration</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Duration</em>'.
	 * @see carisma.modeltype.bpmn2.extension.WorkItem#getDuration()
	 * @see #getWorkItem()
	 * @generated
	 */
	EAttribute getWorkItem_Duration();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.bpmn2.extension.WorkItem#getPerformer <em>Performer</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Performer</em>'.
	 * @see carisma.modeltype.bpmn2.extension.WorkItem#getPerformer()
	 * @see #getWorkItem()
	 * @generated
	 */
	EReference getWorkItem_Performer();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extension.Performer <em>Performer</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Performer</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Performer
	 * @generated
	 */
	EClass getPerformer();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.bpmn2.extension.Performer#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Performer#getName()
	 * @see #getPerformer()
	 * @generated
	 */
	EAttribute getPerformer_Name();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.bpmn2.extension.Performer#getWorkItem <em>Work Item</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Work Item</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Performer#getWorkItem()
	 * @see #getPerformer()
	 * @generated
	 */
	EReference getPerformer_WorkItem();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.bpmn2.extension.Performer#getRole <em>Role</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Role</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Performer#getRole()
	 * @see #getPerformer()
	 * @generated
	 */
	EReference getPerformer_Role();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extension.Role <em>Role</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Role</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Role
	 * @generated
	 */
	EClass getRole();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.bpmn2.extension.Role#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Role#getName()
	 * @see #getRole()
	 * @generated
	 */
	EAttribute getRole_Name();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.bpmn2.extension.Role#getMember <em>Member</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Member</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Role#getMember()
	 * @see #getRole()
	 * @generated
	 */
	EReference getRole_Member();

	/**
	 * Returns the meta object for the reference '{@link carisma.modeltype.bpmn2.extension.Role#getSuper <em>Super</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Super</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Role#getSuper()
	 * @see #getRole()
	 * @generated
	 */
	EReference getRole_Super();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.bpmn2.extension.Role#getSub <em>Sub</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Sub</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Role#getSub()
	 * @see #getRole()
	 * @generated
	 */
	EReference getRole_Sub();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.bpmn2.extension.Role#getConflict <em>Conflict</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Conflict</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Role#getConflict()
	 * @see #getRole()
	 * @generated
	 */
	EReference getRole_Conflict();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extension.Lane <em>Lane</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Lane</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Lane
	 * @generated
	 */
	EClass getLane();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.bpmn2.extension.Lane#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Lane#getName()
	 * @see #getLane()
	 * @generated
	 */
	EAttribute getLane_Name();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.bpmn2.extension.Lane#getRole <em>Role</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Role</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Lane#getRole()
	 * @see #getLane()
	 * @generated
	 */
	EReference getLane_Role();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extension.Task <em>Task</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Task</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Task
	 * @generated
	 */
	EClass getTask();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.bpmn2.extension.Task#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Task#getName()
	 * @see #getTask()
	 * @generated
	 */
	EAttribute getTask_Name();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.bpmn2.extension.Task#getWorkItem <em>Work Item</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Work Item</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Task#getWorkItem()
	 * @see #getTask()
	 * @generated
	 */
	EReference getTask_WorkItem();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extension.TaskSet <em>Task Set</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Task Set</em>'.
	 * @see carisma.modeltype.bpmn2.extension.TaskSet
	 * @generated
	 */
	EClass getTaskSet();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.bpmn2.extension.TaskSet#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.modeltype.bpmn2.extension.TaskSet#getName()
	 * @see #getTaskSet()
	 * @generated
	 */
	EAttribute getTaskSet_Name();

	/**
	 * Returns the meta object for the reference list '{@link carisma.modeltype.bpmn2.extension.TaskSet#getSelectedTasks <em>Selected Tasks</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Selected Tasks</em>'.
	 * @see carisma.modeltype.bpmn2.extension.TaskSet#getSelectedTasks()
	 * @see #getTaskSet()
	 * @generated
	 */
	EReference getTaskSet_SelectedTasks();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extension.Selection <em>Selection</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Selection</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Selection
	 * @generated
	 */
	EClass getSelection();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.modeltype.bpmn2.extension.Selection#getTaskSet <em>Task Set</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Task Set</em>'.
	 * @see carisma.modeltype.bpmn2.extension.Selection#getTaskSet()
	 * @see #getSelection()
	 * @generated
	 */
	EReference getSelection_TaskSet();

	/**
	 * Returns the meta object for class '{@link carisma.modeltype.bpmn2.extension.BaseElement <em>Base Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Base Element</em>'.
	 * @see carisma.modeltype.bpmn2.extension.BaseElement
	 * @generated
	 */
	EClass getBaseElement();

	/**
	 * Returns the meta object for the attribute '{@link carisma.modeltype.bpmn2.extension.BaseElement#getId <em>Id</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Id</em>'.
	 * @see carisma.modeltype.bpmn2.extension.BaseElement#getId()
	 * @see #getBaseElement()
	 * @generated
	 */
	EAttribute getBaseElement_Id();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	ExtensionFactory getExtensionFactory();

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
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extension.impl.ExtensionRootImpl <em>Root</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionRootImpl
		 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getExtensionRoot()
		 * @generated
		 */
		EClass EXTENSION_ROOT = eINSTANCE.getExtensionRoot();

		/**
		 * The meta object literal for the '<em><b>Task</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EXTENSION_ROOT__TASK = eINSTANCE.getExtensionRoot_Task();

		/**
		 * The meta object literal for the '<em><b>Work Item</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EXTENSION_ROOT__WORK_ITEM = eINSTANCE.getExtensionRoot_WorkItem();

		/**
		 * The meta object literal for the '<em><b>Performer</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EXTENSION_ROOT__PERFORMER = eINSTANCE.getExtensionRoot_Performer();

		/**
		 * The meta object literal for the '<em><b>Role</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EXTENSION_ROOT__ROLE = eINSTANCE.getExtensionRoot_Role();

		/**
		 * The meta object literal for the '<em><b>Lane</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EXTENSION_ROOT__LANE = eINSTANCE.getExtensionRoot_Lane();

		/**
		 * The meta object literal for the '<em><b>Selection</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference EXTENSION_ROOT__SELECTION = eINSTANCE.getExtensionRoot_Selection();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extension.impl.WorkItemImpl <em>Work Item</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extension.impl.WorkItemImpl
		 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getWorkItem()
		 * @generated
		 */
		EClass WORK_ITEM = eINSTANCE.getWorkItem();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute WORK_ITEM__NAME = eINSTANCE.getWorkItem_Name();

		/**
		 * The meta object literal for the '<em><b>Duration</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute WORK_ITEM__DURATION = eINSTANCE.getWorkItem_Duration();

		/**
		 * The meta object literal for the '<em><b>Performer</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference WORK_ITEM__PERFORMER = eINSTANCE.getWorkItem_Performer();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extension.impl.PerformerImpl <em>Performer</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extension.impl.PerformerImpl
		 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getPerformer()
		 * @generated
		 */
		EClass PERFORMER = eINSTANCE.getPerformer();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PERFORMER__NAME = eINSTANCE.getPerformer_Name();

		/**
		 * The meta object literal for the '<em><b>Work Item</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PERFORMER__WORK_ITEM = eINSTANCE.getPerformer_WorkItem();

		/**
		 * The meta object literal for the '<em><b>Role</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PERFORMER__ROLE = eINSTANCE.getPerformer_Role();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extension.impl.RoleImpl <em>Role</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extension.impl.RoleImpl
		 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getRole()
		 * @generated
		 */
		EClass ROLE = eINSTANCE.getRole();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ROLE__NAME = eINSTANCE.getRole_Name();

		/**
		 * The meta object literal for the '<em><b>Member</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ROLE__MEMBER = eINSTANCE.getRole_Member();

		/**
		 * The meta object literal for the '<em><b>Super</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ROLE__SUPER = eINSTANCE.getRole_Super();

		/**
		 * The meta object literal for the '<em><b>Sub</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ROLE__SUB = eINSTANCE.getRole_Sub();

		/**
		 * The meta object literal for the '<em><b>Conflict</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ROLE__CONFLICT = eINSTANCE.getRole_Conflict();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extension.impl.LaneImpl <em>Lane</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extension.impl.LaneImpl
		 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getLane()
		 * @generated
		 */
		EClass LANE = eINSTANCE.getLane();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LANE__NAME = eINSTANCE.getLane_Name();

		/**
		 * The meta object literal for the '<em><b>Role</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference LANE__ROLE = eINSTANCE.getLane_Role();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extension.impl.TaskImpl <em>Task</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extension.impl.TaskImpl
		 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getTask()
		 * @generated
		 */
		EClass TASK = eINSTANCE.getTask();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TASK__NAME = eINSTANCE.getTask_Name();

		/**
		 * The meta object literal for the '<em><b>Work Item</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TASK__WORK_ITEM = eINSTANCE.getTask_WorkItem();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extension.impl.TaskSetImpl <em>Task Set</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extension.impl.TaskSetImpl
		 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getTaskSet()
		 * @generated
		 */
		EClass TASK_SET = eINSTANCE.getTaskSet();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TASK_SET__NAME = eINSTANCE.getTaskSet_Name();

		/**
		 * The meta object literal for the '<em><b>Selected Tasks</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TASK_SET__SELECTED_TASKS = eINSTANCE.getTaskSet_SelectedTasks();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extension.impl.SelectionImpl <em>Selection</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extension.impl.SelectionImpl
		 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getSelection()
		 * @generated
		 */
		EClass SELECTION = eINSTANCE.getSelection();

		/**
		 * The meta object literal for the '<em><b>Task Set</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SELECTION__TASK_SET = eINSTANCE.getSelection_TaskSet();

		/**
		 * The meta object literal for the '{@link carisma.modeltype.bpmn2.extension.impl.BaseElementImpl <em>Base Element</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.modeltype.bpmn2.extension.impl.BaseElementImpl
		 * @see carisma.modeltype.bpmn2.extension.impl.ExtensionPackageImpl#getBaseElement()
		 * @generated
		 */
		EClass BASE_ELEMENT = eINSTANCE.getBaseElement();

		/**
		 * The meta object literal for the '<em><b>Id</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute BASE_ELEMENT__ID = eINSTANCE.getBaseElement_Id();

	}

} //ExtensionPackage
