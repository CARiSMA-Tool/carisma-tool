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
package carisma.modeltype.bpmn2.extension.impl;


import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

import carisma.modeltype.bpmn2.extension.BaseElement;
import carisma.modeltype.bpmn2.extension.ExtensionFactory;
import carisma.modeltype.bpmn2.extension.ExtensionPackage;
import carisma.modeltype.bpmn2.extension.ExtensionRoot;
import carisma.modeltype.bpmn2.extension.Lane;
import carisma.modeltype.bpmn2.extension.Performer;
import carisma.modeltype.bpmn2.extension.Role;
import carisma.modeltype.bpmn2.extension.Selection;
import carisma.modeltype.bpmn2.extension.Task;
import carisma.modeltype.bpmn2.extension.TaskSet;
import carisma.modeltype.bpmn2.extension.WorkItem;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class ExtensionFactoryImpl extends EFactoryImpl implements ExtensionFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static ExtensionFactory init() {
		try {
			ExtensionFactory theExtensionFactory = (ExtensionFactory)EPackage.Registry.INSTANCE.getEFactory("http://carisma/modeltype/bpmn2/extension"); 
			if (theExtensionFactory != null) {
				return theExtensionFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new ExtensionFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ExtensionFactoryImpl() {
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
			case ExtensionPackage.EXTENSION_ROOT: return createExtensionRoot();
			case ExtensionPackage.WORK_ITEM: return createWorkItem();
			case ExtensionPackage.PERFORMER: return createPerformer();
			case ExtensionPackage.ROLE: return createRole();
			case ExtensionPackage.LANE: return createLane();
			case ExtensionPackage.TASK: return createTask();
			case ExtensionPackage.TASK_SET: return createTaskSet();
			case ExtensionPackage.SELECTION: return createSelection();
			case ExtensionPackage.BASE_ELEMENT: return createBaseElement();
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
	public ExtensionRoot createExtensionRoot() {
		ExtensionRootImpl extensionRoot = new ExtensionRootImpl();
		return extensionRoot;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public WorkItem createWorkItem() {
		WorkItemImpl workItem = new WorkItemImpl();
		return workItem;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Performer createPerformer() {
		PerformerImpl performer = new PerformerImpl();
		return performer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Role createRole() {
		RoleImpl role = new RoleImpl();
		return role;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Lane createLane() {
		LaneImpl lane = new LaneImpl();
		return lane;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Task createTask() {
		TaskImpl task = new TaskImpl();
		return task;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public TaskSet createTaskSet() {
		TaskSetImpl taskSet = new TaskSetImpl();
		return taskSet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Selection createSelection() {
		SelectionImpl selection = new SelectionImpl();
		return selection;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public BaseElement createBaseElement() {
		BaseElementImpl baseElement = new BaseElementImpl();
		return baseElement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ExtensionPackage getExtensionPackage() {
		return (ExtensionPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static ExtensionPackage getPackage() {
		return ExtensionPackage.eINSTANCE;
	}

} //ExtensionFactoryImpl
