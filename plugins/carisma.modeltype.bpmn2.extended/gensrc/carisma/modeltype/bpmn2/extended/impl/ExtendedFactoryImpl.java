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
package carisma.modeltype.bpmn2.extended.impl;


import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.EFactoryImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;

import carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot;
import carisma.modeltype.bpmn2.extended.ExtendedFactory;
import carisma.modeltype.bpmn2.extended.ExtendedLane;
import carisma.modeltype.bpmn2.extended.ExtendedPackage;
import carisma.modeltype.bpmn2.extended.ExtendedProcess;
import carisma.modeltype.bpmn2.extended.ExtendedTask;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class ExtendedFactoryImpl extends EFactoryImpl implements ExtendedFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static ExtendedFactory init() {
		try {
			ExtendedFactory theExtendedFactory = (ExtendedFactory)EPackage.Registry.INSTANCE.getEFactory("http://carisma/modeltype/bpmn2/extended"); 
			if (theExtendedFactory != null) {
				return theExtendedFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new ExtendedFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ExtendedFactoryImpl() {
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
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT: return createExtendedDocumentRoot();
			case ExtendedPackage.EXTENDED_PROCESS: return createExtendedProcess();
			case ExtendedPackage.EXTENDED_TASK: return createExtendedTask();
			case ExtendedPackage.EXTENDED_LANE: return createExtendedLane();
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
	public ExtendedDocumentRoot createExtendedDocumentRoot() {
		ExtendedDocumentRootImpl extendedDocumentRoot = new ExtendedDocumentRootImpl();
		return extendedDocumentRoot;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ExtendedProcess createExtendedProcess() {
		ExtendedProcessImpl extendedProcess = new ExtendedProcessImpl();
		return extendedProcess;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ExtendedTask createExtendedTask() {
		ExtendedTaskImpl extendedTask = new ExtendedTaskImpl();
		return extendedTask;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ExtendedLane createExtendedLane() {
		ExtendedLaneImpl extendedLane = new ExtendedLaneImpl();
		return extendedLane;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ExtendedPackage getExtendedPackage() {
		return (ExtendedPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static ExtendedPackage getPackage() {
		return ExtendedPackage.eINSTANCE;
	}

} //ExtendedFactoryImpl
