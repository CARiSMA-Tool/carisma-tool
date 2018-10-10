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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.bpmn2.ConversationLink;
import org.eclipse.bpmn2.impl.TaskImpl;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

import carisma.modeltype.bpmn2.extended.ExtendedPackage;
import carisma.modeltype.bpmn2.extended.ExtendedTask;
import carisma.modeltype.bpmn2.extension.WorkItem;


/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Task</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extended.impl.ExtendedTaskImpl#getWorkItem <em>Work Item</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ExtendedTaskImpl extends TaskImpl implements ExtendedTask {
	/**
	 * The cached value of the '{@link #getWorkItem() <em>Work Item</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getWorkItem()
	 * @generated
	 * @ordered
	 */
	protected EList<WorkItem> workItem;

	/**
	 * The value of the incomingConversationLinks reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	protected List<ConversationLink> incomingConversationLinks;
	
	/**
	 * The value of the outgoingConversationLinks reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	protected List<ConversationLink> outgoingConversationLinks;
	
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ExtendedTaskImpl() {
		super();
		this.incomingConversationLinks = new ArrayList<>();
		this.outgoingConversationLinks = new ArrayList<>();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExtendedPackage.Literals.EXTENDED_TASK;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<WorkItem> getWorkItem() {
		if (this.workItem == null) {
			this.workItem = new EObjectResolvingEList<>(WorkItem.class, this, ExtendedPackage.EXTENDED_TASK__WORK_ITEM);
		}
		return this.workItem;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExtendedPackage.EXTENDED_TASK__WORK_ITEM:
				return getWorkItem();
			default:
				return super.eGet(featureID, resolve, coreType);
		}
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
			case ExtendedPackage.EXTENDED_TASK__WORK_ITEM:
				getWorkItem().clear();
				getWorkItem().addAll((Collection<? extends WorkItem>)newValue);
				break;
			default:
				super.eSet(featureID, newValue);
				break;
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ExtendedPackage.EXTENDED_TASK__WORK_ITEM:
				getWorkItem().clear();
				break;
			default:
				super.eUnset(featureID);
				break;
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ExtendedPackage.EXTENDED_TASK__WORK_ITEM:
				return this.workItem != null && !this.workItem.isEmpty();
			default:
				return super.eIsSet(featureID);
		}
	}
	
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	@Override
	public List<ConversationLink> getIncomingConversationLinks() {
        return this.incomingConversationLinks;
    }
	
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public void setIncomingConversationLinks(List<ConversationLink> incomingConversationLinks) {
		this.incomingConversationLinks = incomingConversationLinks;
    }
	
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	@Override
	public List<ConversationLink> getOutgoingConversationLinks() {
        return this.incomingConversationLinks;
    }
	
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public void setOutgoingConversationLinks(List<ConversationLink> outgoingConversationLinks) {
		this.outgoingConversationLinks = outgoingConversationLinks;
    }

} //ExtendedTaskImpl
