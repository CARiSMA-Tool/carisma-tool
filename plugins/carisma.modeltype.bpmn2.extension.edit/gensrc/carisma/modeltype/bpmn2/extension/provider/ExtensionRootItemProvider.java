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
package carisma.modeltype.bpmn2.extension.provider;



import java.util.Collection;
import java.util.List;

import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.ResourceLocator;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.edit.provider.IEditingDomainItemProvider;
import org.eclipse.emf.edit.provider.IItemLabelProvider;
import org.eclipse.emf.edit.provider.IItemPropertyDescriptor;
import org.eclipse.emf.edit.provider.IItemPropertySource;
import org.eclipse.emf.edit.provider.IStructuredItemContentProvider;
import org.eclipse.emf.edit.provider.ITreeItemContentProvider;
import org.eclipse.emf.edit.provider.ItemProviderAdapter;
import org.eclipse.emf.edit.provider.ViewerNotification;

import carisma.modeltype.bpmn2.extension.ExtensionFactory;
import carisma.modeltype.bpmn2.extension.ExtensionPackage;
import carisma.modeltype.bpmn2.extension.ExtensionRoot;

/**
 * This is the item provider adapter for a {@link carisma.modeltype.bpmn2.extension.ExtensionRoot} object.
 * <!-- begin-user-doc -->
 * <!-- end-user-doc -->
 * @generated
 */
public class ExtensionRootItemProvider
	extends ItemProviderAdapter
	implements
		IEditingDomainItemProvider,
		IStructuredItemContentProvider,
		ITreeItemContentProvider,
		IItemLabelProvider,
		IItemPropertySource {
	/**
	 * This constructs an instance from a factory and a notifier.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ExtensionRootItemProvider(AdapterFactory adapterFactory) {
		super(adapterFactory);
	}

	/**
	 * This returns the property descriptors for the adapted class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public List<IItemPropertyDescriptor> getPropertyDescriptors(Object object) {
		if (itemPropertyDescriptors == null) {
			super.getPropertyDescriptors(object);

		}
		return itemPropertyDescriptors;
	}

	/**
	 * This specifies how to implement {@link #getChildren} and is used to deduce an appropriate feature for an
	 * {@link org.eclipse.emf.edit.command.AddCommand}, {@link org.eclipse.emf.edit.command.RemoveCommand} or
	 * {@link org.eclipse.emf.edit.command.MoveCommand} in {@link #createCommand}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Collection<? extends EStructuralFeature> getChildrenFeatures(Object object) {
		if (childrenFeatures == null) {
			super.getChildrenFeatures(object);
			childrenFeatures.add(ExtensionPackage.Literals.EXTENSION_ROOT__TASK);
			childrenFeatures.add(ExtensionPackage.Literals.EXTENSION_ROOT__WORK_ITEM);
			childrenFeatures.add(ExtensionPackage.Literals.EXTENSION_ROOT__PERFORMER);
			childrenFeatures.add(ExtensionPackage.Literals.EXTENSION_ROOT__ROLE);
			childrenFeatures.add(ExtensionPackage.Literals.EXTENSION_ROOT__LANE);
			childrenFeatures.add(ExtensionPackage.Literals.EXTENSION_ROOT__SELECTION);
		}
		return childrenFeatures;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EStructuralFeature getChildFeature(Object object, Object child) {
		// Check the type of the specified child object and return the proper feature to use for
		// adding (see {@link AddCommand}) it as a child.

		return super.getChildFeature(object, child);
	}

	/**
	 * This returns ExtensionRoot.gif.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object getImage(Object object) {
		return overlayImage(object, getResourceLocator().getImage("full/obj16/ExtensionRoot"));
	}

	/**
	 * This returns the label text for the adapted class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getText(Object object) {
		return getString("_UI_ExtensionRoot_type");
	}

	/**
	 * This handles model notifications by calling {@link #updateChildren} to update any cached
	 * children and by creating a viewer notification, which it passes to {@link #fireNotifyChanged}.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void notifyChanged(Notification notification) {
		updateChildren(notification);

		switch (notification.getFeatureID(ExtensionRoot.class)) {
			case ExtensionPackage.EXTENSION_ROOT__TASK:
			case ExtensionPackage.EXTENSION_ROOT__WORK_ITEM:
			case ExtensionPackage.EXTENSION_ROOT__PERFORMER:
			case ExtensionPackage.EXTENSION_ROOT__ROLE:
			case ExtensionPackage.EXTENSION_ROOT__LANE:
			case ExtensionPackage.EXTENSION_ROOT__SELECTION:
				fireNotifyChanged(new ViewerNotification(notification, notification.getNotifier(), true, false));
				return;
		}
		super.notifyChanged(notification);
	}

	/**
	 * This adds {@link org.eclipse.emf.edit.command.CommandParameter}s describing the children
	 * that can be created under this object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected void collectNewChildDescriptors(Collection<Object> newChildDescriptors, Object object) {
		super.collectNewChildDescriptors(newChildDescriptors, object);

		newChildDescriptors.add
			(createChildParameter
				(ExtensionPackage.Literals.EXTENSION_ROOT__TASK,
				 ExtensionFactory.eINSTANCE.createTask()));

		newChildDescriptors.add
			(createChildParameter
				(ExtensionPackage.Literals.EXTENSION_ROOT__WORK_ITEM,
				 ExtensionFactory.eINSTANCE.createWorkItem()));

		newChildDescriptors.add
			(createChildParameter
				(ExtensionPackage.Literals.EXTENSION_ROOT__PERFORMER,
				 ExtensionFactory.eINSTANCE.createPerformer()));

		newChildDescriptors.add
			(createChildParameter
				(ExtensionPackage.Literals.EXTENSION_ROOT__ROLE,
				 ExtensionFactory.eINSTANCE.createRole()));

		newChildDescriptors.add
			(createChildParameter
				(ExtensionPackage.Literals.EXTENSION_ROOT__LANE,
				 ExtensionFactory.eINSTANCE.createLane()));

		newChildDescriptors.add
			(createChildParameter
				(ExtensionPackage.Literals.EXTENSION_ROOT__SELECTION,
				 ExtensionFactory.eINSTANCE.createSelection()));
	}

	/**
	 * Return the resource locator for this item provider's resources.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ResourceLocator getResourceLocator() {
		return ExtensionEditPlugin.INSTANCE;
	}

}
