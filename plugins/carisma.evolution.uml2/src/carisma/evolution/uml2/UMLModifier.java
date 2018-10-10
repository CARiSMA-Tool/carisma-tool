/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.evolution.uml2;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil.Copier;

import org.eclipse.uml2.uml.Association;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Profile;
import org.eclipse.uml2.uml.ProfileApplication;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.Type;
import org.eclipse.uml2.uml.UMLPackage;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.AdditiveElement;
import carisma.evolution.CopyElement;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.IModifier;
import carisma.evolution.SubstElement;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlchange.UMLchangeUtil;


/**
 * The modifier for UML models.
 * @author warzecha
 *
 */
public class UMLModifier implements IModifier {
	
	/**
	 * Name of the attribute "owner".
	 */
	private static final String OWNER = "owner";
	
	/**
	 * Name of the attribute "name".
	 */
	private static final String NAME = "name";
	
	/**
	 * Name of the attribute "value".
	 */
	private static final String VALUE = "value";

	/**
	 * A reference to the original UML model.
	 */
	private Resource originalModel = null;
	
	/**
	 * The UML model that was modified with the delta.
	 */
	private Resource modifiedModel = null;
	
	/**
	 * The delta used to modify the model.
	 */
	private Delta usedDelta = null;
	
	/**
	 * The structure containing the mapping from the original model to the modified model, a copy of the original model.
	 */
	private Copier copier = null;
	
	/**
	 * Contains a mapping from the delta elements adding new UML model elements
	 * to the created model elements in the modified model.
	 */
	private Map<AddElement, EObject> addedElements = new HashMap<>();
	
	/**
	 * Returns the model element in the resource representing the original UML model.
	 * @return - the UML Model element in the original model.
	 */
	private Map<DeltaElement, DeltaElement> oldNewDeltaElementsMap;
	
	/**
	 * Creates a new modifier using a given model.
	 * Using this the modified model is just a copy of the original model.
	 * @param newModel - the model to use
	 */
	public UMLModifier(final Resource newModel) {
		this(newModel, null);
	}
	
	/**
	 * Creates a new modifier using a given model.
	 * Using this the modified model is a copy of the original model which
	 * has been modified according to the given delta.
	 * @param newModel - the model to use
	 * @param delta - the delta to use to modify the model
	 */
	public UMLModifier(final Resource newModel, final Delta delta) {
		this.oldNewDeltaElementsMap = new HashMap<>();
		this.usedDelta = delta;
		this.originalModel = newModel;
		this.copier = new Copier();
		this.modifiedModel = edit(newModel, delta);
	}
	
	public DeltaElement get(DeltaElement oldDeltaElement) {
		return this.oldNewDeltaElementsMap.get(oldDeltaElement);
	}
	
	public EObject getAddedElement(AddElement addElement) {
		DeltaElement newAddElement = this.oldNewDeltaElementsMap.get(addElement);
		return this.addedElements.get(newAddElement);
	}

	/**
	 * returns the original model.
	 * @return - the original model
	 */
	public final Model getOriginalModel() {
		if (this.originalModel == null) {
			return null;
		}
		if (this.originalModel.getContents().get(0) instanceof Model) {
			return (Model) this.originalModel.getContents().get(0);
		}
		return null;
	}

	/**
	 * Returns the model element in the resource representing the modified UML model.
	 * @return - the UML Model element in the modified model.
	 */
	public final Model getModifiedModel() {
		if (this.modifiedModel == null) {
			return null;
		}
		if (this.modifiedModel.getContents().get(0) instanceof Model) {
			return (Model) this.modifiedModel.getContents().get(0);
		}
		return null;
	}

	public Copier getMapping() {
		return this.copier;
	}

	/**
	 * returns the used delta element.
	 * @return - the used delta element
	 */
	public final Delta getUsedDelta() {
		return this.usedDelta;
	}
	
	/**
	 * Edits the given model using the given delta by copying the old model and
	 * the delta and modifying the model copy according to the delta copy.
	 * @param oldModel - the model to edit
	 * @param delta - the delta to use
	 * @return - the modified model resource
	 */
	@Override
	public final Resource edit(final Resource oldModel, final Delta delta) {
		if (oldModel == null) {
			return null;
		}
		//TODO: If no delta or an empty delta is given, do we need to copy the model?
		this.originalModel = oldModel;
		this.oldNewDeltaElementsMap.clear();
		this.usedDelta = delta;
		URI oldUri = oldModel.getURI();
		URI newUri = URI.createURI(oldUri.toString().replaceAll("\\.uml$", "_modified.uml"));
		Resource newModel = oldModel.getResourceSet().createResource(newUri);
		this.copier.clear();
		Collection<EObject> allObjects = this.copier.copyAll(oldModel.getContents());
		this.copier.copyReferences();
		newModel.getContents().addAll(allObjects);
		this.modifiedModel = newModel;
		if (delta == null || delta.isEmpty()) {
			return newModel;
		}
		Delta newDelta = copyDelta(delta);
		if (newModel.getContents().get(0) instanceof Model) {
			for (DeltaElement de : newDelta.getContent()) {
				boolean deletedElement = false;
				if (de instanceof AddElement) {
					addElement((AddElement) de);
				} else if (de instanceof CopyElement) {
						copyElement((CopyElement) de);						
				} else if (de instanceof CopyElement) {
					copyElement((CopyElement) de);						
				} else if (de instanceof DelElement) {
					deleteElement((DelElement) de);
					deletedElement = true;
				} else if (de instanceof EditElement) {
					editElement((EditElement) de);						
				} else if (de instanceof SubstElement) {
					substituteElement((SubstElement) de);
					deletedElement = true;
				}
				if (deletedElement) {
					updateOldNewMapping();
				}
			}
		}
		return newModel;
	}

	private void updateOldNewMapping() {
		List<EObject> deletedElements = new ArrayList<>();
		for (EObject oldElem : this.copier.keySet()) {
			EObject elemCopy = this.copier.get(oldElem);
			if (elemCopy == null || elemCopy.eContainer() == null) {
				deletedElements.add(oldElem);
			}
		}
		for (EObject deletedElement : deletedElements) {
			this.copier.remove(deletedElement);
		}		
	}
	
	/**
	 * Saves a model resource to the designated path and filename.
	 * Does nothing if folder doesn't exist or file already exists.
	 * @param modelToSave - the model to save to disk
	 * @param folder - the folder where the file should be saved to
	 * @param filename - the model filename to use
	 */
// FIXME: Extract saveModel to UMLModelLoader
	public final static void saveModel(final Model modelToSave, final File folder, final String filename, final boolean removeUMLchange) {
		// TODO: Better handling of saving models (overwrite old file, option)
		if (folder != null && !"".equals(filename) && modelToSave != null) {
			if (!folder.exists()) {
				Logger.log(LogLevel.ERROR, "Target folder doesn't exist");
				return;
			}
			File newFile = new File(folder.getAbsolutePath() + File.separator + filename);
			if (newFile.exists()) {
				Logger.log(LogLevel.ERROR, "Target model file already exists.");
				return;
			}
			if (removeUMLchange) {
				removeUMLchange(modelToSave);
			}
			Resource modelResource = modelToSave.eResource();
			modelResource.setURI(URI.createFileURI(newFile.getAbsolutePath()));
			try {
				modelResource.save(new HashMap<String, Object>());
			} catch (IOException e) {
				Logger.log(LogLevel.ERROR, "", e);
			}
		}
	}

	private static void removeUMLchange(final Model theModel) {
		if (UMLHelper.isProfileApplied(theModel, UMLchange.DESCRIPTOR)) {
			for (Element elem : UMLchangeUtil.getStereotypedElements(theModel)) {
				for (Stereotype appliedStereo : UMLchangeUtil.getAppliedStereotypes(elem)) {
					elem.unapplyStereotype(appliedStereo);
				}
			}
			for (ProfileApplication profileApp : theModel.getAllProfileApplications()) {
				Profile appliedProfile = profileApp.getAppliedProfile();
				if (appliedProfile.getName().contains(UMLchange.DESCRIPTOR.getProfileName())) {
					profileApp.getApplyingPackage().unapplyProfile(appliedProfile);
				}
			}
		}
	}

	/**
	 * Copies the given delta according to the mapping between the original and modified model (the copy) and returns the copied delta.
	 * @param oldDelta - the old delta to copy
	 * @return - the delta copy
	 */
	@Override
	public final Delta copyDelta(final Delta oldDelta) {
		List<DeltaElement> newDeltaContent = new ArrayList<>();
		for (DeltaElement oldDe : oldDelta.getContent()) {
			if (oldDe instanceof AddElement) {
				AddElement newAddElem = copyAddElement((AddElement) oldDe, this.oldNewDeltaElementsMap);
				newDeltaContent.add(newAddElem);				
			}
			if (oldDe instanceof CopyElement) {
				CopyElement newCopyElem = copyCopyElement((CopyElement) oldDe, this.oldNewDeltaElementsMap);
				newDeltaContent.add(newCopyElem);
			}
			if (oldDe instanceof DelElement) {
				DelElement newDelElem = copyDelElement((DelElement) oldDe);
				newDeltaContent.add(newDelElem);
				this.oldNewDeltaElementsMap.put(oldDe, newDelElem);
			}
			if (oldDe instanceof EditElement) {
				EditElement newEditElem = copyEditElement((EditElement) oldDe, this.oldNewDeltaElementsMap);
				newDeltaContent.add(newEditElem);
			}
			if (oldDe instanceof SubstElement) {
				SubstElement newSubstElem = copySubstElement((SubstElement) oldDe, this.oldNewDeltaElementsMap);
				newDeltaContent.add(newSubstElem);
			}
		}
		return new Delta(oldDelta.getChangeIDs(), newDeltaContent);
	}
	
	/**
	 * Copies a single DelElement according to the original/copy mapping.
	 * @param oldDelElem - the DelElement to copy.
	 * @return - the copied DelElement
	 */
	private DelElement copyDelElement(final DelElement oldDelElem) {
		EObject newTarget = findOrCreateNewTarget(oldDelElem.getTarget());
		DelElement newDelElem = new DelElement(newTarget);
		for (EObject accompDel : oldDelElem.getAccompanyingDeletions()) {
			newDelElem.addDeletion(findOrCreateNewTarget(accompDel));
		}
		return newDelElem;
	}
	
	/**
	 * Copies a single AddElement and its content according to the original/copy mapping.
	 * @param oldAddElement - the AddElement to copy.
	 * @param oldNewDeltaElems - a mapping from old to new DeltaElements needed for correct copying
	 * @return - the copy of the AddElement
	 */
	private AddElement copyAddElement(final AddElement oldAddElement, final Map<DeltaElement, DeltaElement> oldNewDeltaElems) {
		EObject newTarget = findOrCreateNewTarget(oldAddElement.getTarget());
		AdditiveElement newParent = (AdditiveElement) oldNewDeltaElems.get(oldAddElement.getParent());
		AddElement newAddElem = new AddElement(newTarget, oldAddElement.getMetaClass(), newParent);
		oldNewDeltaElems.put(oldAddElement, newAddElem);
		for (String oldKey : oldAddElement.getValues().keySet()) {
			Object oldValue = oldAddElement.getValues().get(oldKey);
			 if (oldValue instanceof Element) {
				newAddElem.addKeyValuePair(oldKey, this.copier.get(oldValue));
			} else {
				newAddElem.addKeyValuePair(oldKey, oldValue);
			}
		}
		for (AddElement containedElement : oldAddElement.getContent()) {
			AddElement containedElementCopy = copyAddElement(containedElement, oldNewDeltaElems);
			newAddElem.addContainedElement(containedElementCopy);
		}
		return newAddElem;
	}
	
	/**
	 * For a given StereotypeApplication of the original model, it returns the corresponding StereotypeApplication in the modified model.
	 */
	public StereotypeApplication getCorrespondingStereotypeApplication(StereotypeApplication originalStereotypeApplication) {
		Element oldElement = originalStereotypeApplication.getExtendedElement();
		Element newElement = (Element) getMapping().get(oldElement);
		if (newElement != null) {
			Stereotype oldStereotype = originalStereotypeApplication.getAppliedStereotype();
			Stereotype newStereotype = UMLHelper.getAppliedStereotype(newElement, oldStereotype.getName());
			if (newStereotype != null) {
				return new StereotypeApplication(newStereotype, newElement);
			}
		}
		return null;
	}
	/**
	 * Copies a single SubstElement and its components according to the original/copy mapping.
	 * @param oldSubstElem - the SubstElement to copy.
	 * @param oldNewDeltaElems - a mapping from old to new DeltaElements needed for correct copying
	 * @return - the copy of the SubstElement
	 */
	private SubstElement copySubstElement(final SubstElement oldSubstElem, final Map<DeltaElement, DeltaElement> oldNewDeltaElems) {
		List<AddElement> components = new ArrayList<>();
		EObject newTarget = findOrCreateNewTarget(oldSubstElem.getTarget());
		SubstElement newSubstElem = new SubstElement(newTarget, components);
		oldNewDeltaElems.put(oldSubstElem, newSubstElem);
		for (AddElement oldComponent : oldSubstElem.getComponents()) {
			AddElement newComponent = copyAddElement(oldComponent, oldNewDeltaElems); 
			components.add(newComponent);
		}
		newSubstElem.replaceComponents(components);
		return newSubstElem;
	}
	/**
	 * Copies a single EditElement and its components according to the original/copy mapping.
	 * @param oldEditElem - the EditElement to copy
	 * @param oldNewDeltaElems - a mapping from old to new DeltaElements needed for correct copying
	 * @return - the copy of the EditElement
	 */
	private EditElement copyEditElement(final EditElement oldEditElem, final Map<DeltaElement, DeltaElement> oldNewDeltaElems) {
		EObject newTarget = findOrCreateNewTarget(oldEditElem.getTarget());
		EditElement newEditElem = new EditElement(newTarget);
		oldNewDeltaElems.put(oldEditElem, newEditElem);
		for (String oldKey : oldEditElem.getValues().keySet()) {
			Object oldValue = oldEditElem.getValues().get(oldKey);
			if (oldValue instanceof Element) {
				 newEditElem.addKeyValuePair(oldKey, this.copier.get(oldValue));
			} else {
				newEditElem.addKeyValuePair(oldKey, oldValue);
			}
		}
		return newEditElem;
	}

	/**
	 * Copies a single CopyElement and its components according to the original/copy mapping.
	 * @param oldCopyElem - the CopyElement to copy
	 * @param oldNewDeltaElems - a mapping from old to new DeltaElements needed for correct copying
	 * @return - the copy of the CopyElement
	 */
	private CopyElement copyCopyElement(final CopyElement oldCopyElem, final Map<DeltaElement, DeltaElement> oldNewDeltaElems) {
		EObject newTarget = findOrCreateNewTarget(oldCopyElem.getTarget());
		EObject newTargetOwner = findOrCreateNewTarget(oldCopyElem.getReceivingElement());
		CopyElement newCopyElem = new CopyElement(newTarget, newTargetOwner);
		oldNewDeltaElems.put(oldCopyElem, newCopyElem);
		for (String oldKey : oldCopyElem.getChangedValues().keySet()) {
			Object oldValue = oldCopyElem.getChangedValues().get(oldKey);
			if (oldValue instanceof Element) {
				 newCopyElem.addChangedValuePair(oldKey, this.copier.get(oldValue));
			} else {
				newCopyElem.addChangedValuePair(oldKey, oldValue);
			}
		}
		return newCopyElem;
	}
	/**
	 * Returns the new version of a StereotypeApplication according to the original/copy mapping.
	 * @param oldApp - the Application to look for.
	 * @return - the new version of the StereotypeApplication
	 */
	private StereotypeApplication getNewStereotypeApplication(final StereotypeApplication oldApp) {
		Element oldExtendedElem = oldApp.getExtendedElement();
		Element newExtendedElem = (Element) this.copier.get(oldExtendedElem);
		if (newExtendedElem != null) {
			return UMLHelper.getStereotypeApplication(newExtendedElem, oldApp.getStereotypeName());
		}
		return null;
	}
	/**
	 * Returns a TaggedValue according to the original/copy mapping.
	 * @param oldTagValue - the TaggedValue to look for
	 * @return - the new version of the TaggedValue
	 */
	private TaggedValue getNewTaggedValue(final TaggedValue oldTagValue) {
		Element newExtended = (Element) this.copier.get(oldTagValue.getCorrespondingApplication().getExtendedElement());
		if (newExtended != null) {
			return UMLHelper.getStereotypeApplication(newExtended, oldTagValue.getStereotypeName()).getTaggedValue(oldTagValue.getName());
		}
		return null;
	}
	/**
	 * Checks the original/copy mapping for the given target and copies according to type.
	 * @param oldTarget - the EObject reference to find or create
	 * @return - the appropriate reference
	 */
	private EObject findOrCreateNewTarget(final EObject oldTarget) {
		if (oldTarget != null) {
			if (oldTarget instanceof Element) {
				return this.copier.get(oldTarget);
			} else if (oldTarget instanceof StereotypeApplication) {
				return getNewStereotypeApplication((StereotypeApplication) oldTarget);
			} else if (oldTarget instanceof TaggedValue) {
				return getNewTaggedValue((TaggedValue) oldTarget);
			}
		}
		return null;
	}
	/**
	 * Given the list of elements from the old model, a list of those elements which still
	 * exist in the modified model is built.
	 * @param oldElements - the elements to check
	 * @return - elements that are still in the modified model
	 */
	public final List<Element> getRemainingElements(final List<Element> oldElements) {
		List<Element> remainingElements = new ArrayList<>();
		for (Element oldElement : oldElements) {
			Model newModel = (Model) this.copier.get(oldElement.getModel());
			Element copiedElement = (Element) this.copier.get(oldElement);
			if (newModel.allOwnedElements().contains(copiedElement)) {
				remainingElements.add(copiedElement);
			}
		}
		return remainingElements;
	}
	/**
	 * Removes the element described by the DelElement from the given model.
	 * @param del -
	 */
	public final void deleteElement(final DelElement del) {
		if (del != null) {
			EObject target = del.getTarget();
			Model model = UMLHelper.getModel(target);
			if (model != null) {
				if (target instanceof Element) {
					Element elem = (Element) target;
					elem.destroy();
					for (EObject alsoDeleted : del.getAccompanyingDeletions()) {
						deleteElement(new DelElement(alsoDeleted));
					}
				} else if (target instanceof StereotypeApplication) {
					StereotypeApplication applic = (StereotypeApplication) target;
					Element extendedElem = applic.getExtendedElement();
					Stereotype appliedStereo = applic.getAppliedStereotype();
					if (extendedElem != null 
							&& appliedStereo != null) {
						extendedElem.unapplyStereotype(appliedStereo);						
					}
				} else if (target instanceof TaggedValue) {
					TaggedValue tagValue = (TaggedValue) target;
					tagValue.removeValue();
				}
			}
		}
	}
		
// TODO: Beschriebene Elemente, die auf andere beschriebene Elemente aufbauen
// aber "sideways", nicht content
	/**
	 * Adds the given AddElement to the model.
	 * @param add - the AddElement to process
	 */
	public final void addElement(final AddElement add) {
		if (add != null) {
			EObject addedElem = UMLModifierElementFactory.createElement(add);
			this.addedElements.put(add, addedElem);
			add.updateContent(addedElem);
			addContent(add);
		}
	}
//TODO: Nicht blosses Loeschen und Hinzufuegen, sondern Connections beachten
	/**
	 * Processes the given SubstElement.
	 * @param subst - the SubstElement to process
	 */
	public final void substituteElement(final SubstElement subst) {
		DelElement del = new DelElement(subst.getTarget());
		del.replaceAccompanyingDeletions(subst.getAccompanyingDeletions());
		deleteElement(del);
		for (AddElement add : subst.getComponents()) {
			addElement(add);
		}
	}
	
	/**
	 * edits the content of the model according to the EditElement.
	 * @param editElem - the EditElement
	 */
	public final void editElement(final EditElement editElem) {
		if ((editElem != null) && (editElem.getTarget() != null)) {
			EObject target = editElem.getTarget();
			if (target instanceof StereotypeApplication) {
				editStereotypeApplication(editElem);
			} else if (target instanceof TaggedValue) {
				editTaggedValue(editElem);
			} else {
				editUMLElement(editElem);
			}
		}
	}
	
	private static void editUMLElement(final EditElement edit) {
		Element targetElement = (Element) edit.getTarget();
		for (String key : edit.getValues().keySet()) {
			Object mapValue = edit.getValues().get(key);
			Object realValue = UMLModifierElementFactory.findRealValue(UMLHelper.getModel(targetElement),key, mapValue);					
			if (key.equals(OWNER) && realValue instanceof Element) {
				UMLModifierElementFactory.insertContainmentRelationship((Element) realValue, targetElement);
			} else if (targetElement instanceof Association && key.startsWith("end")) {
				//FIXME: just doesn't work; don't know of a way to identify the end
				//example: an element has an association to itself; how to know which end?
				// so associations and association classes can't be edited, moved or copied
//				editAssociationEnd((Association) targetElement, realValue);
				editAssociationEnd();
			} else {
				UMLModifierElementFactory.editStructuralFeatureValue(targetElement, key, realValue, true);
			}
		}
	}
	
	private void editStereotypeApplication(final EditElement edit) {
		if (edit.getValues().containsKey(OWNER)) {
			StereotypeApplication movedApp = (StereotypeApplication) edit.getTarget();
			Element newOwner = (Element) edit.getValues().get(OWNER);
			AddElement addApp = null;
			if (!newOwner.isStereotypeApplied(movedApp.getAppliedStereotype())) {
				addApp = new AddElement(newOwner, UMLPackage.eINSTANCE.getStereotype(), null);
				addApp.addKeyValuePair(NAME, movedApp.getAppliedStereotype().getQualifiedName());
				addElement(addApp);
			}
			StereotypeApplication theApp = new StereotypeApplication(movedApp.getAppliedStereotype(), newOwner);
			for (TaggedValue movedValue : movedApp.getTaggedValues()) {
				AddElement addValue = new AddElement(theApp, UMLPackage.eINSTANCE.getProperty(), null);
				addValue.addKeyValuePair(NAME, movedValue.getName());
				addValue.addKeyValuePair(VALUE, movedValue.getValue());
				addElement(addValue);					
			}
			DelElement delOldApp = new DelElement(movedApp);
			deleteElement(delOldApp);
		}
	}
	
	private void editTaggedValue(final EditElement edit) {
		TaggedValue changedValue = (TaggedValue) edit.getTarget();
		if (edit.getValues().containsKey(OWNER)) {
			TaggedValue movedValue = changedValue;
			Element targetElement = 
					(Element) edit.getValues().get(OWNER);
			StereotypeApplication targetApp = 
					new StereotypeApplication(
							movedValue.getStereotype(),
							targetElement);
// FIXME: What if targetApp is not yet applied?
			AddElement moveValues =
					new AddElement(
							targetApp,
							UMLPackage.eINSTANCE.getProperty(),
							null);
			moveValues.addKeyValuePair(NAME, movedValue.getName());
			moveValues.addKeyValuePair(VALUE, movedValue.getValue());
			addElement(moveValues);
		} else {
			TaggedValue editedValue = changedValue;
			DelElement delOldValues = new DelElement(editedValue);
			deleteElement(delOldValues);
			AddElement newValues = 
			new AddElement(
					editedValue.getCorrespondingApplication(),
					UMLPackage.eINSTANCE.getProperty(),
					null);
			newValues.addKeyValuePair(NAME, editedValue.getName());
			newValues.addKeyValuePair(VALUE, edit.getValues().get(VALUE));
			addElement(newValues);
		}
	}

	public final void copyElement(final CopyElement copyElem) {
		EObject elementToCopy = copyElem.getTarget();
		Element receivingElement = (Element) copyElem.getReceivingElement();
		if (elementToCopy instanceof Element) {
			Copier elementCopier = new Copier();
			elementCopier.clear();
			Element elemCopy = (Element) elementCopier.copy(elementToCopy);
			elementCopier.copyReferences();
//FIXME: What if I copy an element having relationships? Are they copied as well? Does the copy have a reference to the old
//			relations?
			UMLModifierElementFactory.insertContainmentRelationship(receivingElement, elemCopy);
			if (elemCopy.getModel() == null && elemCopy.getOwner() == null) {
				Logger.log(LogLevel.ERROR, "Tried to copy an element but couldn't insert it into new owner!");
			}
			EditElement editValues = new EditElement(elemCopy);
			editValues.replaceValues(copyElem.getChangedValues());
			editElement(editValues);
		} else if (elementToCopy instanceof StereotypeApplication) {
			StereotypeApplication appToCopy = (StereotypeApplication) elementToCopy;
			StereotypeApplication receivingApp = UMLHelper.getStereotypeApplication(receivingElement, appToCopy.getStereotypeName());
			if (receivingApp == null) {
				receivingApp = UMLHelper.applyStereotype(receivingElement, appToCopy.getQualifiedStereotypeName());
			}
			for (TaggedValue tagValueToCopy : appToCopy.getTaggedValues()) {
				receivingApp.getTaggedValue(tagValueToCopy.getName()).removeValue();
				receivingApp.getTaggedValue(tagValueToCopy.getName()).setValue(tagValueToCopy.getValue());
			}
		} else if (elementToCopy instanceof TaggedValue) {
			TaggedValue tagValueToCopy = (TaggedValue) elementToCopy;
			StereotypeApplication receivingApp = UMLHelper.getStereotypeApplication(receivingElement, tagValueToCopy.getStereotypeName());
			if (receivingApp != null) {
				TaggedValue receivingTag = receivingApp.getTaggedValue(tagValueToCopy.getName());
				if (receivingTag != null) {
					receivingTag.removeValue();
					receivingTag.setValue(tagValueToCopy.getValue());
				}
			}
		}
	}
	
	/**
	 * changes the target of an Association.
	 * @param association - the association to change the target for
	 * @param edit - the editElement with the changes
	 * @return - true if the new target could be set, false otherwise
	 */
// TODO Look at me
//	private boolean editAssociationEnd(Association association, final Object realValue) {
	private static boolean editAssociationEnd() {
		return false;
//		if (association == null) {
//			return false;
//		}

//		Model theModel = UMLHelper.getModel(association);
//		// edit enthält end1=ALTES::ZIEL;;;NEUES::ZIEL
//		if (realValue instanceof Map<?,?>) {
//			Map<Element, Element> oldNewEnd = (Map<Element,Element>) realValue;
//			for (Element oldEnd : oldNewEnd.keySet()) {
//				
//			}
//			
//		}
//		String assocName = association.getName();
//		String sourceName = association.getMembers().get(0).getName();
//		String sourceEndName = association.getMemberEnds().get(0).getName();
//		AggregationKind sourceEndKind = association.getMemberEnds().get(0).getAggregation();
//		int sourceLowerBound = ((PropertyImpl) association.getMembers().get(0)).lowerBound();
//		int sourceUpperBound = ((PropertyImpl) association.getMembers().get(0)).upperBound();
//		boolean sourceNavigable = ((Property) association.getMembers().get(0)).isNavigable();	
//		
//		String targetName = (String) edit.getValues().get("target");
//		String targetEndName = getEndName((String) edit.getValues().get("targetEndName"));
//		AggregationKind targetEndKind = getAggregationKind((String) edit.getValues().get("targetEndKind"), AggregationKind.NONE_LITERAL);
//		int targetLowerBound = getBound((String) edit.getValues().get("targetLowerBound"));
//		int targetUpperBound = getBound((String) edit.getValues().get("targetUpperBound"));
//		boolean targetNavigable = getBoolean((String) edit.getValues().get("sourceNavigable"));
//		
//		Classifier source = UMLHelper.getElementOfNameAndType(theModel, sourceName, Classifier.class);
//		Classifier target = UMLHelper.getElementOfNameAndType(theModel, targetName, Classifier.class);
//		
//		if (source != null && target != null) {
//			association =
//					source.createAssociation(
//							sourceNavigable, 
//							sourceEndKind, 
//							sourceEndName, sourceLowerBound, sourceUpperBound, 
//							target, 
//							targetNavigable,
//							targetEndKind,
//							targetEndName, targetLowerBound, targetUpperBound);
//			if (assocName != null) {
//				association.setName(assocName);
//			}
//			return true;
//		}
//		return false;
	}
	
	public static final void changeBinaryAssociationEnds(final Association a, final Type oldSource, final List<NamedElement> newSources, final Type oldTarget, final List<NamedElement> newTargets) {
		if (a.isBinary()) {
			for (Property p : a.getMemberEnds()) {
				Type t = p.getType();
				if (t.equals(oldSource)) {
					if (newSources != null && !newSources.isEmpty()) {
						NamedElement newSource = newSources.get(0);
						if (newSource instanceof Type) {
							p.setType((Type) newSource);
						}
					}
				} else if (t.equals(oldTarget) 
						&& newTargets != null 
						&& !newTargets.isEmpty()) {
					NamedElement newTarget = newTargets.get(0);
					if (newTarget instanceof Type) {
						p.setType((Type) newTarget);
					}
				}
			}
		}
	}
	
/**
	 * Adds the content of the given AddElement to the model.
	 * @param container - the AddElement to generate the content for
	 */
	public final void addContent(final AddElement container) {
		for (AddElement containedElem : container.getContent()) {
			EObject instancedElem = UMLModifierElementFactory.createElement(containedElem);
			containedElem.updateContent(instancedElem);
			addContent(containedElem);
		}
	}
	
}