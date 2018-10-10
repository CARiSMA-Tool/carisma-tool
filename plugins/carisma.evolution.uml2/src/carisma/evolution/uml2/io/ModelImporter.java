package carisma.evolution.uml2.io;

import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.AdditiveElement;
import carisma.evolution.CopyElement;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.io.datatype.ExportAddElement;
import carisma.evolution.uml2.io.datatype.ExportCopyElement;
import carisma.evolution.uml2.io.datatype.ExportDelElement;
import carisma.evolution.uml2.io.datatype.ExportDelta;
import carisma.evolution.uml2.io.datatype.ExportDeltaElement;
import carisma.evolution.uml2.io.datatype.ExportEditElement;
import carisma.evolution.uml2.io.datatype.ExportExtTag;
import carisma.evolution.uml2.io.datatype.ExportExtTagNamedElement;
import carisma.evolution.uml2.io.datatype.ExportExtTagStereotype;
import carisma.evolution.uml2.io.datatype.ExportExtTagTaggedValue;
import carisma.evolution.uml2.io.datatype.ExportSubstElement;
import carisma.evolution.uml2.io.datatype.ExporterUtility;
import carisma.evolution.uml2.io.datatype.XStreamAlias;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.InvalidMetaclassException;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;

import com.thoughtworks.xstream.XStream;


/** Used to import XML based description of a Delta.
 * 
 * @author bberghoff
 *
 */
public class ModelImporter {
	//TODO: Shouldn't this class implement CarismaCheckWithID as it is registered at the extension point carisma.carismacheck? 
	
	/**
	 * The Resource on which the Delta is based.
	 */
	private Resource modelres = null;
	/**
	 * The Model on which the Delta is based.
	 */
	private Model model;


	/** Returns a Delta described in inputXML w.r.t the Resource.
	 *  
	 * @param inputXML Automatic generated XML File. Use carisma.check.modelexporter.check.writeDeltaToFile(..) to create one.
	 * @param modelRes Corresponding Model Resource.
	 * @return A Delta. 
	 * If one of the DeltaElements can not be imported the other will still be.
	 * null if the input is null or malformed.
	 */
	public final Delta getDelta(final File inputXML, final Resource modelRes) {
		if (inputXML == null || modelRes == null) { 
			return null;
		}
		this.model = (Model) modelRes.getContents().get(0);
		this.modelres = modelRes;
		
		XStream stream = XStreamAlias.getXStream();
		ExportDelta expDelta;
		try (FileReader fileReader = new FileReader(inputXML)){
			expDelta = (ExportDelta) stream.fromXML(fileReader);
		
			ArrayList<DeltaElement> deltaElements = new ArrayList<>();
			for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
				
				EObject target = getTarget(expDeltaEle.getTarget());
	
				if (target != null) {
					DeltaElement deltaEle = null;
					if (expDeltaEle instanceof ExportAddElement) { 
						deltaEle = getAddElement(
								(ExportAddElement) expDeltaEle, target,	null);
					} else if (expDeltaEle instanceof ExportDelElement) {
						deltaEle = getDelElement(target);
					} else if (expDeltaEle instanceof ExportSubstElement) {
						deltaEle = getSubstElement(
								(ExportSubstElement) expDeltaEle, target);
					} else if (expDeltaEle instanceof ExportEditElement) {
						deltaEle = getEditElement(
								(ExportEditElement) expDeltaEle, target);
					} else if (expDeltaEle instanceof ExportCopyElement) {
						deltaEle = getCopyElement(
								(ExportCopyElement) expDeltaEle, target);
					}
					deltaElements.add(deltaEle);
				}
	
			}
			return new Delta(expDelta.getUsedChangesID(), deltaElements);
		} catch (Exception e) { 
			Logger.log(LogLevel.ERROR, "Error while reading the input XML File", e);
			return null;
		}
	}
	
	/** Returns the EObject represented by expTarget.
	 *  
	 * @param expTarget .
	 * @return Returns the EObject represented by expTarget.
	 */
	private EObject getTarget(final ExportExtTag expTarget) {
		EObject target = null;
		if (expTarget instanceof ExportExtTagTaggedValue) {
			target = getTaggedValueTarget(
					(ExportExtTagTaggedValue) expTarget);

		} else if (expTarget instanceof ExportExtTagStereotype) {
			target = getStereoTarget(
					(ExportExtTagStereotype) expTarget);
		} else if (expTarget instanceof ExportExtTagNamedElement) {
			target = getTargetByNameAndType(
					(ExportExtTagNamedElement) expTarget);
		}
		return target;
	}

	/** Creates a CopyElement w.r.t ExportCopyElement.
	 * 
	 * @param cpEle The ExportCopyElement to be transformed.
	 * @param target the actual target in the model.  
	 * @return an CopyElement
	 */
	private CopyElement getCopyElement(final ExportCopyElement cpEle, final EObject target) {
		CopyElement cp = new CopyElement(target, getTarget(cpEle.getTargetOwner()));
		cp.replaceChangedValues(new HashMap<>(cpEle.getChangedValues()));
		
		return cp;
	}

	/** Creates a DelElement with a given target.
	 * 
	 * @param target the target of the new DelElement
	 * @return created DelElement.
	 */
	private static DelElement getDelElement(final EObject target) {
		return new DelElement(target);
	}

	/** Creates an EditElement w.r.t. ExportEditElement.
	 * 
	 * @param ed The ExportEditElement to be transformed.
	 * @param target the actual target in the model.  
	 * @return an EditElement
	 */
	private static EditElement getEditElement(final ExportEditElement ed, final EObject target) {
		EditElement editEle = new EditElement(target);
		editEle.replaceValues(ExporterUtility.getValuesWithNull(ed.getValues()));
		
		return editEle;
	}
	
	/** Creates a SubstElement w.r.t an ExportSubstElement.
	 * 
	 * @param ed The ExportSubstElement to be transformed.
	 * @param target the actual target in the model.  
	 * @return a SubstElement.
	 */
	private SubstElement getSubstElement(final ExportSubstElement ed, final EObject target) {
		ArrayList<AddElement> components = new ArrayList<>();
		SubstElement subEle = new SubstElement(target, components);
		for (ExportAddElement component : ed.getComponents()) {
			components.add(getAddElement(component, target, subEle));
		}
		
		subEle.replaceComponents(components);
		//TODO addAccompanyingDeletions
	//	subEle.replaceAccompanyingDeletions(ed.getAccompanyingDeletions());
		
		return subEle;
	}
	
	
	/** Creates a new AddElement.
	 * Based on an existing one 
	 * @param newAddEle Part of the information needed to create the AddElement
	 * @param target Part of the information needed to create the AddElement
	 * @param parent Part of the information needed to create the AddElement
	 * @return the new created AddElement
	 */
	private AddElement getAddElement(final ExportAddElement newAddEle, final EObject target, final AdditiveElement parent) {
		AddElement delta = null;
		try {
			delta = new AddElement(target, UMLHelper.getMetaClass(newAddEle.getType()), parent);
			ArrayList<AddElement> content = new ArrayList<>();
			for (ExportAddElement addEle : newAddEle.getContent()) {
				content.add(getAddElement(addEle, target, delta));
			}
			delta.replaceContent(content);
	        delta.replaceValues(ExporterUtility.getValuesWithNull(newAddEle.getValues()));
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
		}		
		return delta;
	}
	
	
	/** Finds a specific Element by Name and Type.  
	 * 
	 * @param target 
	 * @return a EObject specified in target.
	 */
	private EObject getTargetByNameAndType(final ExportExtTagNamedElement target) {
		List<NamedElement> allElements = new ArrayList<>();

		TreeIterator<EObject> allContent = this.modelres.getAllContents();

		while (allContent.hasNext()) {
			EObject content = allContent.next();
			if ((content instanceof NamedElement) 
					&& ((NamedElement) content).getClass().getSimpleName().equals(target.getType()) 
					&& ((NamedElement) content).getLabel().equals(target.getName())) {
				allElements.add((NamedElement) content);
			}
		}
		if (allElements.size() == 0) {
			return null;
		} else if (allElements.size() == 1) {
			return allElements.get(0);
		} else {
			Logger.log(LogLevel.ERROR, "Couldn't find Target + " + target.getName() + " of type " + target.getType());
			return null;
		}
	}
	
	
	/** Returns the Target.
	 * 
	 * @param ext 
	 * @return EObject target
	 */
	private EObject getTaggedValueTarget(final ExportExtTagTaggedValue ext) {
		TaggedValue value = null;
		StereotypeApplication stereoApp = null;
		try {
			NamedElement extendedElement = UMLHelper.getElementByName(this.model, ext.getExtendedElement());
			for (Stereotype stereo : extendedElement.getAppliedStereotypes()) {
				if (stereo.getQualifiedName().contains(ext.getStereotype()) && stereo.getQualifiedName().contains(ext.getProfile())) {
					stereoApp = new StereotypeApplication(stereo, extendedElement);
					for (Property p : stereo.getAttributes()) {
						if (p.getName().toLowerCase(Locale.ENGLISH).equals(ext.getName().toLowerCase(Locale.ENGLISH))) {
							value = new TaggedValue(p, stereoApp);
						}
					}
				}
			}
		
			if (value == null) {
				Logger.log(LogLevel.ERROR, "Couldn't find the tagged value " + ext.getProfile() + ":" + ext.getStereotype() + ":" + ext.getName());
			}
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "Couldn't find tagged value");
			Logger.log(LogLevel.ERROR, e.getMessage());			
		}
		return value;
	}
	
	
	/** Returns the Target.
	 * 
	 * @param ext 
	 * @return EObject target
	 */
	private EObject getStereoTarget(final ExportExtTagStereotype ext) {
		StereotypeApplication stereoApp = null;
		try {
			NamedElement extendedElement = UMLHelper.getElementByName(this.model, ext.getExtendedElement());
			if (extendedElement == null) { 
					return null;
			}
			for (Stereotype stereo : extendedElement.getAppliedStereotypes()) {
				if (stereo.getQualifiedName().contains(ext.getName()) && stereo.getQualifiedName().contains(ext.getProfile())) {
					stereoApp = new StereotypeApplication(stereo, extendedElement);
				}
			}
			if (stereoApp == null) {
				Logger.log(LogLevel.ERROR, "Couldn't find stereotype " + ext.getExtendedElement() + ":" + ext.getName());
			}
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "Couldn't find extended element.");
			Logger.log(LogLevel.ERROR, e.getMessage());			
		}
		return stereoApp;
	}
}
