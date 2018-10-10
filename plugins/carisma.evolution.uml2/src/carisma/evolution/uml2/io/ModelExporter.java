package carisma.evolution.uml2.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.CopyElement;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.io.datatype.ExportAddElement;
import carisma.evolution.uml2.io.datatype.ExportChange;
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
import carisma.evolution.uml2.io.datatype.XStreamAlias;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;

import com.thoughtworks.xstream.XStream;

 
/** MainClass for Exporting Deltas into an XML File.
 * Used by the ModelExporterCheck.
 * @author bberghoff
 *
 */
public class ModelExporter {
	
	
	/** Exports a given Delta into a XML File. 
	 * 
	 * @param outputFile Target where the XML File will be saved.
	 * @param deltas The Deltas to be Exported
	 * @param resource The Resource which defines the xmi-ID etc.
	 * @return true if no Exception is thrown during writing the xml file.
	 */
	public final static boolean writeDeltasToFile(final File outputFile, final List<Delta> deltas, final Resource resource) {
		boolean result = true;
		XStream stream = XStreamAlias.getXStream();
		
		List<ExportDelta> toBeExported = generateXMLOutput(deltas, resource);

		try (FileOutputStream outStream = new FileOutputStream(outputFile)){
			stream.toXML(toBeExported, new OutputStreamWriter(outStream, "UTF-8"));
		} catch (UnsupportedEncodingException e) {
			Logger.log(LogLevel.ERROR, "Wrong Encoding", e);
			result = false;
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			result = false;
		}
		return result;
	}
	
	/** Exports a given Delta into a XML File. 
	 * 
	 * @param outputFile Target where the XML File will be saved.
	 * @param delta The Delta to be Exported
	 * @param resource The Resource which defines the xmi-ID etc.
	 * @return returns true if everything worked out.
	 */
	public final static boolean writeDeltaToFile(final File outputFile, final Delta delta, final Resource resource) {
		boolean result = true;
		XStream stream = XStreamAlias.getXStream();
		
		ExportDelta toBeExported = generateXMLOutput(delta, resource);
		
		try (FileOutputStream outStream = new FileOutputStream(outputFile)){
				stream.toXML(toBeExported, new OutputStreamWriter(outStream, "UTF-8"));
		} catch (UnsupportedEncodingException e) {
			Logger.log(LogLevel.ERROR, "Wrong Encoding", e);
			result = false;
		} catch (FileNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			return false;
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			return false;
		} 
		return result;
	}
	
	/** Exports a given Delta into a XML File. 
	 * 
	 * @param deltasToExport The List of Deltas to Exported
	 * @param resource The Resource which defines the xmi-ID etc.
	 * @return returns true if everything worked out.
	 */
	private static List<ExportDelta> generateXMLOutput(final List<Delta> deltasToExport, final Resource resource) { 
		List<ExportDelta> deltas = new ArrayList<>();
		for (Delta delta : deltasToExport) {
			deltas.add(generateXMLOutput(delta, resource));
		}
		return deltas;
	}
	
	
	/** Transforms a single Delta Object into a new DataStructure.
	 *  This new Element can be exported into a XML File.
	 *   
	 * @param deltaToExport The Delta to be exported
	 * @param resource the Resource which contains the Delta
	 * @return the new output DataStructure.
	 */
	public final static ExportDelta generateXMLOutput(final Delta deltaToExport, final Resource resource) {
		
		if (deltaToExport == null || resource == null) { 
			return null;
		}
		
		ExportDelta expDelta = new ExportDelta(deltaToExport.getChangeIDs(), null);
		for (DeltaElement deltaEle : deltaToExport.getContent()) {
			ExportDeltaElement newEle = null;
			
			if (deltaEle instanceof AddElement) {
				AddElement delta = (AddElement) deltaEle;
				newEle = new ExportAddElement(delta);
			} else if (deltaEle instanceof DelElement) {
				newEle = new ExportDelElement((DelElement) deltaEle);
			} else if (deltaEle instanceof SubstElement) {
				SubstElement delta = (SubstElement) deltaEle;
				newEle = new ExportSubstElement(delta);
			} else if (deltaEle instanceof EditElement) {
				EditElement editEle = (EditElement) deltaEle;
				newEle = new ExportEditElement(editEle);
			} else if (deltaEle instanceof CopyElement) {
				CopyElement cpEle = (CopyElement) deltaEle;
				newEle = new ExportCopyElement(cpEle);
				((ExportCopyElement) newEle).setTargetOwner(
						getExportTarget(
						cpEle.getReceivingElement(),
						resource));
			} else {
				Logger.log(LogLevel.WARNING, "The element \"" + deltaEle.toString() + "\" has an unexpeced format and will be ignored.");
				continue; // starts at the for-loop and no ElementDescription for deltaEle is added.
			}
			newEle.setExt(getExportTarget(
					deltaEle.getTarget(), resource));

			expDelta.addExpElementDescription(newEle);
		}
		return expDelta;
	}

	/** Method to transform a target in the new data-structure.
	 * @param target The target to be transformed.
	 * @param resource the resource which contains the target.
	 * @return The Target in the new Datastructure
	 */
	private static ExportExtTag getExportTarget(final EObject target, final Resource resource) {
		ExportExtTag expTarget = null;
		if (target instanceof TaggedValue) {
			TaggedValue value = (TaggedValue)
					target;

			try {
				XMLResource xmlRes = ((XMLResource)
						resource);
				StereotypeApplication tmpApp =
						value.getCorrespondingApplication();
				Element tmpEle =
						tmpApp.getExtendedElement();
				expTarget = new ExportExtTagTaggedValue(
						"Tagged Value",
						value.getName(),
						xmlRes.getID(tmpEle),
						((NamedElement) tmpEle).getQualifiedName(),
						tmpApp.getAppliedStereotype().getProfile().getQualifiedName(),
						tmpApp.getAppliedStereotype().getName());
				
			} catch (Exception e) {
				Logger.log(LogLevel.ERROR, "Fail in ExtTaggedValue", e);
			}
		} else if (target instanceof StereotypeApplication) {
				StereotypeApplication stype = (StereotypeApplication) target;

				try {
					expTarget = new ExportExtTagStereotype(
						"StereotypeApplication", 
						stype.getAppliedStereotype().getName(), 
						((XMLResource) resource).
						getID(stype.getExtendedElement()), 
						((NamedElement) stype.getExtendedElement()).getQualifiedName(),
						stype.getAppliedStereotype().getProfile().getQualifiedName()
					);

				} catch (Exception eII) {
					Logger.log(LogLevel.ERROR, "Fail in ExtStereotype. " + eII);
				}
		} else if (target instanceof NamedElement) {

			NamedElement nameEle = (NamedElement) target;
			try {
				expTarget = new ExportExtTagNamedElement(
						nameEle.getClass()
						.getSimpleName(),
						nameEle.getLabel(),
						((XMLResource) resource)
						.getID(nameEle));

			} catch (Exception eIII) {
				Logger.log(LogLevel.ERROR, "Fail in ExtClass. " + eIII);
			}
		}

		return expTarget;
	}


	/** Actually writes down the Deltas which are to be exported.
	 * 
	 * @param outputFile File to Save the Data.
	 * @param exportChanges List of Changes in the right format to be exported
	 * @param overwrite True if one wants to override an existing File.
	 * @return true if and only if no Exception is thrown during the attempt of writing down the File.
	 */
	public final static boolean writeToFile(final File outputFile, final List<ExportChange> exportChanges, final boolean overwrite) {

		XStream stream = XStreamAlias.getXStream();
		
		try (FileOutputStream outStream = new FileOutputStream(outputFile)){
			stream.toXML(exportChanges, new OutputStreamWriter(outStream, "UTF-8"));
		} catch (UnsupportedEncodingException e) {
			Logger.log(LogLevel.ERROR, "Wrong Encoding", e);
			return false;
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, "", e);
			return false;
		}
		return true;
	}
}