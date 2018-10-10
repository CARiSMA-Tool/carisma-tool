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
package carisma.evolution.uml2.umlchange;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Stereotype;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlchange.UMLchangeUtil;



/**
 * The UMLchangeValidator checks if the UMLchange applications in a given model use
 * the correct UMLchange syntax. Aside from that, it checks if
 * - all referenced change ID's are defined and no duplicate ID's exist
 * - elements referenced in ext values exist at the extended element
 * - SimpleElementDescription metaclasses are UML metaclasses (or TaggedValue)
 * - referenced complex namespaces are modeled
 * - <<old>> marked elements exist in the original model
 * @author Klaus Rudack, Daniel Warzecha
 *
 */
public class UMLchangeValidator implements CarismaCheckWithID {
	
	//TODO: Currently not registered at carisma.carismackeck extension point
	private static final String CHECK_ID = null;
	private static final String CHECK_NAME = null;

	private List<String> complexNamespaces = null;

    /**
     * Constant name for TaggedValue 'new'.
     */
    private static final String NEW = "new";
    
    /**
     * Constant name for TaggedValue 'ref'.
     */
    private static final String REF = "ref";
    
    /**
     * Constant String for output.
     */
    private static final String AT = "' at ";

	/**
	 * AnalysisHost for report.
	 */
	private AnalysisHost analysisHost = null;
	
	/**
	 * Constructor initializes the 
	 */
	public UMLchangeValidator() {
		init(null);
	}
	/**
	 * Sets the host and initializes the Validator.
	 * @param newHost - the new AnalysisHost
	 */
	public final void init(final AnalysisHost newHost) {
		if (newHost != null) {
			this.analysisHost = newHost;
		} else if (this.analysisHost == null) {
			this.analysisHost = new DummyHost(true);
		}
		if (this.complexNamespaces == null) {
			this.complexNamespaces = new ArrayList<>();
		} else {
			this.complexNamespaces.clear();
		}
	}
	
	/**
	 * Performs the CARiSMA check to validate the given analysis model.
	 * @param parameters - The check parameters
	 * @param host - the analysis host
	 */
	@Override
	public boolean perform(final Map<String,CheckParameter> parameters, final AnalysisHost host) {
		init(host);
		Model modelToAnalyze = null;
		if (this.analysisHost.getAnalyzedModel() != null 
				&& !this.analysisHost.getAnalyzedModel().getContents().isEmpty() 
				&& this.analysisHost.getAnalyzedModel().getContents().get(0) instanceof Model) {
			modelToAnalyze = (Model) this.analysisHost.getAnalyzedModel().getContents().get(0);
		}
		return isValidModel(modelToAnalyze);
	}
	
	/** 
	 * Validates the model by checking all of the UMLchange applications for
	 * - duplicate change IDs
	 * - basic and special checks for applications
	 * @param theModel - the UML model to validate
	 * @return - true if the model is valid, false otherwise
	 */
	public final boolean isValidModel(final Model theModel) {
		if (!UMLHelper.isProfileApplied(theModel, UMLchange.DESCRIPTOR)) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "No UMLchange profile applied to the model."));
			return true;
		}
		List<StereotypeApplication> allChangeApps = UMLchangeUtil.getStereotypeApplications(theModel);
		boolean hasDuplicateIds = hasDuplicateChangeIds(allChangeApps);
		boolean hasInvalidApplications = false;
		List<String> validChangeIds = collectChangeIds(theModel);
		for (StereotypeApplication umlChangeApp : allChangeApps) {
			if (!isValidApplication(umlChangeApp, validChangeIds)) {
				hasInvalidApplications = true;
			}
		}
		if (hasInvalidApplications || hasDuplicateIds) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Invalid application of UMLchange profile."));
			return false;
		}
		return true;
	}
	
	/**
	 * Validates a single UMLchange StereotypeApplication according to its type.
	 * Checks for/if
	 * - the application defines an invalid change ID
	 * - invalid common tag syntax and semantics
	 * - stereotype-specific syntax
	 * - stereotype-specific semantics
	 * @param umlChangeApp - the StereotypeApplication
	 * @param validChangeIds - list with all valid defined changeIDs in the model
	 * @return - true if the StereotypeApplication is a valid UMLchange Stereotype Application, false otherwise
	 */
	public final boolean isValidApplication(final StereotypeApplication umlChangeApp, final List<String> validChangeIds) {
		if (!UMLchange.contains(umlChangeApp.getAppliedStereotype())) {
			return false;
		}
		boolean definesInvalidChangeIds = definesInvalidChangeIds(umlChangeApp);
		if (!definesInvalidChangeIds) {
			boolean hasInvalidExtEntries = hasInvalidExtEntries(umlChangeApp);
			boolean hasInvalidConstraintEntries = hasInvalidConstraintEntries(umlChangeApp, validChangeIds);
			boolean passesBasicAppValidation = basicAppValidation(umlChangeApp);
			if (hasInvalidExtEntries || hasInvalidConstraintEntries || !passesBasicAppValidation) {
				return false;
			}
			return specialAppValidation(umlChangeApp);
		}
		return true;
	}
	
	public boolean basicAppValidation(final StereotypeApplication umlChangeApp) {
		Stereotype changeStereo = umlChangeApp.getAppliedStereotype();
		if ((UMLchange.ADD.isEqual(changeStereo)) || (UMLchange.SUBST.isEqual(changeStereo))) {
			return basicTagValidation(umlChangeApp, NEW, UMLchangeSyntax.REGEX_NEW_VALUE);
		} else if (UMLchange.DEL.isEqual(changeStereo)) {
			return true;
		} else if (UMLchange.EDIT.isEqual(changeStereo)) {
			return basicTagValidation(umlChangeApp, "values", UMLchangeSyntax.REGEX_VALUES_VALUE);
		} else if (UMLchange.MOVE.isEqual(changeStereo)) {
			return basicTagValidation(umlChangeApp, "to", UMLchangeSyntax.REGEX_MOVE_TO_VALUE);
		} else if (UMLchange.COPY.isEqual(changeStereo)) {
			return basicTagValidation(umlChangeApp, "to", UMLchangeSyntax.REGEX_COPY_TO_VALUE);
		} else if ((UMLchange.ADDALL.isEqual(changeStereo)) || (UMLchange.SUBSTALL.isEqual(changeStereo))) {
			boolean validNewValues = basicTagValidation(umlChangeApp, NEW, UMLchangeSyntax.REGEX_NEW_VALUE);
			boolean validPatternValues = basicTagValidation(umlChangeApp, "pattern", UMLchangeSyntax.REGEX_PATTERN_VALUE);
			return (validNewValues && validPatternValues); 
		} else if (UMLchange.DELALL.isEqual(changeStereo)) {
			return basicTagValidation(umlChangeApp, "pattern", UMLchangeSyntax.REGEX_PATTERN_VALUE);
		} else if (UMLchange.KEEP.isEqual(changeStereo)) {
			return matchesSyntax(umlChangeApp.getTaggedValue("adopter").getStringValues(), UMLchangeSyntax.REGEX_ADOPTER_VALUE);
		} else if (UMLchange.OLD.isEqual(changeStereo)) {
			return true;
		} else {
			this.analysisHost.appendLineToReport("Not known UMLchange Stereotype " + umlChangeApp);
			return false;
		}
	}
	
	public boolean specialAppValidation(final StereotypeApplication umlChangeApp) {
		Stereotype changeStereo = umlChangeApp.getAppliedStereotype();
		if (umlChangeApp.getTaggedValue(NEW) != null) {
		// FIXME: Metaklasse UML-Metaklasse oder TaggedValue (UMLHelper benutzen)? 
		// Bei allen SEDs (also auch in contents)?
			boolean returnValue = true;
			for (String tagValue : umlChangeApp.getTaggedValue(NEW).getStringValues()) {
				if (invalidNamespace(umlChangeApp, tagValue)) {
					returnValue = false;
				}
			}
			return returnValue;
		} else if (UMLchange.KEEP.isEqual(changeStereo)) {
			if (umlChangeApp.getTaggedValue("adopter").getStringValues().isEmpty()) {
				this.analysisHost.appendLineToReport("The adopter-tag in stereotype <<keep>> at element \"" + umlChangeApp.getExtendedElementName()
						+ "\" should have content");
				return false;
			}
		} else if (UMLchange.OLD.isEqual(changeStereo)) {
			return isValidOldApplication(umlChangeApp);
		}
		return true;
	}
	/**
	 * 
	 * @param changeIds
	 * @return
	 */
	public static boolean hasDuplicateChangeIds(final List<StereotypeApplication> changeApps) {
		List<String> changeIds = new ArrayList<>();
		for (StereotypeApplication umlChangeApp : changeApps) {
			if (umlChangeApp.hasTagValue(REF)) {
				for (String ref : umlChangeApp.getTaggedValue(REF).getStringValues()) {
					if (changeIds.contains(ref)) {
						return true;
					}
					changeIds.add(ref);
				}
			}
		}
		return false;
	}
	
	public static List<String> collectChangeIds(final Model forModel) {
		List<String> changeIds = new ArrayList<>();
		for (StereotypeApplication umlChangeApp : UMLchangeUtil.getStereotypeApplications(forModel)) {
			if (umlChangeApp.hasTagValue(REF)) {
				for (String ref : umlChangeApp.getTaggedValue(REF).getStringValues()) {
					changeIds.add(ref);
				}
			}
		}
		return changeIds;
	}
	
	public boolean definesInvalidChangeIds(final StereotypeApplication umlChangeApp) {
		boolean definesInvalidChangeIds = false;
		if (!umlChangeApp.hasTagValue(REF)) {
			return false;
		}
		for (String changeId : umlChangeApp.getTaggedValue(REF).getStringValues()) {
			if (!matchesSyntax(changeId, "^" + UMLchangeSyntax.REGEX_REFID + "$")) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(
						StatusType.ERROR, "Invalid change ID '" + changeId + AT + umlChangeApp.toString()));
				definesInvalidChangeIds = true;
			}
		}
		return definesInvalidChangeIds;
	}
	
	public boolean hasInvalidTarget(final String extValue, final Element extendedElement) {
		Pattern p = Pattern.compile("(?<=\\=)" + UMLchangeSyntax.REGEX_STEREOTYPE);
		Matcher m = p.matcher(extValue);
		if (m.find()) {
			String stereoName = m.group();
			if (UMLchange.getValue(stereoName) != null) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "{ext}: Targets an UMLchange app '" + extValue + "'"));
				return true;
			}
			boolean foundApp = false;
			for (StereotypeApplication app : UMLHelper.getStereotypeApplications(extendedElement)) {
				if (app.getName().equalsIgnoreCase(stereoName)) {
					foundApp = true; 
				}
				if (!foundApp) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "{ext}: Targets non-applied stereotype '" + extValue + "'"));
					return true;
				}
				m.usePattern(Pattern.compile("(?<=\\.)" + UMLchangeSyntax.REGEX_TAG));
				if (m.find()) {
					String tagName = m.group();
					if (app.getTaggedValue(tagName) == null) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "{ext}: Wrong tag '" + extValue + "'"));
						return true;
					}
				}
			}
		}
		return false;
	}
	
	public boolean hasInvalidExtEntries(final StereotypeApplication umlChangeApp) {
		boolean hasInvalidExtEntries = false;
		if (!umlChangeApp.hasTagValue("ext")) {
			return false;
		}
		hasInvalidExtEntries = (!basicTagValidation(umlChangeApp, "ext", UMLchangeSyntax.REGEX_EXT_VALUE));
		for (String extValue : umlChangeApp.getTaggedValue("ext").getStringValues()) {
			if (hasInvalidTarget(extValue, umlChangeApp.getExtendedElement())) {
				hasInvalidExtEntries = true;
			}
		}	
		return hasInvalidExtEntries;
	}
	
	public static boolean hasInvalidChangeReference(final String referencingValue, final List<String> validReferences) {
		if (referencingValue.isEmpty()) {
			return false;
		}
		Pattern p = Pattern.compile("^" + UMLchangeSyntax.REGEX_REFID);
		Matcher m = p.matcher(referencingValue);
		if (m.find()) {
			String referencedId = m.group();
			if (!validReferences.contains(referencedId)) {
				return true;
			}
		}
		return false;
	}

	public boolean hasInvalidReferences(final String constraintValue, final List<String> validReferences) {
		Pattern p = Pattern.compile("^" + UMLchangeSyntax.REGEX_REFID);
		Matcher m = p.matcher(constraintValue);
		if (m.find()) {
			String selfId = m.group();
			m.usePattern(Pattern.compile(UMLchangeSyntax.REGEX_CONSTRAINT_REFERENCE));
			while (m.find()) {
				String referencedChangeId = m.group();
				if (selfId.equalsIgnoreCase(referencedChangeId)
						|| !validReferences.contains(referencedChangeId)) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "{constraints}: Wrong refID '" + referencedChangeId + "'"));
					return true;
				}
			}
		}
		return false;
	}
	
	public boolean hasInvalidConstraintEntries(final StereotypeApplication umlChangeApp, final List<String> validChangeIds) {
		boolean hasInvalidConstraintEntries = false;
		if (!umlChangeApp.hasTagValue("constraint")) {
			return false;
		}
		hasInvalidConstraintEntries = (!basicTagValidation(umlChangeApp, "constraint", UMLchangeSyntax.REGEX_CONSTRAINT_VALUE));
		for (String constraintValue : umlChangeApp.getTaggedValue("constraint").getStringValues()) {
			if (hasInvalidReferences(constraintValue, validChangeIds)) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "{constraint}: invalid constraint '" + constraintValue + AT
			+ umlChangeApp + "."));
				hasInvalidConstraintEntries = true;
			}
		}
		return hasInvalidConstraintEntries;
	}
	
	public static boolean matchesSyntax(final List<String> values, final String syntax) {
		for (String value : values) {
			if (!matchesSyntax(value, syntax)) {
				return false;
			}
		}
		return true;
	}
	
	public static boolean matchesSyntax(final String value, final String syntax) {
		Pattern p = Pattern.compile(syntax);
		Matcher m = p.matcher(value);
		return m.find();
	}
	
	public boolean basicTagValidation(final StereotypeApplication umlChangeApp, final String tagName, final String syntax) {
		boolean validTagValues = true;
		List<String> tagValues = umlChangeApp.getTaggedValue(tagName).getStringValues();
		
		for (String tagValue : tagValues) {
			if (!matchesSyntax(tagValue, syntax)) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "{" + tagName + "}: Wrong syntax '" + tagValue + AT + umlChangeApp
						+ "."));
				validTagValues = false;
				continue;
			}
			if (hasInvalidChangeReference(tagValue, umlChangeApp.getTaggedValue(REF).getStringValues())) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "{" + tagName + "}: Invalid change reference '" + tagValue + AT
			+ umlChangeApp + "."));
				validTagValues = false;
			}
		}
		return validTagValues;
	}
	
	/**
	 * validates an UMLchange old StereotypeApplication.
	 * @param umlChangeApp - the UMLchange move StereotypeApplication
	 * @return - true if the StereotypeApplication is a valid UMLchange old StereotypeApplication, false otherwise
	 */
	public final static boolean isValidOldApplication(final StereotypeApplication umlChangeApp) {
		if (!UMLchange.OLD.isEqual(umlChangeApp.getAppliedStereotype())) {
			return false;
		}
		String elementName = umlChangeApp.getExtendedElementName();
		List<NamedElement> referencedElements = UMLHelper.getAllSameNameElements(umlChangeApp.getModel(), elementName);
		if (referencedElements.size() == 2) {
			return true;
		}
		return false;
	}
	
	
	/**
	 * checks if the content of the newTag  is a valid Namespace, if it is a NamespaceDescription.
	 * @param umlChangeApp - StereotypeApplication 
	 * @param tagValue - value of the new-tag of the StereotypeApplication
	 * @return - false if tagValue is a correct NamespaceDescription or no NamespaceDescription, false otherwise
	 */
	public final boolean invalidNamespace(final StereotypeApplication umlChangeApp, final String tagValue) {
		boolean returnValue = false;
		Pattern p = Pattern.compile("^" + UMLchangeSyntax.REGEX_REFID_PREFIX);
		Matcher m = p.matcher(tagValue);
		m.usePattern(Pattern.compile(UMLchangeSyntax.REGEX_NAMESPACE_REFERENCE));
		while (m.find()) {
			String namespace = m.group();
			Model model = UMLHelper.getModel(umlChangeApp.getExtendedElement());
			List<NamedElement> referencedElements = UMLHelper.getAllSameNameElements(model, namespace);
			if (referencedElements.size() > 1) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Error, duplicatet Namespace \"" + namespace + "\"."));
				returnValue = true;
			} else if (referencedElements.size() == 0) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No Element found for the complex Namespace \"" + namespace
						+ "\" at Stereotype <<"	+ umlChangeApp.getStereotypeName() + ">> at Element \""
						+ ((NamedElement) umlChangeApp.getExtendedElement()).getQualifiedName() + "\"."));
				returnValue = true;
			}
		}
		return returnValue;
	}
	@Override
	public String getCheckID() {
		return CHECK_ID;
	}
	@Override
	public String getName() {
		return CHECK_NAME;
	}
}