package carisma.profile.umlsec.enc;
//	UMLsec Stereotypen:
//	locked status		UMLsec::locked-status
//	authorized status	UMLsec::authorized-status
//	fair exchange		UMLsec::fair exchange
//	guarded				UMLsec::guarded
//	secure links		UMLsec::secure links
//	critical			UMLsec::critical
//	provable			UMLsec::provable
//	noDownFlow			UMLsec::no down-flow
//	noUpFlow			UMLsec::no up-flow
//	guarded access		UMLsec::guarded access
//	Secure Dependency	UMLsec::secure dependency
//	Data Security		UMLsec::data security
//	RBAC				UMLsec::rbac
//	requires			UMLsec::requires

//	ask for the following Stereotypes, they have no need of checking but are valid UMLsec stereotypes
//	call				UMLsec::call
//	send				UMLsec::send
//	secrecy				UMLsec::secrecy
//	high				UMLsec::high
//	identifiable		UMLsec::identifiable
//	integrity			UMLsec::integrity
//	LAN					UMLsec::LAN
//	Internet			UMLsec::Internet
//	encrypted			UMLsec::encrypted
//	smart card			UMLsec::smart card
//	POSdevice			UMLsec::POS device
//	issuer node			UMLsec::issuer node
//	wire				UMLsec::wire

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.FinalState;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;

import java.util.logging.Logger;

/**
 * This class validates UMLsec stereotypes.
 * When validating  a model, a List<String> will be returned, where every entry describes a violation
 * If the List is empty, all UMLsec Stereotypes are applied in a correct way
 * @author Klaus Rudack
 *
 */
@SuppressWarnings("deprecation")
public final class UMLsecValidation {
	
	private static final Logger logger = Logger.getLogger(UMLsecValidation.class.getName());
	
//	TODO KR: beim testen ob ein String leer ist vielleicht besser --> testen ob ein String etwas anderes als leere Zeichen enth�lt
//	TODO KR: Identifiable, wo darf das dran, findet sich im UMLsec Buch nicht
//	TODO KR: Es fehlen laut UMLsec.profile.png vom 14.11.2013 folgende Stereotypen
//				identifiable (als nicht zu checkender Stereotyp aufgenommen)
//				allowed users
//				seperation of duty
//				SAP Transaction
//				used-by
//				protected
	
	/**
	 * Making constructor private.
	 */
	private UMLsecValidation() {
		
	}
	
	/**
	 * Checks all UMLsec Stereotypes of a given UML-Model.
	 * @param file The UML Model
	 * @throws IllegalArgumentException if the given file is null or does not exist
	 * @return false if any Stereotype isn't filled correctly or is applied to an element where it does not belong to, true otherwise
	 */
	public static List<String> validateModel(final File file) {
		Model model = null;
		UML2ModelLoader ml = new UML2ModelLoader();
		Resource modelres = null;
		if ((file == null) || !file.exists()) {
			throw new IllegalArgumentException("The given File ist not allowed to be null and has to exist!");
		}
		try {
			modelres = ml.load(file);
		} catch (IOException e) {
			logger.warning("Error message: " + e.getMessage());
			List<String> exceptionValue = new ArrayList<String>();
			exceptionValue.add("IOException occured  while loading the model");
			return exceptionValue;
		}
		if (modelres == null) {
//			TODO KR: Fehlermeldung
		}
		model = (Model) modelres.getContents().get(0);
		if (model == null) {
//			TODO KR: Fehlermeldung, dies m�sste bedeuteten, dass das UML-Model leer ist
		}
		return validateModel(model);
	}
	
	/**
	 * Checks all UMLsec Stereotypes of a given UML-Model.
	 * @param file Path to the UML Model
	 * @throws IllegalArgumentException if the given String does not lead to an existing file
	 * @return false if any Stereotype isn't filled correctly or is applied to an element where it does not belong to, true otherwise
	 */
	public static List<String> validateModel(final String file) {
		File modelFile = new File(file);
		if ((modelFile == null) || !modelFile.exists()) {
			throw new IllegalArgumentException("The given String has to lead to an existing UML model!");
		}
		return validateModel(modelFile);
	}
	
	/**
	 * Checks all UMLsec Stereotypes of a given UML-Model.
	 * @param model a model to check, annotated with UMLsec Stereotypes
	 * @throws IllegalArgumentException if the given model is null
	 * @return  true if every UMLsec Stereotype is filled correctly, false otherwise
	 */
	public static List<String> validateModel(final Model model) {
		List<String> validations = new ArrayList<String>();
		if (model == null) {
			throw new IllegalArgumentException("The model is not allowed to be null!");
		}
		List<StereotypeApplication> allStereotypes = getAllStereoApps(model);
		for (StereotypeApplication stApp : allStereotypes) {
			validations.addAll(validateSingleStereotype(stApp));
		}
		return validations;
	}
	
	/**
	 * This method gets a single StereotypeApplication and determines the associated Stereotype.
	 * Than if calls the check-method for the Stereotype.
	 * @param stApp StereotypeApplication to check.
	 * @return true if the Stereotype associated with the given StereotypeApplication if filled correctly, false otherwise
	 */
	public static List<String> validateSingleStereotype(final StereotypeApplication stApp) {
		String stereoName = stApp.getQualifiedStereotypeName();
		if (stereoName.equals("UMLsec::secure links")) {
			return validateSecureLinks(stApp);
		}
		if (stereoName.equals("UMLsec::fair exchange")) {
			return validateFairExchange(stApp);
		}
		if (stereoName.equals("UMLsec::no down-flow")) {
			return validateNoDownFlow(stApp);
		}
		if (stereoName.equals("UMLsec::provable")) {
			return validateProvable(stApp);
		}
		if (stereoName.equals("UMLsec::requires")) {
			return validateRequires(stApp);
		}
		if (stereoName.equals("UMLsec::locked-status")) {
			return validateLockedStatus(stApp);
		}
		if (stereoName.equals("UMLsec::secure dependency")) {
			return validateSecureDependency(stApp);
		}
		if (stereoName.equals("UMLsec::data security")) {
			return validateDataSecurity(stApp);
		}
		if (stereoName.equals("UMLsec::no up-flow")) {
			return validateNoUpFlow(stApp);
		}
		if (stereoName.equals("UMLsec::authorized-status")) {
			return validateAuthorizedStatus(stApp);
		}
		if (stereoName.equals("UMLsec::rbac")) {
			return validateRBAC(stApp);
		}
		if (stereoName.equals("UMLsec::critical")) {
			return validateCritical(stApp);
		}
		if (stereoName.equals("UMLsec::guarded access")) {
			return validateGuardedAccess(stApp);
		}
		if (stereoName.equals("UMLsec::guarded")) {
			return validateGuarded(stApp);
		}
		if (stereoName.equals("UMLsec::call")) {
			//call muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::secrecy")) {
			//secrecy muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::LAN")) {
			//Lan muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::high")) {
			//high  muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::Internet")) {
			//Internet  muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::send")) {
			//send  muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::integrity")) {
			//integrity  muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::identifiable")) {
			//identifiable  muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::encrypted")) {
			//encrypted  muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::wire")) {
			//wire  muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::POS device")) {
			//POS device  muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::issuer node")) {
			//issuer node  muss nicht validiert werden
			return new ArrayList<String>();
		}
		if (stereoName.equals("UMLsec::smart card")) {
			//smart card  muss nicht validiert werden
			return new ArrayList<String>();
		}
//		TODO KR: Es sind noch nicht alle Stereotypen drin, die nicht �berpr�ft werden m�ssen
//		Stereotyp nicht gefunden, was tuen?
		List<String> noStereotype = new ArrayList<String>();
		noStereotype.add("Stereotype " + stApp.getQualifiedStereotypeName() + " is no valid UMLsec Stereotype!");
		return noStereotype;
	}
	
	/**
	 * This method checks a given StereotypeApplication if it is a correct filled &lt;&lt;fair exchange&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;fair exchange&gt;&gt; {@link StereotypeApplication}
	 * @return true if the stereotype is filled correctly, false otherwise
	 */
	//check if tag 'start' got content
	//check if tag 'stop' got content
	//check if the stereotype is applied to a org.uml2.uml.Package.
	@SuppressWarnings("unchecked")
	public static List<String> validateFairExchange(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		String fairName = "UMLsec::fair exchange";
		Element fairExchangeElement = stereoApp.getExtendedElement();
		Stereotype stereotype = fairExchangeElement.getApplicableStereotype(fairName);
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.FAIR_EXCHANGE)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<fair exchange>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		if ((!(stereoApp.getExtendedElement() instanceof Package)) || (stereoApp.getExtendedElement() instanceof Model)) {
			validations.add("Stereotype <<fair exchange>> is only  allowed to be applied to a Package! Actually it is applied to "
									+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
			return validations;
		}
		List<List<String>> startValues = (List<List<String>>) fairExchangeElement.getValue(stereotype, "start");
		List<List<String>> stopValues = (List<List<String>>) fairExchangeElement.getValue(stereotype, "stop");
		if (startValues.size() < 1) {
			validations.add("Empty Tag start of Stereotype <<fair exchange>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		if (stopValues.size() < 1) {
			validations.add("Empty Tag stop of Stereotype <<fair exchange>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		//no need to check if the values of the start and stop tags represent existing elements
		//in the .uml file, the ids of the elements will be stored in the start and stop tag, if they do not exist
		//an exception will be thrown while loading the model
		return validations;
	}
	
	/**
	 * This method checks a given StereotypeApplication if it is a correct filled &lt;&lt;authorized status&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;authorized status&gt;&gt; {@link StereotypeApplication}
	 * @return true if the stereotype is filled correctly, false otherwise
	 */
	//check if tag permission got content
	//check if stereotype <<authorized status>> is applied to a State, not a FinalState
	public static List<String> validateAuthorizedStatus(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.AUTHORIZED_STATUS)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<authorized status>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		TaggedValue permissionTag = stereoApp.getTaggedValue("permission");
		if ((permissionTag.getValue() == null) || (permissionTag.getValue().equals(""))) {
//			so aus FairExchangeCheck entnommen, soll bedeuten ist nichts im Tag eingetragen
			validations.add("Empty permission tag of Stereotype <<authorized-status>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		if (stereoApp.getExtendedElement() instanceof FinalState) {
			validations.add("Stereotype <<authorized status>> is not allowed to be applied to a Final State! Actually it is applied to "
									+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
		}
		return validations;
	}
	
	
	/**
	 * This method checks a given StereotypeApplication if it is a correct filled &lt;&lt;guarded&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;guarded&gt;&gt; {@link StereotypeApplication}
	 * @return true if the stereotype is filled correctly, false otherwise
	 */
	//check if the tag 'guard' of the stereotype &lt;&lt;guarded&gt;&gt; is not empty.
	@SuppressWarnings("unchecked")
	public static List<String> validateGuarded(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.GUARDED)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <guarded>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		TaggedValue guardTag = stereoApp.getTaggedValue("guard");
		if ((guardTag.getValue() == null) || (((List<List<Element>>) guardTag.getValue()).size() < 1)) {
			validations.add("Empty guard tag of Stereotype <<guarded>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		return validations;
	}
	
	/**
	 * This method validates a given StereotypeApplication if it is a correct &lt;&lt;locked statusgt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;locked status&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	//check if sterotype <<locked status>> is applied to a State, not a FinalState
	public static List<String> validateLockedStatus(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.LOCKED_STATUS)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<lockes status>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		if (stereoApp.getExtendedElement() instanceof FinalState) {
			validations.add("Stereotype <<locked status>> is not allowed to be applied to a FinalState! Actually it is applied to "
									+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
			return validations;
		}
		return validations;
	}

	/**
	 * This method validates a given StereotypeApplication if it is a correct filled &lt;&lt;secure links&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;secure links&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	//check if the tag 'adversary' of the sterotype <<secure links>> is not empty.
	//check if the stereotype is applied to a org.uml2.uml.Package.
	public static List<String> validateSecureLinks(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.SECURE_LINKS)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<secure links>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		if ((!(stereoApp.getExtendedElement() instanceof Package)) || (stereoApp.getExtendedElement() instanceof Model)) {
			validations.add("Stereotype <<secure links>> is only  allowed to be applied to a Package!\nActually it is applied to "
									+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
			return validations;
		}
		TaggedValue adversaryTag = stereoApp.getTaggedValue("adversary");
		if ((adversaryTag.getValue() == null) || adversaryTag.getValue().equals("")) {
			validations.add("Empty adversary tag of Stereotype <<secure links>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		return validations;
	}
	
	/**
	 * This method validates a given StereotypeApplication if it is a correct filled &lt;&lt;critical&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;critical&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	//check if one of the tags of critical got content
	@SuppressWarnings("unchecked")
	public static List<String> validateCritical(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.CRITICAL)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<critical>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		TaggedValue secrecyTag = stereoApp.getTaggedValue("secrecy");
		TaggedValue integrityTag = stereoApp.getTaggedValue("integrity");
		TaggedValue highTag = stereoApp.getTaggedValue("high");
		TaggedValue freshTag = stereoApp.getTaggedValue("fresh");
		TaggedValue authenticityTag = stereoApp.getTaggedValue("authenticity");
		boolean returnvalue = false;
		Model model = stereoApp.getModel();
		if (!(secrecyTag.getValue() == null)) {
			List<String> secrecyValues = (List<String>) secrecyTag.getValue();
			if (!(secrecyValues.size() < 1)) {
				System.out.println(secrecyTag.getValue());
				returnvalue = true;
				for (String element : secrecyValues) {
					if (!findModelElement(model, element)) {
						validations.add("Element " + element + " in Tag secrecy of Stereotype <<critical>> at Element "
								+ stereoApp.getExtendedElementName() + " not found!");
					}
				}
			}
		}
		if (!(integrityTag.getValue() == null)) {
			List<String> integrityValues = (List<String>) integrityTag.getValue();
			if (!(integrityValues.size() < 1)) {
				returnvalue = true;
				for (String element : integrityValues) {
					if (!findModelElement(model, element)) {
						validations.add("Element " + element + " in Tag integrity of Stereotype <<critical>> at Element "
								+ stereoApp.getExtendedElementName() + " not found!");
					}
				}
			}
		}
		if (!(highTag.getValue() == null)) {
			List<String> highValues = (List<String>) highTag.getValue();
			if (!(highValues.size() < 1)) {
				returnvalue = true;
				for (String element : highValues) {
					if (!findModelElement(model, element)) {
						validations.add("Element " + element + " in Tag high of Stereotype <<critical>> at Element "
								+ stereoApp.getExtendedElementName() + " not found!");
					}
				}
			}
		}
		if (!(freshTag.getValue() == null)) {
			List<String> freshValues = (List<String>) freshTag.getValue();
			if (!(freshValues.size() < 1)) {
				returnvalue = true;
				for (String element : freshValues) {
					if (!findModelElement(model, element)) {
						validations.add("Element " + element + " in Tag fresh of Stereotype <<critical>> at Element "
								+ stereoApp.getExtendedElementName() + " not found!");
					}
				}
			}
		}
		if (!(authenticityTag.getValue() == null)) {
			List<String> authenticityValues = (List<String>) authenticityTag.getValue();
			if (!(authenticityValues.size() < 1)) {
				returnvalue = true;
				for (String element : authenticityValues) {
					if (!findModelElement(model, element)) {
						validations.add("Element " + element + " in Tag authenticity of Stereotype <<critical>> at Element "
								+ stereoApp.getExtendedElementName() + " not found!");
					}
				}
			}
		} else {
//			TODO KR: Fehlermeldung, kann eigentlich nicht passieren.
		}
		if (!returnvalue) {
			validations.add("One of the tags 'secrecy, integrity, high, fresh, authenticity' of Sterotype <<critical>> has to hold content!");
		}
		return validations;
	}
	
	/**
	 * This method validates a given StereotypeApplication if it is a correct filled &lt;&lt;requires&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;requires&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	@SuppressWarnings("unchecked")
	//check if the tag 'actions' got content
	public static List<String> validateRequires(final StereotypeApplication stereoApp) {
//		TODO KR: Darf requires an etwas anderem als an einer OpaqueAction sein?
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.REQUIRES)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<requires>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		TaggedValue actionsTag = stereoApp.getTaggedValue("actions");
		if ((actionsTag.getValue() == null) || (((List<List<Element>>) actionsTag.getValue()).size() < 1)) {
			validations.add("Empty actions tag of Stereotype <<requires>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		//no need to check if the values of the actions tag represent existing elements
		//in the .uml file, the ids of the elements will be stored in the start and stop tag, if they do not exist
		//an exception will be thrown while loading the model
		return validations;
	}
	
	/**
	 * This method validates a given StereotypeApplication if it is a correct filled &lt;&lt;provable&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;provable&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	//check if the tag 'action' got content
	//check if the tag 'adversary' got content
	//check if the tag 'cert' got content
	//check if stereotype 'provable' is applied to a package but not to a model
	public static List<String> validateProvable(final StereotypeApplication stereoApp) {
//		TODO KR: an welchen Diagrammtypen darf provable dran?
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.PROVABLE)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<provable>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		if ((!(stereoApp.getExtendedElement() instanceof Package)) || (stereoApp.getExtendedElement() instanceof Model)) {
			validations.add("Stereotype <<provable>> is only  allowed to be applied to a Package! Actually it is applied to "
									+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
			return validations;
		}
		TaggedValue actionTag = stereoApp.getTaggedValue("action");
		if ((actionTag.getValue() == null) || actionTag.getValue().equals("")) {
			validations.add("Empty action tag of Stereotype <<provable>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		if (!findModelElement(stereoApp.getModel(), (String) actionTag.getValue())) {
			System.out.println("Element for the action-value of Stereotype <<provable>> at Element " + stereoApp.getExtendedElementName() + " not found!");
		}
		TaggedValue adversaryTag = stereoApp.getTaggedValue("adversary");
		if ((adversaryTag.getValue() == null) || adversaryTag.getValue().equals("")) {
			validations.add("Empty adversary tag of Stereotype <<provable>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		TaggedValue cerTag = stereoApp.getTaggedValue("cert");
		if ((cerTag.getValue() == null) || cerTag.getValue().equals("")) {
			validations.add("Empty cert tag of Stereotype <<provable>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		return validations;
	}
	
	/**
	 * This method validates a given StereotypeApplication if it is a correct filled &lt;&lt;no down flow&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;no down flow&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	//check if stereotype 'no down flow' is applied to a package but not to a model
	public static List<String> validateNoDownFlow(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.NO_DOWN_FLOW)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<no down flow>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		if ((!(stereoApp.getExtendedElement() instanceof Package)) || (stereoApp.getExtendedElement() instanceof Model)) {
			validations.add("Stereotype <<no down flow>> is only  allowed to be applied to a Package! Actually it is applied to "
									+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
			return validations;
		}
		return validations;
	}
	
	/**
	 * This method validates a given StereotypeApplication if it is a correct filled &lt;&lt;secure dependency&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;secure dependency&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	//check if stereotype 'secure dependency' is applied to a package but not to a model
	public static List<String> validateSecureDependency(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.SECURE_DEPENDENCY)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<secure dependency>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		if ((!(stereoApp.getExtendedElement() instanceof Package)) || (stereoApp.getExtendedElement() instanceof Model)) {
			validations.add("Stereotype <<secure dependency>> is only  allowed to be applied to a Package! Actually it is applied to "
									+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
			return validations;
		}
		return validations;
	}
	
	/**
	 * This method validates a given StereotypeApplication if it is a correct filled &lt;&lt;guarded access&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;guarded access&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	//check if stereotype 'secure dependency' is applied to a package but not to a model
	public static List<String> validateGuardedAccess(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.GUARDED_ACCESS)) {
			// wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<guarded access>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		if ((!(stereoApp.getExtendedElement() instanceof Package)) || (stereoApp.getExtendedElement() instanceof Model)) {
			validations.add("Stereotype <<guarded access>> is only  allowed to be applied to a Package! Actually it is applied to "
									+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
			return validations;
		}
		return validations;
	}
	
	/**
	 * This method validates a given StereotypeApplication if it is a correct filled &lt;&lt;no up flow&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;no up flow&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	//check if stereotype 'no up flow' is applied to a package but not to a model
	public static List<String> validateNoUpFlow(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.NO_UP_FLOW)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<no up flow>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		if ((!(stereoApp.getExtendedElement() instanceof Package)) || (stereoApp.getExtendedElement() instanceof Model)) {
			validations.add("Stereotype <<no up flow>> is only  allowed to be applied to a Package! Actually it is applied to "
									+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
			return validations;
		}
		return validations;
	}
	
	/**
	 * This method validates a given StereotypeApplication if it is a correct filled &lt;&lt;data security&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;data security&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	//check if stereotype 'no down flow' is applied to a Sequence Diagram but not to a model
	//check if tag 'adversary' got content
	//check if tag 'authenticity' got content
	//check if tag 'integrity' got content
	public static List<String> validateDataSecurity(final StereotypeApplication stereoApp) {
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.DATA_SECURITY)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<data security>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		if ((!(stereoApp.getExtendedElement() instanceof Package)) || (stereoApp.getExtendedElement() instanceof Model)) {
			validations.add("Stereotype <<data security>> is only  allowed to be applied to a Package!\nActually it is applied to "
									+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
			return validations;
		}
		TaggedValue adversaryTag = stereoApp.getTaggedValue("adversary");
		if ((adversaryTag.getValue() == null) || adversaryTag.getValue().equals("")) {
			validations.add("Empty adversary tag of Stereotype <<data security>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		
		TaggedValue authenticityTag = stereoApp.getTaggedValue("authenticity");
		if ((authenticityTag.getValue() == null) || authenticityTag.getValue().equals("")) {
			validations.add("Empty authenthicity tag of Stereotype <<data security>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		TaggedValue integrityTag = stereoApp.getTaggedValue("integrity");
		if ((integrityTag.getValue() == null) || integrityTag.getValue().equals("")) {
			validations.add("Empty integrity tag of Stereotype <<data security>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		return validations;
	}
	
	/**
	 * This method validates a given StereotypeApplication if it is a correct filled &lt;&lt;data security&gt;&gt; {@link StereotypeApplication}.
	 * @param stereoApp &lt;&lt;data security&gt;&gt; {@link StereotypeApplication}
	 * @return if the stereotype is filled correctly, false otherwise
	 */
	//check if stereotype 'rbac' is applied to a Sequence Diagram but not to a model
	//check if tag 'protected' got content
	//check if tag 'role' got content and is filled correct as tuples in braces
	//check if tag 'right' got content and is filled correct as tuples in braces
	@SuppressWarnings("unchecked")
	public static List<String> validateRBAC(final StereotypeApplication stereoApp) {
//		TODO KR: die Rollen extrahieren und gucken ob alle Rollen die etwas betreten d�rfen auch eine zugewiesene Person haben
//		TODO KR: m�ssen bei RBAC die TaggeddValues Inhalt haben?
		List<String> validations = new ArrayList<String>();
		if (!(UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) == UMLsec.RBAC)) {
//			wrong StereotypeApplication for this check
			validations.add("Wrong stereotype. <<rbac>> expected, but was <<"
								+ UMLsec.getValue(stereoApp.getAppliedStereotype().getName()) + ">>!");
			return validations;
		}
		if ((!(stereoApp.getExtendedElement() instanceof Package)) || (stereoApp.getExtendedElement() instanceof Model)) {
			validations.add("Stereotype <<rbac>> is only  allowed to be applied to a Package! Actually it is applied to "
					+ ((NamedElement) stereoApp.getExtendedElement()).getName() + "!");
			return validations;
		}
		TaggedValue protectedTag = stereoApp.getTaggedValue("protected");
		if ((protectedTag.getValue() == null) || (((List<String>) protectedTag.getValue()).isEmpty())) {
			validations.add("Empty protected tag of Stereotype <<rbc>> at Element " + stereoApp.getExtendedElementName() + "!");
		}
		
		TaggedValue roleTag = stereoApp.getTaggedValue("role");
		if ((roleTag.getValue() == null) || (((List<String>) roleTag.getValue()).isEmpty())) {
			validations.add("Empty role tag of Stereotype <<rbc>> at Element " + stereoApp.getExtendedElementName() + "!");
		} else  {
//			TODO KR: testen ob das zweite Tuppelelemente ein existierendes Element im Model ist.
			List<String> contentList = new ArrayList<String>();
			contentList.addAll((Collection<? extends String>) roleTag.getValue());
			for (String content : contentList) {
				if (!checkTuple(content)) {
					validations.add("Entrys in tag 'role' must be tuples in braces, e.g. '"
							+ "(E1, E2)', but was '" + content + "' of of Stereotype <<rbc>> at Element " + stereoApp.getExtendedElementName() + "!");
				}
			}
		}
		TaggedValue rightTag = stereoApp.getTaggedValue("right");
		if ((rightTag.getValue() == null) || (((List<String>) rightTag.getValue()).isEmpty())) {
			validations.add("Empty right tag of Stereotype <<rbc>> at Element " + stereoApp.getExtendedElementName() + "!");
		} else {
			List<String> contentList = new ArrayList<String>();
			contentList.addAll((Collection<? extends String>) rightTag.getValue());
			for (String content : contentList) {
				if (!checkTuple(content)) {
					validations.add("Entrys in tag 'right' must be tuples in braces, e.g. '"
							+ "(E1, E2)', but was '" + content + "' of of Stereotype <<rbc>> at Element " + stereoApp.getExtendedElementName() + "!");
				}
			}
		}
		return validations;
	}
	
	/**
	 * Checks if a given String is a tupel in braces.<br>
	 * @example (E1,E2)
	 * @param tuple the String to check
	 * @return true if the given String is a tuple in braces, false otherwise
	 */
	static boolean checkTuple(final String tuple) {
		String tupleRegex = "(?<=^)\\([a-zA-Z0-9]+, [a-zA-Z0-9]+\\)(?=$)";
		Pattern p = Pattern.compile(tupleRegex);
		Matcher m = p.matcher(tuple);
		return m.matches();
	}
	
	/**
	 * Checks if a model-element represented by a string exists.
	 * @param model the {@link Model} containing the model-element
	 * @param elementName String-representation of the element
	 * @return false if the String doesn't represent a model-element or represents more than one element, true otherwise
	 */
	static boolean findModelElement(final Model model, final String elementName) {
		try {
			UMLHelper.getElementByName(model, elementName);
		} catch (ModelElementNotFoundException e) {
			return false;
		}
		return true;
	}
	
	/**
	 * Returns a list with all StereotypApplications at the given element and its subelements.
	 * @param element the element to check for StereotypeApplications
	 * @return List with all StereotypeApplications attached to the given Element or its subelements
	 */
	private static List<StereotypeApplication> getAllStereoApps(final Element element) {
		List<StereotypeApplication> stereotypeApplications = UMLHelper.getStereotypeApplications(element);
		for (Element ele : element.getOwnedElements()) {
			stereotypeApplications.addAll(getAllStereoApps(ele));
		}
		return stereotypeApplications;
	}
	
}
