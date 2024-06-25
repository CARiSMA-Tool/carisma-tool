package carisma.check.policycreation;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;
import org.json.JSONObject;

import ODRLCommonVocabulary.Action;
import ODRLCommonVocabulary.AssetRelationType;
import ODRLCommonVocabulary.ConflictStrategy;
import ODRLCommonVocabulary.ConstraintOperator;
import ODRLCommonVocabulary.LeftOperand;
import ODRLCommonVocabulary.LogicalOperator;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.ODRLCommonVocabularyFactory;
import ODRLCommonVocabulary.PartyFunctionType;
import ODRLCommonVocabulary.PolicyType;
import ODRLCommonVocabulary.impl.ODRLCommonVocabularyFactoryImpl;
import ODRLCommonVocabulary.util.ODRLCommonVocabularySwitchImpl;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.AcceptTracking;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Aggregate;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Annotate;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Anonymize;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Archive;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Attribute;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Attribution;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.CommercialUse;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Compensate;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.ConcurrentUse;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Delete;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.DerivativeWorks;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Derive;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Digitize;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Display;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Distribute;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Distribution;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.EnsureExclusivity;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Execute;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Extract;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Give;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.GrantUse;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Include;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Index;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Inform;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Install;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Modify;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Move;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.NextPolicy;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Notice;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.ObtainConsent;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Play;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Present;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Print;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Read;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Reproduce;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Reproduction;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.ReviewPolicy;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Sell;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.ShareAlike;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Sharing;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.SourceCode;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Stream;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Synchronize;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.TextToSpeech;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Transform;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Translate;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Uninstall;
import carisma.profile.uconcreation.odrl.common.internal.classes.action.Watermark;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.AttributedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.CompensatedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.CompensatingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.ConsentedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.ConsentingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.ContractedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.ContractingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.InformedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.InformingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.TrackedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.TrackingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.AbsoluteAssetPosition;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.AbsoluteAssetSize;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.AbsoluteSpatialAssetPosition;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.AbsoluteTemporalAssetPosition;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.AssetPercentage;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.Count;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.DateTime;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.DelayPeriod;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.DeliveryChannel;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.ElapsedTime;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.Event;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.FileFormat;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.GeospatialCoordinates;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.GeospatialNamedArea;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.IndustryContext;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.Language;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.MediaContext;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.MeteredTime;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.PaymentAmount;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.ProductContext;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.Purpose;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.Recipient;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.RecurringTimeInterval;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.RelativeAssetPosition;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.RelativeAssetSize;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.RelativeSpatialAssetPosition;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.RelativeTemporalAssetPosition;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.RenditionResolution;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.SystemDevice;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.UnitOfCount;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.Version;
import carisma.profile.uconcreation.odrl.common.internal.classes.leftoperand.VirtualItCommunicationLocation;
import carisma.profile.uconcreation.odrl.common.internal.classes.policy.Assertion;
import carisma.profile.uconcreation.odrl.common.internal.classes.policy.Privacy;
import carisma.profile.uconcreation.odrl.common.internal.classes.policy.Request;
import carisma.profile.uconcreation.odrl.common.internal.classes.policy.Ticket;
import carisma.profile.uconcreation.odrl.common.internal.classes.relation.Output;
import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.action.TransferOwnership;
import carisma.profile.uconcreation.odrl.core.internal.classes.action.Use;
import carisma.profile.uconcreation.odrl.core.internal.classes.asset.Asset;
import carisma.profile.uconcreation.odrl.core.internal.classes.asset.AssetCollection;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.Permit;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.Prohibit;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.VoidPolicy;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.Constraint;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintList;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.LogicalConstraint;
import carisma.profile.uconcreation.odrl.core.internal.classes.failure.Consequence;
import carisma.profile.uconcreation.odrl.core.internal.classes.failure.Failure;
import carisma.profile.uconcreation.odrl.core.internal.classes.failure.Remedy;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Assignee;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Assigner;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Function;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.And;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.AndSequence;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.Operand;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.Or;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.Xone;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.EqualTo;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.GreaterEq;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.GreaterThan;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.HasPart;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.IsA;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.IsAllOf;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.IsAnyOf;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.IsNoneOf;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.IsPartOf;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.LessThan;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.LessThanEq;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.NotEqualTo;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.Operator;
import carisma.profile.uconcreation.odrl.core.internal.classes.party.Party;
import carisma.profile.uconcreation.odrl.core.internal.classes.party.PartyCollection;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Agreement;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Offer;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Policy;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Set;
import carisma.profile.uconcreation.odrl.core.internal.classes.relation.Relation;
import carisma.profile.uconcreation.odrl.core.internal.classes.relation.Target;
import carisma.profile.uconcreation.odrl.core.internal.classes.rightoperand.RightOperandInterface;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Duty;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Permission;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Prohibition;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Rule;

/** Contains a Simple CARiSMA Check which returns all elements of a given Model.
 *
 */

public class Check implements CarismaCheckWithID {
	public static final String CHECK_ID = "carisma.check.policycreation";

	public static final String CHECK_NAME = "Policy Model Transformation";
	
//--------------
	AnalysisHost host;

	Map<EObject,ODRLClass> referencingList2 = new HashMap<>();
	Map<String,Collection<ODRLClass>> typeBuckets = new HashMap<String,Collection<ODRLClass>>();//TODO Not used anymore
	ODRLClass root;
	Package usedPackage;
	//Map<JSONObject>
	final static String typeString = "@type";
	final static String nullString = "Null";
	//possibly add emptyString-Variable since empty strings are treated as Null-Strings
	final static String profileName = "ODRLCommonVocabulary";
	ODRLCommonVocabularyPackage odrlPackage = ODRLCommonVocabularyPackage.eINSTANCE;
	ODRLCommonVocabularySwitchImpl<? extends ODRLClass> odrlSwitch = new ODRLCommonVocabularySwitchImpl<>();//TODO:remove
	//TODO remove
	int numOfElements = 0;
	//
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		System.out.println("Starting Policycheck performance");
		this.host = host;
		this.numOfElements = 0;
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
//		for (EObject content : currentModel.getContents()) {
//			System.out.println("Model content:" + content);
//		}

		if (currentModel.getContents().get(0) instanceof Package model) {
			
			
			//
//			usedPackage = model;
//			for (Stereotype s : usedPackage.getAppliedProfile(profileName).getOwnedStereotypes())
//			System.out.println("-----------------Owned Stereotype: " + s);
//			for (NamedElement ne : usedPackage.getAppliedProfile(profileName).getOwnedMembers())
//				System.out.println("-------|||--------Named Element: " + ne + " --------||||------- " + ne.eClass());
			//
			
			//
//			for (Element e : model.allOwnedElements()) {
//				System.out.println("Owned Element: " + e);
//				
//				for (Stereotype st : e.getAppliedStereotypes()) {
//					System.out.println("Stereotype: "+st);
//					if (st.getName().equals("ODRL-Policy")) {
//					}
//					if (st.getQualifiedName().equals(profileName + "::Permission")) {
//						if (e.getValue(st, "refinement") instanceof EObject) {
//							EObject datatype = (EObject) e.getValue(st, "refinement");
//							System.out.println("           -------------             "+ datatype);
//							
//							for (EStructuralFeature f : datatype.eClass().getEAllStructuralFeatures()) {
//								System.out.println(f);
//								Object value = datatype.eGet(f);
//								if (value != null) {
//								System.out.println(datatype.eGet(f));
//								System.out.println(datatype.eGet(f).getClass());
//								}
//						}
//							System.out.println("-|-|-|-<");
//						}
//					}
//				}
//			}
//			for (NamedElement ne : model.getMembers()) {
//				System.out.println(ne.getName() + "      " + ne);
//			}

			//Convert the UML-Model to the Java-Class-Model used here
			ODRLCommonVocabularyFactory factory = ODRLCommonVocabularyFactory.eINSTANCE;
			System.out.println("Registered package: " + EPackage.Registry.INSTANCE.keySet());
			System.out.println("Test of duty: " + factory.eINSTANCE.createDuty().eClass().getEPackage());
			System.out.println("Package: " + odrlPackage);
//			for (Element e : model.allOwnedElements()) {
//				odrlSwitch.doSwitch(e);//TODO remove
//				System.out.println("Classifier id: " + e.eClass().getClassifierID());
//				System.out.println("InstanceClassName: "+ e.eClass().getInstanceClassName() + "           , InstanenceType: " + e.eClass().getInstanceTypeName());
//			}
			structureModel(model);
			//

			printContent(model, "");

			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "number of elements counted: "+numOfElements));
			return true;
			//------
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		return false;
	}
	
	
	
	public void printContent(Element element, String indent) {
		numOfElements++;
		host.appendToReport(indent+element.eClass().getName()+": ");
		if (!element.getAppliedStereotypes().isEmpty()) {
			host.appendToReport("<<");
			for (Stereotype st : element.getAppliedStereotypes()) {
				host.appendToReport(st.getName()+",");
			}
			host.appendToReport(">> ");
		}
		if (element instanceof NamedElement) {
			NamedElement namedElement = (NamedElement)element;
			host.appendToReport(namedElement.getName());
		}
		host.appendLineToReport("");
		for (Element child : element.allOwnedElements()) {
			printContent(child, indent+"  ");
		}
	}
	private void structureModel(Package inputModel) {
		List<JSONObject> printList = new LinkedList<JSONObject>();
		List<ODRLClass> objectList = new LinkedList<ODRLClass>();
		Collection<Element> modelContents = inputModel.allOwnedElements();
		for (Element e : modelContents) {
			System.out.println("Element: " + e);//TODO: remove
			for(EStructuralFeature esf : e.eClass().getEAllStructuralFeatures()) {//TODO remove
				if (e.eGet(esf)!=null) {
					if (e.eGet(esf) instanceof List list && !list.isEmpty())
					System.out.println("Feature: " + esf + "        With value: " + e.eGet(esf));
				}
			}
			for (Stereotype s : e.getAppliedStereotypes()) {
				if (s.getProfile().getQualifiedName().equals(profileName)) { //probably replace qualified name comparison by an more unique identifier
					Object object = (addElement(e.getStereotypeApplication(s), null, e));
					
					if (object instanceof ODRLClass odrlC) {
						System.out.println("Created ODRLObject: " + odrlC);
						JSONObject jso = new JSONObject(odrlC);
						printList.add(jso);
						objectList.add(odrlC);
						System.out.println(jso.toString(4));
					}
					else {
						System.out.println("Null: " + e.getStereotypeApplication(s));
						for (EStructuralFeature esf : e.getStereotypeApplication(s).eClass().getEStructuralFeatures()) {
							System.out.println("ESF: " + esf);
						}
					}
					System.out.println();
				}			
			}
		}
		for (ODRLClass odrlc : objectList) {
			checkProfileRequirements(odrlc);
		}
//		for (JSONObject jobj : printList) {
//			System.out.println(jobj.toString(4));
//			System.out.println();
//		}
	}
		
		
		
	
	
	
	///////////////////////////////
	private Object addElement(EObject currentEObject, ODRLClass odrlParent, EObject activityElement) {//TODO modify the names if the eCore naming derives form the names in generated code
//		odrlSwitch.doSwitch(currentEObject);//TODO remove
//		System.out.println("Classifier id: " + currentEObject.eClass().getClassifierID());//TODO remove
		if (currentEObject==null) {//Not necessary for result
			return null;
		}
		if (referencingList2.get(currentEObject)!= null) {//TODO either add parent to parents-Attribute here (and before the other return-statement) or in the method processing the return value (if that's where it's decided whether it's actually added (for example in case it would, but must not ovewrite a value of the parent))
			System.out.println("already generated: " + currentEObject);
			return referencingList2.get(currentEObject);//Problem: Same enums are used in the generation of different ODRL-Objects (e.g. one action-enum with several different refinements)
		}
		String objectClassName = currentEObject.eClass().getName();
		Object newObject = null;
		
		UMLModelConverter converter = new UMLModelConverter();
		//Filling the generated object
		newObject = converter.addElement(currentEObject, odrlParent, activityElement);
		//fill(currentEObject, newObject, activityElement);//TODO maybe return boolean with the fill()-method to signal whether it was filled sufficiently
		
		
		//
		if (newObject instanceof ODRLClass newOdrlObject) {
			System.out.println("At end of addElement :" + newObject);
			referencingList2.put(currentEObject, newOdrlObject);//Maybe extend the valid keys and add all objects, not just ODRLClasses
		}
		System.out.println(converter.addElement(currentEObject,null,null)==null?"Helper: Null": "Helper:  " + converter.addElement(currentEObject,null,null));
		System.out.println("passed EObject: " + currentEObject);
		return newObject;
	}
	
	
	private Object foo() {
		Map<String,Class> map = new HashMap<>();
		map.put(odrlPackage.getProhibition().getName(), Prohibit.class);
		return map;
	}
	
	
	private <T> List<T> addElement(List currentList, ODRLClass odrlParent, EObject activityElement, Class<T> type) {//No check for several layers of lists as that case does not occur in the current model
		List<T> newOdrlList = new LinkedList<>();
		boolean fullyCompartible = true;
		if (currentList!=null && !currentList.isEmpty()) {
			for (Object o : currentList) {
				if (o instanceof EObject eObj) {
					Object newOdrlObject = addElement(eObj,odrlParent,activityElement);
					if (type.isInstance(newOdrlObject)) {
						newOdrlList.add((T)newOdrlObject);
					} else {
						fullyCompartible=false;
					}
				}
				else if (o instanceof String string) {
					Object newOdrlObject = addElement(string,odrlParent,activityElement);
					if (type.isInstance(newOdrlObject)) {
						newOdrlList.add((T)newOdrlObject);
					} else {
						fullyCompartible=false;
					}
				}
			}
		}
		return newOdrlList.isEmpty()||!fullyCompartible? null : newOdrlList;//Only return a List if all elements of the passed List were of the specified class (and it's not empty)
	}

	
	/*
	private boolean addElement(EObject eParent, EStructuralFeature feature, ODRLClass odrlParent) {
		Object eValue = eParent.eGet(feature);
		//if (eParent instanceof )
			
			return false;
		return false;
	}
	*/
	
	private String addElement(String currentObject, ODRLClass odrlParent, EObject activityElement) {
		return currentObject;
	}
	
	
	private void fill(EObject currentEObject, Object toBeFilled, EObject activityElement) {
		if (toBeFilled instanceof Asset asset) {
			fillAsset(currentEObject, asset, activityElement);
		}
		if (toBeFilled instanceof AssetCollection assetCollection) {
			fillAssetCollection(currentEObject, assetCollection, activityElement);
		}
		if (toBeFilled instanceof Constraint constraint) {
			fillConstraint(currentEObject, constraint, activityElement);
		}
		if (toBeFilled instanceof  LogicalConstraint logCon) {
			fillLogicalConstraint(currentEObject, logCon, activityElement);
		}
		if (toBeFilled instanceof  Failure failure) {
			fillFailure(currentEObject, failure, activityElement);
		}
		if (toBeFilled instanceof  Function function) {
			fillFunction(currentEObject, function, activityElement);
		}
//		if (toBeFilled instanceof  Operand operand) {//Currently empty as Contents need to be filled in the LogicalConstraint-fill-method
//			fillOperand(currentEObject, operand, activityElement);
//		}
		if (toBeFilled instanceof  Party party) {
			fillParty(currentEObject, party, activityElement);
		}
		if (toBeFilled instanceof  PartyCollection partyCollection) {
			fillPartyCollection(currentEObject, partyCollection, activityElement);
		}
		if (toBeFilled instanceof  Policy policy) {
			fillPolicy(currentEObject, policy, activityElement);
		}
		if (toBeFilled instanceof  Relation relation) {
			fillRelation(currentEObject, relation, activityElement);
		}
//		if (toBeFilled instanceof  RightOperand rightOperand) {//TODO add RightOperand-Fillers once the structure of them is set
//		}
		//Rules
		if (toBeFilled instanceof  Duty duty) {
			fillDuty(currentEObject, duty, activityElement);
		}
		if (toBeFilled instanceof  Permission permission) {
			fillPermission(currentEObject, permission, activityElement);
		}
		if (toBeFilled instanceof  Prohibition prohibition) {
			fillProhibition(currentEObject, prohibition, activityElement);
		}
		if (toBeFilled instanceof  Rule rule) {
			fillRule(currentEObject, rule, activityElement);
		}
	}
	
	//Filling-Methods for assets
	private void fillAsset(EObject currentEObject, Asset asset, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getAsset_Uid().getName());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {
			
			asset.setUid(stringValue);
		}
	}
	private void fillAssetCollection(EObject currentEObject, AssetCollection assetCollection, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getAssetCollection_Source().getName());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {
			
			assetCollection.setSource(stringValue);
		}
		attributeValue = getValue(currentEObject, odrlPackage.getRefinableElement_Refinement().getName()); 
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = addElement(newEObj, assetCollection, activityElement);
			if (attributeValueOdrl instanceof ConstraintInterface refinement) {
				assetCollection.setRefinement(refinement);
			}
		}
	}
	//Filling-Methods for Conflict (TODO only maybe add (as the methods would be empty))
	//Filling-Methods for Constraints (TODO maybe add ConstraintInterface)
	private void fillConstraint(EObject currentEObject, Constraint constraint, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getConstraint_DataType().getName());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {		
			constraint.setDataType(stringValue);
		}
		attributeValue = getValue(currentEObject, odrlPackage.getConstraint_LeftOperand().getName());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = addElement(newEObj, constraint, activityElement);
			if (attributeValueOdrl instanceof carisma.profile.uconcreation.odrl.core.internal.classes.leftoperand.LeftOperand leftOperand) {
				constraint.setLeftOperand(leftOperand);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getConstraint_Operator().getName());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = addElement(newEObj, constraint, activityElement);
			if (attributeValueOdrl instanceof Operator operator) {
				constraint.setOperator(operator);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getConstraint_RightOperand().getName());
		if (attributeValue instanceof List list) { //TODO List attribute, also rightOperand not yet implemented
			List<RightOperandInterface> attributeValueOdrl = addElement(list, constraint, activityElement, RightOperandInterface.class);
			if (attributeValueOdrl!=null) {
				constraint.setRightOperand(attributeValueOdrl);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getConstraint_RightOperandReference().getName());
		if (attributeValue instanceof List list) { //TODO List attribute, also rightOperand not yet implemented
			List<String> attributeValueOdrl = addElement(list, constraint, activityElement, String.class);
			if (attributeValueOdrl!=null) {
				constraint.setRightOperandReference(attributeValueOdrl);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getConstraint_Status().getName());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {			
			constraint.setStatus(stringValue);
		}
		attributeValue = getValue(currentEObject, odrlPackage.getConstraint_Uid().getName());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {		
				constraint.setUid(stringValue);			
		}
		attributeValue = getValue(currentEObject, odrlPackage.getConstraint_Unit().getName());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {		
			constraint.setUnit(stringValue);
		}
	}
	private void fillLogicalConstraint(EObject currentEObject, LogicalConstraint logicalConstraint, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getLogicalConstraint_LogicalOperator().getName());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = addElement(newEObj, logicalConstraint, activityElement);
			if (attributeValueOdrl instanceof Operand operand) {
				logicalConstraint.setOperand(operand);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getLogicalConstraint_Constraints().getName());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<Constraint> attributeValueOdrl = addElement(list, logicalConstraint, activityElement, Constraint.class);
			if (attributeValueOdrl!=null&&logicalConstraint.getOperand()!=null) {//TODO Maybe remove operand-nullcheck, as it being null would point to a faulty model
				logicalConstraint.getOperand().setConstraints(attributeValueOdrl);//(After creation of operand earlier in this method) set constraints to it
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getLogicalConstraint_Uid().getName());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {
			logicalConstraint.setUid(stringValue);
		}
	}
	//Filling-Methods for Failures (TODO maybe add empty subproperties)
	private void fillFailure(EObject currentEObject, Failure failure, EObject activityElement) {//TODO failure currently not present on the uml-profile
		//Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage));
		//if (attributeValue instanceof EObject newEObj) { //TODO List attribute
		//}
	}
	//Filling-Methods for Functions
	private void fillFunction(EObject currentEObject, Function function, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getPartyFunction_Party().getName());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = addElement(newEObj, function, activityElement);
			if (attributeValueOdrl instanceof Party party) {
				function.setParty(party);
			}
		}
		
	}
	//Filling-Methods for Leftoperands (TODO only maybe add (as the methods would be empty))
	
	//Filling-Methods for Operands (TODO maybe add empty subproperties)  //Filling is done in the owning LogicalConstraint currently as it owns the Constraint list
//	private void fillOperand(EObject currentEObject, Operand operand, EObject activityElement) {
//	}
	//Filling-Methods for Operators (TODO only maybe add (as the methods would be empty))
	
	//Filling-Methods for Parties
	private void fillParty(EObject currentEObject, Party party, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getParty_Uid().getName());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {	
			party.setUid(stringValue);
		}
	}
	private void fillPartyCollection(EObject currentEObject, PartyCollection partyCollection, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getPartyCollection_Source().getName());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {
			partyCollection.setSource(stringValue);
		}
		attributeValue = getValue(currentEObject, odrlPackage.getRefinableElement_Refinement().getName()); 
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = addElement(newEObj, partyCollection, activityElement);
			if (attributeValueOdrl instanceof ConstraintInterface refinement) {
				partyCollection.setRefinement(refinement);
			}
		}
	}	
	//Filling-methods for Policies (TODO maybe add empty subclass-methods)
	private void fillPolicy(EObject currentEObject, Policy policy, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getODRLPolicy_ConflictStrategy().getName());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = addElement(newEObj, policy, activityElement);
			if (attributeValueOdrl instanceof carisma.profile.uconcreation.odrl.core.internal.classes.conflict.ConflictStrategy conflictValue) {
				policy.setConflictStrategy(conflictValue);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getODRLPolicy_InheritsFrom().getName());
		if (attributeValue instanceof List list) { //TODO String List attribute
			List<String> attributeValueOdrl = addElement(list, policy, activityElement, String.class);
			if (attributeValueOdrl!=null) {
				policy.setInheritsFrom(attributeValueOdrl);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getODRLPolicy_Profiles().getName());
		if (attributeValue instanceof List list) { //TODO String List attribute
			List<String> attributeValueOdrl = addElement(list, policy, activityElement, String.class);
			if (attributeValueOdrl!=null) {
				policy.setProfiles(attributeValueOdrl);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getODRLPolicy_Uid().getName());
		if (attributeValue instanceof String string) {
			policy.setUid(string);
		}
	}
	//Filling-Methods for Relation
	private void fillRelation(EObject currentEObject, Relation relation, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getAssetRelation_Asset().getName());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = addElement(newEObj, relation, activityElement);
			if (attributeValueOdrl instanceof Asset asset) {
				relation.setAsset(asset);
			}
		}
	}
	//Filling-Methods for RightOperands TODO deal with once the different RightOperandInterface-implementers are finished	
	
	//Filling-methods for rules
	private void fillRule(EObject currentEObject, Rule rule, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getRule_Action().getName());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = addElement(newEObj, rule, activityElement);
			if (attributeValueOdrl instanceof carisma.profile.uconcreation.odrl.core.internal.classes.action.Action action) {
				rule.setAction(action);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getRefinableElement_Refinement().getName());
		if (attributeValue instanceof EObject newEObj) {//TODO get constraint
			Object attributeValueOdrl = addElement(newEObj, rule.getAction(), activityElement);
			if (attributeValueOdrl instanceof ConstraintInterface constraintInterface) {
				//if (attributeValueOdrl instanceof List constraintList) {TODO add seperate cases for logicalConstraint and List of constraints (in the 2nd case possibly also add instead of set)
				//	rule.getConstraint().
				//}
				if (rule.getAction()!=null) {//TODO also add null check for other cases where a gotten object is further used or keep the nullpointer as sign that something is missing
					rule.getAction().setRefinement(constraintInterface);
				}
			}
			
		}
		attributeValue = getValue(currentEObject, odrlPackage.getRule_Uid().getName());
		if (attributeValue instanceof String string) {
			rule.setUid(string);
		}
		attributeValue = getValue(currentEObject, odrlPackage.getRule_InvolvedAssets().getName());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<Relation> attributeValueOdrl = addElement(list, rule, activityElement, Relation.class);
			if (attributeValueOdrl!=null) {
				rule.setInvolvedAssets(attributeValueOdrl);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getRule_InvolvedParties().getName());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<Function> attributeValueOdrl = addElement(list, rule, activityElement, Function.class);
			if (attributeValueOdrl!=null) {
				rule.setInvolvedParties(attributeValueOdrl);
			}
		}
		attributeValue = getValue(currentEObject, odrlPackage.getConstrainableElement_Constraint().getName());
		if (attributeValue instanceof EObject newEObj) {//TODO get constraint
			Object attributeValueOdrl = addElement(newEObj, rule, activityElement);
			if (attributeValueOdrl instanceof ConstraintInterface constraintInterface) {
				//if (attributeValueOdrl instanceof List constraintList) {TODO maybe add seperate cases for logicalConstraint and List of constraints (in the 2nd case possibly also add instead of set)
				//	rule.getConstraint().
				//}
				rule.setConstraint(constraintInterface);
			}
		}		
	}
	private void fillPermission(EObject currentEObject, Permission permission, EObject activityElement) {
		Object attributeValue = getValue(currentEObject, odrlPackage.getPermission_Duties().getName());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<Duty> attributeValueOdrl = addElement(list, permission, activityElement, Duty.class);
			if (attributeValueOdrl!=null) {
				permission.setDuties(attributeValueOdrl);
			}
		}
	}
	private void fillProhibition(EObject currentEObject, Prohibition prohibition, EObject activityElement) {
		//Currently leads to properties of unrelated duty being taken over
		EStructuralFeature remedyFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getProhibition_Remedies().getName());
		if (getValue(currentEObject,odrlPackage.getProhibition_Remedies().getName()) != null) {
			Object attributeValueOdrl = addElement(remedyFeature, prohibition, activityElement);
			if (attributeValueOdrl instanceof Remedy remedy) {
				prohibition.setRemedy(remedy);
			}
		}//TODO only set the remedy if its rules-Property is not empty (in ecore it has the empty list, making the remedy and List non-null in any case). Possibility: fillRemedies
		Object attributeValue = getValue(currentEObject,odrlPackage.getProhibition_Remedies().getName());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<Duty> attributeValueOdrl = addElement(list, prohibition.getRemedy(), activityElement, Duty.class);
			if (attributeValueOdrl!=null) {
				if (prohibition.getRemedy().getRules()==null)
					prohibition.getRemedy().setRules(new LinkedList<Rule>());
				prohibition.getRemedy().getRules().addAll(attributeValueOdrl);//TODO change getters to conditional generators or add null-checks with additional creation everywhere were gotten objects are further used
			}
		}
	}
	private void fillDuty(EObject currentEObject, Duty duty, EObject activityElement) {
//		EStructuralFeature consequenceFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getDuty_Consequences().getName());
//		if (getValue(currentEObject,odrlPackage.getDuty_Consequences().getName()) != null) {
//			System.out.println("ConsequenceValue" + getValue(currentEObject,odrlPackage.getDuty_Consequences().getName()));
//			Object attributeValueOdrl = addElement(consequenceFeature, duty, activityElement);
//			if (attributeValueOdrl instanceof Consequence consequence) {
//				duty.setConsequences(consequence);
//			}
//		}
		//Following part currently leads to stack overflow when JSON-Objects are created
//		Object attributeValue = getValue(currentEObject, odrlPackage.getDuty_Consequences().getName());
//		if (attributeValue instanceof List list) { //TODO List attribute
//			List<Duty> attributeValueOdrl = addElement(list, duty.getConsequences(), activityElement, Duty.class);
//			if (attributeValueOdrl!=null) {
//				if (duty.getConsequences().getRules()==null)
//					duty.getConsequences().setRules(new LinkedList<Rule>());
//				duty.getConsequences().getRules().addAll(attributeValueOdrl);//TODO change getters to conditional generators or add null-checks with additional creation everywhere were gotten objects are further used
//			}
//		}
	}
	
	
	private Object getValue(EObject eObject, String featureName) {
//		EStructuralFeature feature = eObject.eClass().getEStructuralFeature(featureName);
//		if(feature == null) {
//			//TODO add missing feature information?
//		}
//		return feature==null ? null : eObject.eGet(feature);
		return eObject.eGet(eObject.eClass().getEStructuralFeature(featureName));//Nullpointer-exception with null-feature can only be produced by code errors, not by input errors
	}
	
	
	///////////////////////////////
	
	private void checkProfileRequirements(ODRLClass testedElement) {//replace testedElement with the created objects
		//TODO no checks for valid form of IRIs so far
		if (testedElement instanceof Policy policy) {
			if (policy.getPermission().isEmpty()
					&& policy.getProhibition().isEmpty()
					&& policy.getObligation().isEmpty()) {
				//TODO add warning: invalid policy: needs to have at least one permission, prohibition or obligation
				addWarning("Invalid Policy. Policy needs to have at least one permission, prohibition or obligation.",testedElement);;
			}
			
			if (policy instanceof Offer offer) {
				//TODO one assigner (only one? needed with every rule or just with one?)
			} else if (policy instanceof Agreement agreement) {
				//TODO one assigner, one assignee
			}
		} else if (testedElement instanceof AssetCollection assetCollection) {
			if (assetCollection.getRefinement()!=null
					&& assetCollection.getSource()==null) {
				addWarning("Invalid assetCollection: source-property needs to be used with refinement.", testedElement);
			}
		} else if (testedElement instanceof PartyCollection partyCollection) {
			if (partyCollection.getRefinement()!=null
					&& partyCollection.getSource()==null) {
				addWarning("Invalid partyCollection: source-property needs to be used with refinement.", testedElement);
			}
		} else if (testedElement instanceof Constraint constraint) {
			if (constraint.getLeftOperand()==null) {
				addWarning("Invalid constraint: needs to have a leftOperand selected.",testedElement);
			}
			if (constraint.getOperator()==null) {
				addWarning("Invalid constraint: needs to have an operator selected.",testedElement);
			}
			if ( (constraint.getRightOperand()==null
					||constraint.getRightOperand().isEmpty() 
					) && ( constraint.getRightOperandReference()==null
					||constraint.getRightOperandReference().isEmpty())) {
				addWarning("Invalid Constraint: needs to have either a rightOperand, or rightOperandReference, has neither.",testedElement);
			}
			if ( (constraint.getRightOperand()!=null
					&& !constraint.getRightOperand().isEmpty() 
					 && constraint.getRightOperandReference()!=null
					&& !constraint.getRightOperandReference().isEmpty())) {
				addWarning("Invalid Constraint: must not have both rightOperand and rightOperandReference (is that the case?)",testedElement);//TODO check if restriction actually exists
			}
		}
		if (testedElement instanceof Rule rule) {
			if (rule.getAction() == null) {
				addWarning("Invalid rule: needs to have an action selected.",testedElement);
			}
			if (testedElement instanceof Permission permission) {
				boolean hasTarget = false;
				for (Relation relation : permission.getInvolvedAssets()) {
					if (relation instanceof Target) {
						hasTarget = true;
					}
				}
				if (!hasTarget) {
					addWarning("Invalid permission: needs to have a relation of type target.",testedElement);
				}
			} else if (testedElement instanceof Prohibition prohibition) {
				boolean hasTarget = false;
				for (Relation relation : prohibition.getInvolvedAssets()) {
					if (relation instanceof Target) {
						hasTarget = true;
					}
				}
				if (!hasTarget) {
					addWarning("Invalid prohibition: needs to have a relation of type target.",testedElement);
				}
				if (prohibition.getRemedy()!=null && prohibition.getRemedy().getRules()!=null && !prohibition.getRemedy().getRules().isEmpty()) {
					for (Rule remedy : prohibition.getRemedy().getRules()) {
						if (!(remedy instanceof Duty)) {
							addWarning("Invalid Prohibition: remedy must be of type Duty.",testedElement);//TODO covered in the Model
						}
						if (remedy instanceof Duty consequenceDuty) {
							if (consequenceDuty.getConsequences()!=null) {
								addWarning("Invalid remedy duty: remedy-Duty must not have a consequence itself.",testedElement);
							}
						}
					}
				}
			} else if (testedElement instanceof Duty duty) {
				if (duty.getConsequences()!=null && duty.getConsequences().getRules()!=null && !duty.getConsequences().getRules().isEmpty()) {
					for (Rule consequence : duty.getConsequences().getRules()) {
						if (!(consequence instanceof Duty)) {
							addWarning("Invalid Duty: consequence must be of type Duty.",testedElement);//TODO covered in the Model
						}
						if (consequence instanceof Duty consequenceDuty) {
							if (consequenceDuty.getConsequences()!=null) {
								addWarning("Invalid consequence duty: consequence-Duty must not have a consequence itself.",testedElement);
							}
						}
					}
				}
			} 
		}		
	}
	private void addWarning(String warning, ODRLClass object) {//possibly add with list of ODRLClass-objects
		host.appendLineToReport("Warning: " + warning + " Found in a " + object.getClass().getSimpleName() + " contained in the " + "object.containingUMLElement.getClass()" + " named " + "object.containingUMLElement.getName()");//TODO: unimplemented parts
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