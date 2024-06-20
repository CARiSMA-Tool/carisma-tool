package carisma.check.policycreation;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;

import ODRLCommonVocabulary.Action;
import ODRLCommonVocabulary.AssetRelation;
import ODRLCommonVocabulary.AssetRelationType;
import ODRLCommonVocabulary.ConflictStrategy;
import ODRLCommonVocabulary.ConstraintOperator;
import ODRLCommonVocabulary.LeftOperand;
import ODRLCommonVocabulary.LogicalOperator;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.PartyFunctionType;
import ODRLCommonVocabulary.PolicyType;
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
import carisma.profile.uconcreation.odrl.common.internal.classes.function.InformingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.TrackedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.TrackingParty;
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
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintList;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.LogicalConstraint;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Assignee;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Assigner;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.And;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.AndSequence;
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
import carisma.profile.uconcreation.odrl.core.internal.classes.party.Party;
import carisma.profile.uconcreation.odrl.core.internal.classes.party.PartyCollection;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Agreement;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Offer;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Policy;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Set;
import carisma.profile.uconcreation.odrl.core.internal.classes.relation.Target;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Duty;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Permission;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Prohibition;

public class UMLModelConverter {
	private  final Map<String,Map<String,Class<? extends ODRLClass>>> enumMap = new HashMap<>();
	private  final Map<String,String> typeEnumMap1 = new HashMap<>();
	private  final Map<String,Map<String,Class<? extends ODRLClass>>> typeEnumMap2 = new HashMap<>();
	private  final Map<String,Class<? extends ODRLClass>> classMap = new HashMap<>();
	private  final ODRLCommonVocabularyPackage odrlPackage = ODRLCommonVocabularyPackage.eINSTANCE;
	
	private Map<EObject,ODRLClass> referencingMap = new HashMap<>();//Save with unique EObject, watch out for uniqueness of enums (may need to be saved as triple (the )
	//Also save lists, not just their elements
	
	 {
		
		enumMap.put(ConflictStrategy.class.getSimpleName(), Map.ofEntries(
				Map.entry(ConflictStrategy.PERMIT.getName(),Permit.class),
				Map.entry(ConflictStrategy.PROHIBIT.getName(), Prohibit.class),
				Map.entry(ConflictStrategy.VOID_POLICY.getName(), VoidPolicy.class)				
				));
		enumMap.put(Action.class.getSimpleName(), Map.ofEntries(
				Map.entry(Action.ACCEPT_TRACKING.getName(), AcceptTracking.class),
				Map.entry(Action.AGGREGATE.getName(), Aggregate.class),
				Map.entry(Action.ANNOTATE.getName(), Annotate.class),
				Map.entry(Action.ANONYMIZE.getName(), Anonymize.class),
				Map.entry(Action.ARCHIVE.getName(), Archive.class),
				Map.entry(Action.ATTRIBUTE.getName(), Attribute.class),
				Map.entry(Action.CC_ATTRIBUTION.getName(), Attribution.class),
				Map.entry(Action.CC_COMMERCIAL_USE.getName(), CommercialUse.class),
				Map.entry(Action.CC_DERIVATIVE_WORKS.getName(), DerivativeWorks.class),
				Map.entry(Action.CC_DISTRIBUTION.getName(), Distribution.class),
				Map.entry(Action.CC_NOTICE.getName(), Notice.class),
				Map.entry(Action.CC_REPRODUCTION.getName(), Reproduction.class),
				Map.entry(Action.CC_SHARE_ALIKE.getName(), ShareAlike.class),
				Map.entry(Action.CC_SHARING.getName(), Sharing.class),
				Map.entry(Action.CC_SOURCE_CODE.getName(), SourceCode.class),
				Map.entry(Action.COMPENSATE.getName(), Compensate.class),
				Map.entry(Action.CONCURRENT_USE.getName(), ConcurrentUse.class),
				Map.entry(Action.DELETE.getName(), Delete.class),
				Map.entry(Action.DERIVE.getName(), Derive.class),
				Map.entry(Action.DIGITIZE.getName(), Digitize.class),
				Map.entry(Action.DISPLAY.getName(), Display.class),
				Map.entry(Action.DISTRIBUTE.getName(), Distribute.class),
				Map.entry(Action.ENSURE_EXCLUSIVITY.getName(), EnsureExclusivity.class),
				Map.entry(Action.EXECUTE.getName(), Execute.class),
				Map.entry(Action.EXTRACT.getName(), Extract.class),
				Map.entry(Action.GIVE.getName(), Give.class),
				Map.entry(Action.GRANT_USE.getName(), GrantUse.class),
				Map.entry(Action.INCLUDE.getName(), Include.class),
				Map.entry(Action.INDEX.getName(), Index.class),
				Map.entry(Action.INFORM.getName(), Inform.class),
				Map.entry(Action.INSTALL.getName(), Install.class),
				Map.entry(Action.MODIFY.getName(), Modify.class),
				Map.entry(Action.MOVE.getName(), Move.class),
				Map.entry(Action.NEXT_POLICY.getName(), NextPolicy.class),
				Map.entry(Action.OBTAIN_CONSENT.getName(), ObtainConsent.class),
				Map.entry(Action.PLAY.getName(), Play.class),
				Map.entry(Action.PRESENT.getName(), Present.class),
				Map.entry(Action.PRINT.getName(), Print.class),
				Map.entry(Action.READ.getName(), Read.class),
				Map.entry(Action.REPRODUCE.getName(), Reproduce.class),
				Map.entry(Action.REVIEW_POLICY.getName(), ReviewPolicy.class),
				Map.entry(Action.SELL.getName(), Sell.class),
				Map.entry(Action.STREAM.getName(), Stream.class),
				Map.entry(Action.SYNCHRONIZE.getName(), Synchronize.class),
				Map.entry(Action.TEXT_TO_SPEECH.getName(), TextToSpeech.class),
				Map.entry(Action.TRANSFER.getName(), TransferOwnership.class),
				Map.entry(Action.TRANSFORM.getName(), Transform.class),
				Map.entry(Action.TRANSLATE.getName(), Translate.class),
				Map.entry(Action.UNINSTALL.getName(), Uninstall.class),
				Map.entry(Action.USE.getName(), Use.class),
				Map.entry(Action.WATERMARK.getName(), Watermark.class)
				));
		enumMap.put(ConstraintOperator.class.getSimpleName(), Map.ofEntries(
				Map.entry(ConstraintOperator.EQ.getName(), EqualTo.class),
				Map.entry(ConstraintOperator.GTEQ.getName(), GreaterEq.class),
				Map.entry(ConstraintOperator.GT.getName(), GreaterThan.class),
				Map.entry(ConstraintOperator.HAS_PART.getName(), HasPart.class),
				Map.entry(ConstraintOperator.IS_A.getName(), IsA.class),
				Map.entry(ConstraintOperator.IS_ALL_OF.getName(), IsAllOf.class),
				Map.entry(ConstraintOperator.IS_ANY_OF.getName(), IsAnyOf.class),
				Map.entry(ConstraintOperator.IS_NONE_OF.getName(), IsNoneOf.class),
				Map.entry(ConstraintOperator.IS_PART_OF.getName(), IsPartOf.class),
				Map.entry(ConstraintOperator.LT.getName(), LessThan.class),
				Map.entry(ConstraintOperator.LTEQ.getName(), LessThanEq.class),
				Map.entry(ConstraintOperator.NEQ.getName(), NotEqualTo.class)
				));
		enumMap.put(LogicalOperator.class.getSimpleName(),Map.ofEntries(
				Map.entry(LogicalOperator.AND.getName(), And.class),
				Map.entry(LogicalOperator.AND_SEQUENCE.getName(), AndSequence.class),
				Map.entry(LogicalOperator.OR.getName(), Or.class),
				Map.entry(LogicalOperator.XONE.getName(), Xone.class)
				//The Null-Case and its special implications (Constraint instead of LogicalConstraint) are handled in the LogicalConstraint-case
				));
		enumMap.put(LeftOperand.class.getSimpleName(), Map.ofEntries(
				//TODO absolute position seems to be missing in UMl-Model
				Map.entry(LeftOperand.ABSOLUTE_SIZE.getName(), AbsoluteAssetSize.class),
				Map.entry(LeftOperand.ABSOLUTE_SPARTIAL_POSITION.getName(), AbsoluteSpatialAssetPosition.class),//TODO correct spelling from spartial to spatial
				Map.entry(LeftOperand.ABSOLUTE_TEMPORAL_POSITION.getName(), AbsoluteTemporalAssetPosition.class),
				Map.entry(LeftOperand.PERCENTAGE.getName(), AssetPercentage.class),
				Map.entry(LeftOperand.COUNT.getName(), Count.class),
				Map.entry(LeftOperand.DATE_TIME.getName(), DateTime.class),
				Map.entry(LeftOperand.DELAY_PERIOD.getName(), DelayPeriod.class),
				Map.entry(LeftOperand.DELIVERY_CHANNEL.getName(), DeliveryChannel.class),
				Map.entry(LeftOperand.ELAPSED_TIME.getName(), ElapsedTime.class),
				Map.entry(LeftOperand.EVENT.getName(), Event.class),
				Map.entry(LeftOperand.FILE_FORMAT.getName(), FileFormat.class),
				Map.entry(LeftOperand.SPARTIAL_COORDINATES.getName(), GeospatialCoordinates.class),//TODO correct spelling from spartial to spatial
				Map.entry(LeftOperand.SPARTIAL.getName(), GeospatialNamedArea.class),//TODO correct spelling from spartial to spatial
				Map.entry(LeftOperand.INDUSTRY.getName(), IndustryContext.class),
				Map.entry(LeftOperand.LANGUAGE.getName(), Language.class),
				Map.entry(LeftOperand.MEDIA.getName(), MediaContext.class),
				Map.entry(LeftOperand.METERED_TIME.getName(), MeteredTime.class),
				Map.entry(LeftOperand.PAY_AMOUNT.getName(), PaymentAmount.class),
				Map.entry(LeftOperand.PRODUCT.getName(), ProductContext.class),
				Map.entry(LeftOperand.PURPOSE.getName(), Purpose.class),
				Map.entry(LeftOperand.RECIPIENT.getName(), Recipient.class),
				Map.entry(LeftOperand.TIME_INTERVAL.getName(), RecurringTimeInterval.class),
				Map.entry(LeftOperand.RELATIVE_POSITION.getName(), RelativeAssetPosition.class),
				Map.entry(LeftOperand.RELATIVE_SIZE.getName(), RelativeAssetSize.class),
				Map.entry(LeftOperand.RELATIVE_SPARTIAL_POSITION.getName(), RelativeSpatialAssetPosition.class),//TODO correct spelling from spartial to spatial
				Map.entry(LeftOperand.RELATIVE_TEMPORAL_POSITION.getName(), RelativeTemporalAssetPosition.class),
				Map.entry(LeftOperand.RESOLUTION.getName(), RenditionResolution.class),
				Map.entry(LeftOperand.DEVICE.getName(), SystemDevice.class),
				Map.entry(LeftOperand.UNIT_OF_COUNT.getName(), UnitOfCount.class),
				Map.entry(LeftOperand.VERSION.getName(), Version.class),
				Map.entry(LeftOperand.VIRTUAL_LOCATION.getName(), VirtualItCommunicationLocation.class)
				));
		typeEnumMap1.put(odrlPackage.getODRLPolicy().getName(), odrlPackage.getODRLPolicy_PolicyType().getName());
		typeEnumMap2.put(odrlPackage.getODRLPolicy().getName(), Map.ofEntries(
				Map.entry(PolicyType.AGREEMENT.getName(), Agreement.class),
				Map.entry(PolicyType.ASSERTION.getName(), Assertion.class),
				Map.entry(PolicyType.OFFER.getName(), Offer.class),
				Map.entry(PolicyType.PRIVACY.getName(), Privacy.class),
				Map.entry(PolicyType.REQUEST.getName(), Request.class),
				Map.entry(PolicyType.SET.getName(), Set.class),
				Map.entry(PolicyType.TICKET.getName(), Ticket.class),
				Map.entry(PolicyType.NULL.getName(), Policy.class)//No type-information (is interpreted as Set-Policy by evaluators)
				));
		typeEnumMap1.put(odrlPackage.getAssetRelation().getName(), odrlPackage.getAssetRelation_Type().getName());
		typeEnumMap2.put(odrlPackage.getAssetRelation().getName(), Map.ofEntries(
				Map.entry(AssetRelationType.TARGET.getName(), Target.class),
				Map.entry(AssetRelationType.OUTPUT.getName(), Output.class)
				));
		typeEnumMap1.put(odrlPackage.getPartyFunction().getName(), odrlPackage.getPartyFunction_Type().getName());
		typeEnumMap2.put(odrlPackage.getPartyFunction().getName(), Map.ofEntries(
				Map.entry(PartyFunctionType.ASSIGNEE.getName(), Assignee.class),
				Map.entry(PartyFunctionType.ASSIGNER.getName(), Assigner.class),
				Map.entry(PartyFunctionType.ATTRIBUTED_PARTY.getName(), AttributedParty.class),
				Map.entry(PartyFunctionType.COMPENSATED_PARTY.getName(), CompensatedParty.class),
				Map.entry(PartyFunctionType.COMPENSATING_PARTY.getName(), CompensatingParty.class),
				Map.entry(PartyFunctionType.CONSENTED_PARTY.getName(), ConsentedParty.class),
				Map.entry(PartyFunctionType.CONSENTING_PARTY.getName(), ConsentingParty.class),
				Map.entry(PartyFunctionType.CONTRACTED_PARTY.getName(), ContractedParty.class),
				Map.entry(PartyFunctionType.CONTRACTING_PARTY.getName(), ContractingParty.class),
				Map.entry(PartyFunctionType.INFORMED_PARTY.getName(), InformingParty.class),
				Map.entry(PartyFunctionType.TRACKED_PARTY.getName(), TrackedParty.class),
				Map.entry(PartyFunctionType.TRACKING_PARTY.getName(), TrackingParty.class)
				));
		classMap.putAll(Map.ofEntries(
				Map.entry(odrlPackage.getPermission().getName(), Permission.class),
				Map.entry(odrlPackage.getProhibition().getName(), Prohibition.class),
				Map.entry(odrlPackage.getDuty().getName(), Duty.class)
				));
		classMap.put(odrlPackage.getConstraint().getName(), Constraint.class);
		classMap.putAll(Map.ofEntries(
				Map.entry(odrlPackage.getAsset().getName(), Asset.class),
				Map.entry(odrlPackage.getAssetCollection().getName(), AssetCollection.class)
				));
		classMap.putAll(Map.ofEntries(
				Map.entry(odrlPackage.getParty().getName(), Party.class),
				Map.entry(odrlPackage.getPartyCollection().getName(), PartyCollection.class)
				));
		
		//Missing: LogicalConstraint, StructuralFeatures
	}
	
	private  Object specialCases(EObject currentEObject, ODRLClass odrlParent, EObject activityElement) {
		ODRLClass newObject = null;
		String objectClassName = currentEObject.eClass().getName();
		if (objectClassName.equals(odrlPackage.getLogicalConstraint().getName())) {
			EStructuralFeature classFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getLogicalConstraint_LogicalOperator().getName());
			if (currentEObject.eGet(classFeature) instanceof EEnumLiteral classEnum) {
				if (classEnum.toString().equals(LogicalOperator.NULL.getName())) {//Operator Null: LogicalConstraint only used as wrapper for the constraint without added information (using a common super-datatype to make both eligible as value does not work with papyrus)
					if (getValue(currentEObject, odrlPackage.getLogicalConstraint_Constraints().getName()) instanceof List constraintList) {
						List<Constraint> constraints = new ConstraintList();
						constraints.addAll(addElement(constraintList, odrlParent, activityElement, Constraint.class));
						return constraints;//TODO watch out in with doubled parent-assignment.
					}//may need to be returned directly and not just assigned so that the fill-method is not called twice (in this method at the end and in the one called with the constraintList). Alternatively: alreadyProcessed-Boolean or something like that, that prevents adding parents and calling the fill()-method (should not prevent adding to the referenceList (as the called methods add with another key))
				} else {
					newObject=new LogicalConstraint();
				}
			}
		}
		return newObject;
	}
	
	public  Object addElement(EObject currentEObject, ODRLClass odrlParent, EObject activityElement) {
		Object newObject = null;
		newObject = getOdrlObject(currentEObject, odrlParent, activityElement);
		if (newObject ==null) {
			newObject = specialCases(currentEObject, odrlParent, activityElement);
		}
		return newObject;
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
	
	private String addElement(String currentObject, ODRLClass odrlParent, EObject activityElement) {
		return currentObject;
	}
	
	public  Object getValue(EObject eObject, String featureName) {
//		EStructuralFeature feature = eObject.eClass().getEStructuralFeature(featureName);
//		if(feature == null) {
//			//TODO add missing feature information?
//		}
//		return feature==null ? null : eObject.eGet(feature);
		return eObject.eGet(eObject.eClass().getEStructuralFeature(featureName));//Nullpointer-exception with null-feature can only be produced by code errors, not by input errors
	}
	
	public  ODRLClass getOdrlObject(EObject eObject, ODRLClass odrlParent, EObject activityElement) {
		Class<? extends ODRLClass> odrlClass = null;
		if (eObject instanceof EEnumLiteral enumLiteral) {
			Map<String, Class<? extends ODRLClass>> literalMap = enumMap.get(enumLiteral.getEEnum().getName());
			if (literalMap!= null) {
				odrlClass = literalMap.get(enumLiteral.getName());
			}
		} else {
			String className = eObject.eClass().getName();
			odrlClass = classMap.get(className);
			if (odrlClass==null) {
				String typeFeatureString = typeEnumMap1.get(className);
				EStructuralFeature typeFeature = eObject.eClass().getEStructuralFeature(typeFeatureString);
				if (typeFeature != null) {
					Object typeValue = eObject.eGet(typeFeature);
					if (typeValue instanceof EEnumLiteral enumLiteral) {
						Map<String, Class<? extends ODRLClass>> typeEnums = typeEnumMap2.get(className);
						if (typeEnums != null) {
							odrlClass = typeEnums.get(enumLiteral.getName());
						}
					}
				}
			}
		}
		if (odrlClass!=null && ODRLClass.class.isAssignableFrom(odrlClass)) {
			Class<ODRLClass> newClass = (Class<ODRLClass>) odrlClass;
			try {
			ODRLClass newObject = newClass.getConstructor().newInstance();
			return newObject;
			} catch(Exception e) {
				e.printStackTrace();
			}
		}
		return null;
	}
}
