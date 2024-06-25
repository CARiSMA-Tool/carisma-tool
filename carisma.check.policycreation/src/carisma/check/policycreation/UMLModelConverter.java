package carisma.check.policycreation;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.ENamedElement;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.InputPin;
import org.eclipse.uml2.uml.OutputPin;

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

public class UMLModelConverter {
	private final Map<String,Map<String,Class<? extends ODRLClass>>> enumMap = new HashMap<>();
	private final Map<String,String> typeEnumMap1 = new HashMap<>();
	private final Map<String,Map<String,Class<? extends ODRLClass>>> typeEnumMap2 = new HashMap<>();
	private final Map<String,Class<? extends ODRLClass>> classMap = new HashMap<>();
	private final Map<StringTuple,Class<? extends ODRLClass>> featureMap = new HashMap<>();
	private final ODRLCommonVocabularyPackage odrlPackage = ODRLCommonVocabularyPackage.eINSTANCE;
	
	private Map<EObject,ODRLClass> referencingMap = new HashMap<>();//Currently: Save top-level elements (stereotype applications) as they may be referred by several objects, others may not. (If more Elements should be accessed: Save with unique EObject, watch out for uniqueness of enums (may need to be saved as triple)
	//Also save lists, not just their elements
	
	private class StringTuple {
		public StringTuple(String owner, String feature) {
			this.owner=owner;
			this.feature=feature;
		}
		String owner;
		String feature;
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + getEnclosingInstance().hashCode();
			result = prime * result + Objects.hash(feature, owner);
			return result;
		}
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			StringTuple other = (StringTuple) obj;
			if (!getEnclosingInstance().equals(other.getEnclosingInstance()))
				return false;
			return Objects.equals(feature, other.feature) && Objects.equals(owner, other.owner);
		}
		private UMLModelConverter getEnclosingInstance() {
			return UMLModelConverter.this;
		}
		
	}
	
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
		featureMap.putAll(Map.ofEntries(
				Map.entry(new StringTuple(odrlPackage.getProhibition().getName(),odrlPackage.getProhibition_Remedies().getName()), Remedy.class),
				Map.entry(new StringTuple(odrlPackage.getDuty().getName(),odrlPackage.getDuty_Consequences().getName()), Consequence.class)
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
		if (currentEObject!=null) {
			System.out.println("Qualified name of " + currentEObject + ": " + EcoreUtil.getIdentification(currentEObject));//TODO remove
		}
		fill(currentEObject, newObject, activityElement);
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
	public Object getValue(EObject eObject, EStructuralFeature feature) {
		return eObject.eGet(eObject.eClass().getEStructuralFeature(feature.getName()));
	}
	
	public  ODRLClass getOdrlObject(EObject eObject, ODRLClass odrlParent, EObject activityElement) {
		if (referencingMap.get(eObject)!=null) {
			return referencingMap.get(eObject);
		}
		System.out.println("EObject in getOdrlObjects: " + eObject);
		Class<? extends ODRLClass> odrlClass = null;
		if (eObject instanceof EEnumLiteral enumLiteral) {
			Map<String, Class<? extends ODRLClass>> literalMap = enumMap.get(enumLiteral.getEEnum().getName());
			if (literalMap!= null) {
				odrlClass = literalMap.get(enumLiteral.getName());
			}
		} else if (eObject instanceof EStructuralFeature eFeature) {
			String featureName = eFeature.getName();
			String owningClassName = eFeature.getEContainingClass().getName();
			StringTuple tuple = new StringTuple(owningClassName,featureName);
			odrlClass = featureMap.get(tuple);
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
	
	
	private void fill(EObject currentEObject, Object toBeFilled, EObject activityElement) {//TODO change to switch-case with Class name or implement as functions of the ODRLClasses (both cases would need to call the function of their "superclass)
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
//		private void fillOperand(EObject currentEObject, Operand operand, EObject activityElement) {
//		}
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
			//Activity Diagram: Get contained rules from the contained actions
			if (getValue(currentEObject, odrlPackage.getODRLPolicy_Base_Activity()) instanceof Activity baseActivity) {
				for (ActivityNode node : baseActivity.getNodes()) {
					if (node instanceof org.eclipse.uml2.uml.Action action) {
						for (EObject stereoAppl : action.getStereotypeApplications()) {
							Object newObject = addElement(stereoAppl, policy, action);
							if (newObject instanceof Permission permission) {
								policy.addPermission(permission);
							} else if (newObject instanceof Prohibition prohibition) {
								policy.addProhibition(prohibition);
							} else if (newObject instanceof Duty obligation) {
								policy.addObligation(obligation);
							}
						}
					}
				}
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
			//Activity diagram: Get related Assets from neighboring pins (TODO: clear up conflicts with explicitly listed Relations?)
			if (activityElement instanceof org.eclipse.uml2.uml.Action action) {
				for (InputPin inPin : action.getInputs()) {
					for (EObject stereoAppl : inPin.getStereotypeApplications()) {
						if (addElement(stereoAppl, rule, action) instanceof Asset asset) {
							Relation newTarget = new Target();
							newTarget.setAsset(asset);
							rule.addInvolvedAssets(newTarget);
						}
					}
				}
				for (OutputPin outPin : action.getOutputs()) {
					for (EObject stereoAppl : outPin.getStereotypeApplications()) {
						if (addElement(stereoAppl, rule, action) instanceof Asset asset) {
							Relation newTarget = new Output();
							newTarget.setAsset(asset);
							rule.addInvolvedAssets(newTarget);
						}
					}
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
				System.out.println("attributeValue remedy: " + attributeValueOdrl);
				if (attributeValueOdrl instanceof Remedy remedy) {
					prohibition.setRemedy(remedy);
				}
			}//TODO only set the remedy if its rules-Property is not empty (in ecore it has the empty list, making the remedy and List non-null in any case). Possibility: fillRemedies
			Object attributeValue = getValue(currentEObject,odrlPackage.getProhibition_Remedies().getName());
			if (attributeValue instanceof List list) { //TODO List attribute
				List<Duty> attributeValueOdrl = addElement(list, prohibition.getRemedy(), activityElement, Duty.class);
				if (attributeValueOdrl!=null) {
					if (prohibition.getRemedy().getRules()==null)
						prohibition.getRemedy().setRules(new LinkedList<>());
					prohibition.getRemedy().getRules().addAll(attributeValueOdrl);//TODO change getters to conditional generators or add null-checks with additional creation everywhere were gotten objects are further used
				}
			}
		}
		private void fillDuty(EObject currentEObject, Duty duty, EObject activityElement) {
			EStructuralFeature consequenceFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getDuty_Consequences().getName());
			if (getValue(currentEObject,odrlPackage.getDuty_Consequences().getName()) != null) {
				System.out.println("ConsequenceValue" + getValue(currentEObject,odrlPackage.getDuty_Consequences().getName()));
				Object attributeValueOdrl = addElement(consequenceFeature, duty, activityElement);
				if (attributeValueOdrl instanceof Consequence consequence) {
					duty.setConsequences(consequence);
				}
			}
			//Following part currently leads to stack overflow when JSON-Objects are created (Does not anymore. did with old references)
			Object attributeValue = getValue(currentEObject, odrlPackage.getDuty_Consequences().getName());
			if (attributeValue instanceof List list) { //TODO List attribute
				List<Duty> attributeValueOdrl = addElement(list, duty.getConsequences(), activityElement, Duty.class);
				if (attributeValueOdrl!=null) {
					if (duty.getConsequences().getRules()==null)
						duty.getConsequences().setRules(new LinkedList<Rule>());
					duty.getConsequences().getRules().addAll(attributeValueOdrl);//TODO change getters to conditional generators or add null-checks with additional creation everywhere were gotten objects are further used
				}
			}
		}
	
		
}
