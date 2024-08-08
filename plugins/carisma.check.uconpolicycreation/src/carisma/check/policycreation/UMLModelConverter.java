package carisma.check.policycreation;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.uml2.uml.Element;

import ODRLCommonVocabulary.Action;
import ODRLCommonVocabulary.ConflictStrategy;
import ODRLCommonVocabulary.ConstraintOperator;
import ODRLCommonVocabulary.LeftOperand;
import ODRLCommonVocabulary.LogicalOperator;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.PartyFunctionType;
import ODRLCommonVocabulary.PolicyType;
import carisma.check.policycreation.profileimpl.common.action.AcceptTrackingImpl;
import carisma.check.policycreation.profileimpl.common.action.AggregateImpl;
import carisma.check.policycreation.profileimpl.common.action.AnnotateImpl;
import carisma.check.policycreation.profileimpl.common.action.AnonymizeImpl;
import carisma.check.policycreation.profileimpl.common.action.ArchiveImpl;
import carisma.check.policycreation.profileimpl.common.action.AttributeImpl;
import carisma.check.policycreation.profileimpl.common.action.AttributionImpl;
import carisma.check.policycreation.profileimpl.common.action.CommercialUseImpl;
import carisma.check.policycreation.profileimpl.common.action.CompensateImpl;
import carisma.check.policycreation.profileimpl.common.action.ConcurrentUseImpl;
import carisma.check.policycreation.profileimpl.common.action.DeleteImpl;
import carisma.check.policycreation.profileimpl.common.action.DerivativeWorksImpl;
import carisma.check.policycreation.profileimpl.common.action.DeriveImpl;
import carisma.check.policycreation.profileimpl.common.action.DigitizeImpl;
import carisma.check.policycreation.profileimpl.common.action.DisplayImpl;
import carisma.check.policycreation.profileimpl.common.action.DistributeImpl;
import carisma.check.policycreation.profileimpl.common.action.DistributionImpl;
import carisma.check.policycreation.profileimpl.common.action.EnsureExclusivityImpl;
import carisma.check.policycreation.profileimpl.common.action.ExecuteImpl;
import carisma.check.policycreation.profileimpl.common.action.ExtractImpl;
import carisma.check.policycreation.profileimpl.common.action.GiveImpl;
import carisma.check.policycreation.profileimpl.common.action.GrantUseImpl;
import carisma.check.policycreation.profileimpl.common.action.IncludeImpl;
import carisma.check.policycreation.profileimpl.common.action.IndexImpl;
import carisma.check.policycreation.profileimpl.common.action.InformImpl;
import carisma.check.policycreation.profileimpl.common.action.InstallImpl;
import carisma.check.policycreation.profileimpl.common.action.ModifyImpl;
import carisma.check.policycreation.profileimpl.common.action.MoveImpl;
import carisma.check.policycreation.profileimpl.common.action.NextPolicyImpl;
import carisma.check.policycreation.profileimpl.common.action.NoticeImpl;
import carisma.check.policycreation.profileimpl.common.action.ObtainConsentImpl;
import carisma.check.policycreation.profileimpl.common.action.PlayImpl;
import carisma.check.policycreation.profileimpl.common.action.PresentImpl;
import carisma.check.policycreation.profileimpl.common.action.PrintImpl;
import carisma.check.policycreation.profileimpl.common.action.ReadImpl;
import carisma.check.policycreation.profileimpl.common.action.ReproduceImpl;
import carisma.check.policycreation.profileimpl.common.action.ReproductionImpl;
import carisma.check.policycreation.profileimpl.common.action.ReviewPolicyImpl;
import carisma.check.policycreation.profileimpl.common.action.SellImpl;
import carisma.check.policycreation.profileimpl.common.action.ShareAlikeImpl;
import carisma.check.policycreation.profileimpl.common.action.SharingImpl;
import carisma.check.policycreation.profileimpl.common.action.SourceCodeImpl;
import carisma.check.policycreation.profileimpl.common.action.StreamImpl;
import carisma.check.policycreation.profileimpl.common.action.SynchronizeImpl;
import carisma.check.policycreation.profileimpl.common.action.TextToSpeechImpl;
import carisma.check.policycreation.profileimpl.common.action.TransformImpl;
import carisma.check.policycreation.profileimpl.common.action.TranslateImpl;
import carisma.check.policycreation.profileimpl.common.action.UninstallImpl;
import carisma.check.policycreation.profileimpl.common.action.WatermarkImpl;
import carisma.check.policycreation.profileimpl.common.function.AttributedPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.AttributingPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.CompensatedPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.CompensatingPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.ConsentedPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.ConsentingPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.ContractedPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.ContractingPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.InformedPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.InformingPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.TrackedPartyImpl;
import carisma.check.policycreation.profileimpl.common.function.TrackingPartyImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.AbsoluteAssetPositionImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.AbsoluteAssetSizeImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.AbsoluteSpatialAssetPositionImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.AbsoluteTemporalAssetPositionImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.AssetPercentageImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.CountImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.DateTimeImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.DelayPeriodImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.DeliveryChannelImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.ElapsedTimeImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.EventImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.FileFormatImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.GeospatialCoordinatesImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.GeospatialNamedAreaImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.IndustryContextImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.LanguageImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.MediaContextImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.MeteredTimeImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.PaymentAmountImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.ProductContextImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.PurposeImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.RecipientImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.RecurringTimeIntervalImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.RelativeAssetPositionImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.RelativeAssetSizeImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.RelativeSpatialAssetPositionImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.RelativeTemporalAssetPositionImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.RenditionResolutionImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.SystemDeviceImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.UnitOfCountImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.VersionImpl;
import carisma.check.policycreation.profileimpl.common.leftoperand.VirtualItCommunicationLocationImpl;
import carisma.check.policycreation.profileimpl.common.policy.AssertionImpl;
import carisma.check.policycreation.profileimpl.common.policy.PrivacyImpl;
import carisma.check.policycreation.profileimpl.common.policy.RequestImpl;
import carisma.check.policycreation.profileimpl.common.policy.TicketImpl;
import carisma.check.policycreation.profileimpl.common.relation.OutputImpl;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.action.ActionImpl;
import carisma.check.policycreation.profileimpl.core.action.TransferOwnershipImpl;
import carisma.check.policycreation.profileimpl.core.action.UseImpl;
import carisma.check.policycreation.profileimpl.core.asset.AssetCollectionImpl;
import carisma.check.policycreation.profileimpl.core.asset.AssetImpl;
import carisma.check.policycreation.profileimpl.core.conflict.ConflictStrategyImpl;
import carisma.check.policycreation.profileimpl.core.conflict.PermitImpl;
import carisma.check.policycreation.profileimpl.core.conflict.ProhibitImpl;
import carisma.check.policycreation.profileimpl.core.conflict.VoidPolicyImpl;
import carisma.check.policycreation.profileimpl.core.constraint.ConstraintImpl;
import carisma.check.policycreation.profileimpl.core.constraint.ConstraintListImpl;
import carisma.check.policycreation.profileimpl.core.constraint.LogicalConstraintImpl;
import carisma.check.policycreation.profileimpl.core.failure.ConsequenceImpl;
import carisma.check.policycreation.profileimpl.core.failure.RemedyImpl;
import carisma.check.policycreation.profileimpl.core.function.AssigneeImpl;
import carisma.check.policycreation.profileimpl.core.function.AssignerImpl;
import carisma.check.policycreation.profileimpl.core.operand.AndImpl;
import carisma.check.policycreation.profileimpl.core.operand.AndSequenceImpl;
import carisma.check.policycreation.profileimpl.core.operand.OperandImpl;
import carisma.check.policycreation.profileimpl.core.operand.OrImpl;
import carisma.check.policycreation.profileimpl.core.operand.XoneImpl;
import carisma.check.policycreation.profileimpl.core.operator.EqualToImpl;
import carisma.check.policycreation.profileimpl.core.operator.GreaterEqImpl;
import carisma.check.policycreation.profileimpl.core.operator.GreaterThanImpl;
import carisma.check.policycreation.profileimpl.core.operator.HasPartImpl;
import carisma.check.policycreation.profileimpl.core.operator.IsAImpl;
import carisma.check.policycreation.profileimpl.core.operator.IsAllOfImpl;
import carisma.check.policycreation.profileimpl.core.operator.IsAnyOfImpl;
import carisma.check.policycreation.profileimpl.core.operator.IsNoneOfImpl;
import carisma.check.policycreation.profileimpl.core.operator.IsPartOfImpl;
import carisma.check.policycreation.profileimpl.core.operator.LessThanEqImpl;
import carisma.check.policycreation.profileimpl.core.operator.LessThanImpl;
import carisma.check.policycreation.profileimpl.core.operator.NotEqualToImpl;
import carisma.check.policycreation.profileimpl.core.party.PartyCollectionImpl;
import carisma.check.policycreation.profileimpl.core.party.PartyImpl;
import carisma.check.policycreation.profileimpl.core.policy.AgreementImpl;
import carisma.check.policycreation.profileimpl.core.policy.OfferImpl;
import carisma.check.policycreation.profileimpl.core.policy.PolicyImpl;
import carisma.check.policycreation.profileimpl.core.policy.SetImpl;
import carisma.check.policycreation.profileimpl.core.relation.RelationImpl;
import carisma.check.policycreation.profileimpl.core.relation.TargetImpl;
import carisma.check.policycreation.profileimpl.core.rule.DutyImpl;
import carisma.check.policycreation.profileimpl.core.rule.PermissionImpl;
import carisma.check.policycreation.profileimpl.core.rule.ProhibitionImpl;
import carisma.check.policycreation.profileimpl.core.rule.RuleImpl;

public class UMLModelConverter {
	/**
	 * Map of {@link EEnum}s and their {@link EEnumLiteral}s to the {@link ODRLClass} they represent, by name.
	 */
	private final Map<String,Map<String,Class<? extends ODRLClassImpl>>> enumMap = new HashMap<>();
	/**
	 * Map of {@link EClass}es to the {@link EStructuralFeature} containing their ODRL-type-defining {@link EEnumLiteral}, by name.
	 */
	private final Map<String,String> typeEnumMap1 = new HashMap<>();
	/**
	 * Map of {@link EClass}es and the {@link EEnumLiteral} defining their ODRL-type to the {@link ODRLClass} they represent, by name.
	 */
	private final Map<String,Map<String,Class<? extends ODRLClassImpl>>> typeEnumMap2 = new HashMap<>();//TODO: potential problem: if several Enumerations with same-name Literals are valid as value of the structural feature their literals are not distinguishable with the current approach
	/**
	 * Map of {@link EClass}es to the {@link ODRLClass} they represent, by name.
	 */
	private final Map<String,Class<? extends ODRLClassImpl>> classMap = new HashMap<>();
	/**
	 * Map of {@link EClass}es and their {@link EStructuralFeature}s (wrapped in a {@link StringTuple}) to the {@link ODRLClass} they represent, by name.
	 */
	private final Map<StringTuple,Class<? extends ODRLClassImpl>> featureMap = new HashMap<>();//Mapping of EStructuralFeatures to the ODRLClass-Objects they represent
	
	public static final String TYPE_STRING = "@type";
	
	public static final String JSONLD_TYPE_STRING = "@type";
	
	/**
	 * Map of {@link EClass}es and their {@link EStructuralFeature}s to the terms used in their JSON-LD-representation.
	 */
	private final Map<Object,String> termMap = new HashMap<>(); //Instead do mapping in the map-conversion-methods of the odrl-classes?
	/**
	 * Package generated from the used profile.
	 */
	public static final ODRLCommonVocabularyPackage odrlPackage = ODRLCommonVocabularyPackage.eINSTANCE;
	
	private Map<EObject,ODRLClassImpl> referencingMap = new HashMap<>();//Currently: Save top-level elements (stereotype applications) as they may be referred by several objects, others may not. (If more Elements should be accessed: Save with unique EObject, watch out for uniqueness of enums (may need to be saved as triple)
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
				Map.entry(ConflictStrategy.PERMIT.getName(),PermitImpl.class),
				Map.entry(ConflictStrategy.PROHIBIT.getName(), ProhibitImpl.class),
				Map.entry(ConflictStrategy.VOID_POLICY.getName(), VoidPolicyImpl.class)				
				));
		enumMap.put(Action.class.getSimpleName(), Map.ofEntries(
				Map.entry(Action.ACCEPT_TRACKING.getName(), AcceptTrackingImpl.class),
				Map.entry(Action.AGGREGATE.getName(), AggregateImpl.class),
				Map.entry(Action.ANNOTATE.getName(), AnnotateImpl.class),
				Map.entry(Action.ANONYMIZE.getName(), AnonymizeImpl.class),
				Map.entry(Action.ARCHIVE.getName(), ArchiveImpl.class),
				Map.entry(Action.ATTRIBUTE.getName(), AttributeImpl.class),
				Map.entry(Action.CC_ATTRIBUTION.getName(), AttributionImpl.class),
				Map.entry(Action.CC_COMMERCIAL_USE.getName(), CommercialUseImpl.class),
				Map.entry(Action.CC_DERIVATIVE_WORKS.getName(), DerivativeWorksImpl.class),
				Map.entry(Action.CC_DISTRIBUTION.getName(), DistributionImpl.class),
				Map.entry(Action.CC_NOTICE.getName(), NoticeImpl.class),
				Map.entry(Action.CC_REPRODUCTION.getName(), ReproductionImpl.class),
				Map.entry(Action.CC_SHARE_ALIKE.getName(), ShareAlikeImpl.class),
				Map.entry(Action.CC_SHARING.getName(), SharingImpl.class),
				Map.entry(Action.CC_SOURCE_CODE.getName(), SourceCodeImpl.class),
				Map.entry(Action.COMPENSATE.getName(), CompensateImpl.class),
				Map.entry(Action.CONCURRENT_USE.getName(), ConcurrentUseImpl.class),
				Map.entry(Action.DELETE.getName(), DeleteImpl.class),
				Map.entry(Action.DERIVE.getName(), DeriveImpl.class),
				Map.entry(Action.DIGITIZE.getName(), DigitizeImpl.class),
				Map.entry(Action.DISPLAY.getName(), DisplayImpl.class),
				Map.entry(Action.DISTRIBUTE.getName(), DistributeImpl.class),
				Map.entry(Action.ENSURE_EXCLUSIVITY.getName(), EnsureExclusivityImpl.class),
				Map.entry(Action.EXECUTE.getName(), ExecuteImpl.class),
				Map.entry(Action.EXTRACT.getName(), ExtractImpl.class),
				Map.entry(Action.GIVE.getName(), GiveImpl.class),
				Map.entry(Action.GRANT_USE.getName(), GrantUseImpl.class),
				Map.entry(Action.INCLUDE.getName(), IncludeImpl.class),
				Map.entry(Action.INDEX.getName(), IndexImpl.class),
				Map.entry(Action.INFORM.getName(), InformImpl.class),
				Map.entry(Action.INSTALL.getName(), InstallImpl.class),
				Map.entry(Action.MODIFY.getName(), ModifyImpl.class),
				Map.entry(Action.MOVE.getName(), MoveImpl.class),
				Map.entry(Action.NEXT_POLICY.getName(), NextPolicyImpl.class),
				Map.entry(Action.OBTAIN_CONSENT.getName(), ObtainConsentImpl.class),
				Map.entry(Action.PLAY.getName(), PlayImpl.class),
				Map.entry(Action.PRESENT.getName(), PresentImpl.class),
				Map.entry(Action.PRINT.getName(), PrintImpl.class),
				Map.entry(Action.READ.getName(), ReadImpl.class),
				Map.entry(Action.REPRODUCE.getName(), ReproduceImpl.class),
				Map.entry(Action.REVIEW_POLICY.getName(), ReviewPolicyImpl.class),
				Map.entry(Action.SELL.getName(), SellImpl.class),
				Map.entry(Action.STREAM.getName(), StreamImpl.class),
				Map.entry(Action.SYNCHRONIZE.getName(), SynchronizeImpl.class),
				Map.entry(Action.TEXT_TO_SPEECH.getName(), TextToSpeechImpl.class),
				Map.entry(Action.TRANSFER.getName(), TransferOwnershipImpl.class),
				Map.entry(Action.TRANSFORM.getName(), TransformImpl.class),
				Map.entry(Action.TRANSLATE.getName(), TranslateImpl.class),
				Map.entry(Action.UNINSTALL.getName(), UninstallImpl.class),
				Map.entry(Action.USE.getName(), UseImpl.class),
				Map.entry(Action.WATERMARK.getName(), WatermarkImpl.class)
				));
		enumMap.put(ConstraintOperator.class.getSimpleName(), Map.ofEntries(
				Map.entry(ConstraintOperator.EQ.getName(), EqualToImpl.class),
				Map.entry(ConstraintOperator.GTEQ.getName(), GreaterEqImpl.class),
				Map.entry(ConstraintOperator.GT.getName(), GreaterThanImpl.class),
				Map.entry(ConstraintOperator.HAS_PART.getName(), HasPartImpl.class),
				Map.entry(ConstraintOperator.IS_A.getName(), IsAImpl.class),
				Map.entry(ConstraintOperator.IS_ALL_OF.getName(), IsAllOfImpl.class),
				Map.entry(ConstraintOperator.IS_ANY_OF.getName(), IsAnyOfImpl.class),
				Map.entry(ConstraintOperator.IS_NONE_OF.getName(), IsNoneOfImpl.class),
				Map.entry(ConstraintOperator.IS_PART_OF.getName(), IsPartOfImpl.class),
				Map.entry(ConstraintOperator.LT.getName(), LessThanImpl.class),
				Map.entry(ConstraintOperator.LTEQ.getName(), LessThanEqImpl.class),
				Map.entry(ConstraintOperator.NEQ.getName(), NotEqualToImpl.class)
				));
		enumMap.put(LogicalOperator.class.getSimpleName(),Map.ofEntries(
				Map.entry(LogicalOperator.AND.getName(), AndImpl.class),
				Map.entry(LogicalOperator.AND_SEQUENCE.getName(), AndSequenceImpl.class),
				Map.entry(LogicalOperator.OR.getName(), OrImpl.class),
				Map.entry(LogicalOperator.XONE.getName(), XoneImpl.class)
				//The Null-Case and its special implications (Constraint instead of LogicalConstraint) are handled in the LogicalConstraint-case
				));
		enumMap.put(LeftOperand.class.getSimpleName(), Map.ofEntries(
				//TODO absolute position seems to be missing in UMl-Model
				Map.entry(LeftOperand.ABSOLUTE_SIZE.getName(), AbsoluteAssetSizeImpl.class),
				Map.entry(LeftOperand.ABSOLUTE_SPATIAL_POSITION.getName(), AbsoluteSpatialAssetPositionImpl.class),//TODO correct spelling from spartial to spatial
				Map.entry(LeftOperand.ABSOLUTE_TEMPORAL_POSITION.getName(), AbsoluteTemporalAssetPositionImpl.class),
				Map.entry(LeftOperand.PERCENTAGE.getName(), AssetPercentageImpl.class),
				Map.entry(LeftOperand.COUNT.getName(), CountImpl.class),
				Map.entry(LeftOperand.DATE_TIME.getName(), DateTimeImpl.class),
				Map.entry(LeftOperand.DELAY_PERIOD.getName(), DelayPeriodImpl.class),
				Map.entry(LeftOperand.DELIVERY_CHANNEL.getName(), DeliveryChannelImpl.class),
				Map.entry(LeftOperand.ELAPSED_TIME.getName(), ElapsedTimeImpl.class),
				Map.entry(LeftOperand.EVENT.getName(), EventImpl.class),
				Map.entry(LeftOperand.FILE_FORMAT.getName(), FileFormatImpl.class),
				Map.entry(LeftOperand.SPATIAL_COORDINATES.getName(), GeospatialCoordinatesImpl.class),//TODO correct spelling from spartial to spatial
				Map.entry(LeftOperand.SPATIAL.getName(), GeospatialNamedAreaImpl.class),//TODO correct spelling from spartial to spatial
				Map.entry(LeftOperand.INDUSTRY.getName(), IndustryContextImpl.class),
				Map.entry(LeftOperand.LANGUAGE.getName(), LanguageImpl.class),
				Map.entry(LeftOperand.MEDIA.getName(), MediaContextImpl.class),
				Map.entry(LeftOperand.METERED_TIME.getName(), MeteredTimeImpl.class),
				Map.entry(LeftOperand.PAY_AMOUNT.getName(), PaymentAmountImpl.class),
				Map.entry(LeftOperand.PRODUCT.getName(), ProductContextImpl.class),
				Map.entry(LeftOperand.PURPOSE.getName(), PurposeImpl.class),
				Map.entry(LeftOperand.RECIPIENT.getName(), RecipientImpl.class),
				Map.entry(LeftOperand.TIME_INTERVAL.getName(), RecurringTimeIntervalImpl.class),
				Map.entry(LeftOperand.RELATIVE_POSITION.getName(), RelativeAssetPositionImpl.class),
				Map.entry(LeftOperand.RELATIVE_SIZE.getName(), RelativeAssetSizeImpl.class),
				Map.entry(LeftOperand.RELATIVE_SPATIAL_POSITION.getName(), RelativeSpatialAssetPositionImpl.class),//TODO correct spelling from spartial to spatial
				Map.entry(LeftOperand.RELATIVE_TEMPORAL_POSITION.getName(), RelativeTemporalAssetPositionImpl.class),
				Map.entry(LeftOperand.RESOLUTION.getName(), RenditionResolutionImpl.class),
				Map.entry(LeftOperand.DEVICE.getName(), SystemDeviceImpl.class),
				Map.entry(LeftOperand.UNIT_OF_COUNT.getName(), UnitOfCountImpl.class),
				Map.entry(LeftOperand.VERSION.getName(), VersionImpl.class),
				Map.entry(LeftOperand.VIRTUAL_LOCATION.getName(), VirtualItCommunicationLocationImpl.class)
				));
		typeEnumMap1.put(odrlPackage.getODRLPolicy().getName(), odrlPackage.getODRLPolicy_PolicyType().getName());
		typeEnumMap2.put(odrlPackage.getODRLPolicy().getName(), Map.ofEntries(
				Map.entry(PolicyType.AGREEMENT.getName(), AgreementImpl.class),
				Map.entry(PolicyType.ASSERTION.getName(), AssertionImpl.class),
				Map.entry(PolicyType.OFFER.getName(), OfferImpl.class),
				Map.entry(PolicyType.PRIVACY.getName(), PrivacyImpl.class),
				Map.entry(PolicyType.REQUEST.getName(), RequestImpl.class),
				Map.entry(PolicyType.SET.getName(), SetImpl.class),
				Map.entry(PolicyType.TICKET.getName(), TicketImpl.class),
				Map.entry(PolicyType.NULL.getName(), PolicyImpl.class)//No type-information (is interpreted as Set-Policy by evaluators)
				));
		//Explicit Relations were removed from the diagram
//		typeEnumMap1.put(odrlPackage.getAssetRelation().getName(), odrlPackage.getAssetRelation_Type().getName());
//		typeEnumMap2.put(odrlPackage.getAssetRelation().getName(), Map.ofEntries(
//				Map.entry(AssetRelationType.TARGET.getName(), TargetImpl.class),
//				Map.entry(AssetRelationType.OUTPUT.getName(), OutputImpl.class)
//				));
		typeEnumMap1.put(odrlPackage.getPartyFunction().getName(), odrlPackage.getPartyFunction_Type().getName());
		typeEnumMap2.put(odrlPackage.getPartyFunction().getName(), Map.ofEntries(
				Map.entry(PartyFunctionType.ASSIGNEE.getName(), AssigneeImpl.class),
				Map.entry(PartyFunctionType.ASSIGNER.getName(), AssignerImpl.class),
				Map.entry(PartyFunctionType.ATTRIBUTED_PARTY.getName(), AttributedPartyImpl.class),
				Map.entry(PartyFunctionType.COMPENSATED_PARTY.getName(), CompensatedPartyImpl.class),
				Map.entry(PartyFunctionType.COMPENSATING_PARTY.getName(), CompensatingPartyImpl.class),
				Map.entry(PartyFunctionType.CONSENTED_PARTY.getName(), ConsentedPartyImpl.class),
				Map.entry(PartyFunctionType.CONSENTING_PARTY.getName(), ConsentingPartyImpl.class),
				Map.entry(PartyFunctionType.CONTRACTED_PARTY.getName(), ContractedPartyImpl.class),
				Map.entry(PartyFunctionType.CONTRACTING_PARTY.getName(), ContractingPartyImpl.class),
				Map.entry(PartyFunctionType.INFORMED_PARTY.getName(), InformingPartyImpl.class),
				Map.entry(PartyFunctionType.TRACKED_PARTY.getName(), TrackedPartyImpl.class),
				Map.entry(PartyFunctionType.TRACKING_PARTY.getName(), TrackingPartyImpl.class)
				));
		classMap.putAll(Map.ofEntries(
				Map.entry(odrlPackage.getPermission().getName(), PermissionImpl.class),
				Map.entry(odrlPackage.getProhibition().getName(), ProhibitionImpl.class),
				Map.entry(odrlPackage.getDuty().getName(), DutyImpl.class)
				));
		classMap.put(odrlPackage.getConstraint().getName(), ConstraintImpl.class);
		classMap.putAll(Map.ofEntries(
				Map.entry(odrlPackage.getAsset().getName(), AssetImpl.class),
				Map.entry(odrlPackage.getAssetCollection().getName(), AssetCollectionImpl.class)
				));
		classMap.putAll(Map.ofEntries(
				Map.entry(odrlPackage.getParty().getName(), PartyImpl.class),
				Map.entry(odrlPackage.getPartyCollection().getName(), PartyCollectionImpl.class)
				));
		featureMap.putAll(Map.ofEntries(
				Map.entry(new StringTuple(odrlPackage.getProhibition().getName(),odrlPackage.getProhibition_Remedies().getName()), RemedyImpl.class),
				Map.entry(new StringTuple(odrlPackage.getDuty().getName(),odrlPackage.getDuty_Consequences().getName()), ConsequenceImpl.class)
				));
		
		//Missing: LogicalConstraint (is in specialCases)
		
		
		try {
			//ODRL-Core
			//Action
			termMap.put(ActionImpl.class, "Action");//Currently abstract
			termMap.put(ActionImpl.class.getDeclaredField("refinement"), "refinement");
			termMap.put(TransferOwnershipImpl.class, "transfer");
			termMap.put(UseImpl.class, "use");
			//Asset
			termMap.put(AssetImpl.class, "Asset");
			termMap.put(AssetImpl.class.getDeclaredField("uid"), "uid");
			termMap.put(AssetCollectionImpl.class, "AssetCollection");
			termMap.put(AssetCollectionImpl.class.getDeclaredField("source"), "source");
			termMap.put(AssetCollectionImpl.class.getDeclaredField("refinement"), "refinement");
			//Conflict
			termMap.put(ConflictStrategyImpl.class, "ConflictTerm");//Currently abstract
			termMap.put(PermitImpl.class, "perm");
			termMap.put(ProhibitionImpl.class, "prohibit");
			termMap.put(VoidPolicyImpl.class, "invalid");
			//Constraint
			termMap.put(ConstraintImpl.class, "Constraint");
			termMap.put(ConstraintImpl.class.getDeclaredField("uid"), "uid");
			termMap.put(ConstraintImpl.class.getDeclaredField("leftOperand"), "leftOperand");
			termMap.put(ConstraintImpl.class.getDeclaredField("operator"), "operator");
			termMap.put(ConstraintImpl.class.getDeclaredField("rightOperand"), "rightOperand");
			termMap.put(ConstraintImpl.class.getDeclaredField("rightOperandReference"), "rightOperandReference");
			termMap.put(ConstraintImpl.class.getDeclaredField("dataType"), "dataType");
			termMap.put(ConstraintImpl.class.getDeclaredField("unit"), "unit");
			termMap.put(ConstraintImpl.class.getDeclaredField("status"), "status");
			termMap.put(LogicalConstraintImpl.class, "LogicalConstraint");
			termMap.put(LogicalConstraintImpl.class.getDeclaredField("uid"), "uid");
			//termMap.put(LogicalConstraintImpl.class.getDeclaredField("operand"), "operand");//TODO Possibly remove as only subproperties of operand are used
			//Failure
			//termMap.put(FailureImpl.class, ""); //Is (Sub)-Property, does not exist as class in the model
			termMap.put(ConsequenceImpl.class, "consequence"); //Is (Sub)-Property, does not exist as class in the model
			termMap.put(RemedyImpl.class, "remedy"); //Is (Sub)-Property, does not exist as class in the model
			//Function
			//termMap.put(FunctionImpl.class, "function"); //Is (Sub)-Property, does not exist as class in the model
			termMap.put(AssigneeImpl.class, "assignee"); //Is (Sub)-Property, does not exist as class in the model
			termMap.put(AssignerImpl.class, "assigner"); //Is (Sub)-Property, does not exist as class in the model
			//LeftOperand
			termMap.put(LeftOperand.class, "LeftOperand");//Currently abstract
			//Operand
			//termMap.put(OperandImpl.class, "");
			termMap.put(OperandImpl.class.getDeclaredField("constraints"), "TODO:Remove operand from termMap");//TODO:Remove, just here for testing
			termMap.put(AndImpl.class, "and");
			termMap.put(AndSequenceImpl.class, "andSequence");
			termMap.put(OrImpl.class, "or");
			termMap.put(XoneImpl.class, "xone");
			//Operator
			//termMap.put(OperatorImpl.class, "");
			termMap.put(EqualToImpl.class, "eq");
			termMap.put(GreaterEqImpl.class, "gteq");
			termMap.put(GreaterThanImpl.class, "gt");
			termMap.put(HasPartImpl.class, "hasPart");
			termMap.put(IsAImpl.class, "isA");
			termMap.put(IsAllOfImpl.class, "isAllOf");
			termMap.put(IsAnyOfImpl.class, "isAnyOf");
			termMap.put(IsNoneOfImpl.class, "isNoneOf");
			termMap.put(IsPartOfImpl.class, "isPartOf");
			termMap.put(LessThanEqImpl.class, "lteq");
			termMap.put(LessThanImpl.class, "lt");
			termMap.put(NotEqualToImpl.class, "neq");
			//Party
			termMap.put(PartyImpl.class, "Party");
			termMap.put(PartyImpl.class.getDeclaredField("uid"), "uid");
			termMap.put(PartyCollectionImpl.class, "PartyCollection");
			termMap.put(PartyCollectionImpl.class.getDeclaredField("refinement"), "refinement");
			termMap.put(PartyCollectionImpl.class.getDeclaredField("source"), "source");
			//Policy
			termMap.put(PolicyImpl.class, "Policy");
			termMap.put(PolicyImpl.class.getDeclaredField("uid"), "uid");
			termMap.put(PolicyImpl.class.getDeclaredField("conflictStrategy"), "conflict");
			termMap.put(PolicyImpl.class.getDeclaredField("profiles"), "profile");
			termMap.put(PolicyImpl.class.getDeclaredField("inheritsFrom"), "inheritFrom");
			termMap.put(PolicyImpl.class.getDeclaredField("permission"), "permission");
			termMap.put(PolicyImpl.class.getDeclaredField("obligation"), "obligation");
			termMap.put(PolicyImpl.class.getDeclaredField("prohibition"), "prohibition");
			termMap.put(AgreementImpl.class, "Agreement");
			termMap.put(OfferImpl.class, "Offer");
			termMap.put(SetImpl.class, "Set");
			//Relation
			termMap.put(RelationImpl.class, "relation"); //Is (Sub)-Property, does not exist as class in the model
			termMap.put(TargetImpl.class, "target"); //Is (Sub)-Property, does not exist as class in the model
			//RightOperand//TODO
			//
			//Rule
			termMap.put(RuleImpl.class, "Rule");//Currently abstract
			termMap.put(RuleImpl.class.getDeclaredField("uid"), "uid");
			//termMap.put(Rule.class.getDeclaredField("involvedParties"), ""); //handled through value types
			//termMap.put(Rule.class.getDeclaredField("involvedAssets"), ""); //handled through value types
			termMap.put(RuleImpl.class.getDeclaredField("action"), "action");
			termMap.put(RuleImpl.class.getDeclaredField("constraint"), "constraint");
			termMap.put(DutyImpl.class, "Duty");
			termMap.put(DutyImpl.class.getDeclaredField("consequences"), "consequence");//TODO: maybe change to failure
			termMap.put(PermissionImpl.class, "Permission");
			termMap.put(PermissionImpl.class.getDeclaredField("duties"), "duty");
			termMap.put(ProhibitionImpl.class, "Prohibition");
			termMap.put(ProhibitionImpl.class.getDeclaredField("remedy"), "remedy");//TODO: maybe change to failure
			
			//ODRL-Common
			//Action
			termMap.put(AcceptTrackingImpl.class, "acceptTracking");
			termMap.put(AggregateImpl.class, "aggregate");
			termMap.put(AnnotateImpl.class, "annotate");
			termMap.put(AnonymizeImpl.class, "anonymize");
			termMap.put(ArchiveImpl.class, "archive");
			termMap.put(AttributeImpl.class, "attribute");
			termMap.put(AttributionImpl.class, "cc:Attribution");//TODO: maybe deal with creative commons terms differently (identify creative commons terms by the cc: in their ID currently)
			termMap.put(CommercialUseImpl.class, "cc:CommericalUse");
			termMap.put(CompensateImpl.class, "compensate");
			termMap.put(ConcurrentUseImpl.class, "concurrentUse");
			termMap.put(DeleteImpl.class, "delete");
			termMap.put(DerivativeWorksImpl.class, "cc:DerivativeWorks");
			termMap.put(DeriveImpl.class, "derive");
			termMap.put(DigitizeImpl.class, "digitize");
			termMap.put(DisplayImpl.class, "display");
			termMap.put(DistributeImpl.class, "distribute");
			termMap.put(DistributionImpl.class, "cc:Distribution");
			termMap.put(EnsureExclusivityImpl.class, "ensureExclusivity");
			termMap.put(ExecuteImpl.class, "execute");
			termMap.put(ExtractImpl.class, "extract");
			termMap.put(GiveImpl.class, "give");
			termMap.put(GrantUseImpl.class, "grantUse");
			termMap.put(IncludeImpl.class, "include");
			termMap.put(IndexImpl.class, "index");
			termMap.put(InformImpl.class, "inform");
			termMap.put(InstallImpl.class, "install");
			termMap.put(ModifyImpl.class, "modify");
			termMap.put(MoveImpl.class, "move");
			termMap.put(NextPolicyImpl.class, "nextPolicy");
			termMap.put(NoticeImpl.class, "cc:Notice");
			termMap.put(ObtainConsentImpl.class, "obtainConsent");
			termMap.put(PlayImpl.class, "play");
			termMap.put(PresentImpl.class, "present");
			termMap.put(PrintImpl.class, "print");
			termMap.put(ReadImpl.class, "read");
			termMap.put(ReproduceImpl.class, "reproduce");
			termMap.put(ReproductionImpl.class, "cc:Reproduction");
			termMap.put(ReviewPolicyImpl.class, "reviewPolicy");
			termMap.put(SellImpl.class, "sell");
			termMap.put(ShareAlikeImpl.class, "cc:ShareAlike");
			termMap.put(SharingImpl.class, "cc:Sharing");
			termMap.put(SourceCodeImpl.class, "cc:SourceCode");
			termMap.put(StreamImpl.class, "stream");
			termMap.put(SynchronizeImpl.class, "synchronize");
			termMap.put(TextToSpeechImpl.class, "textToSpeech");
			termMap.put(TransformImpl.class, "transform");
			termMap.put(TranslateImpl.class, "translate");
			termMap.put(UninstallImpl.class, "uninstall");
			termMap.put(WatermarkImpl.class, "watermark");
			
			//Function
			termMap.put(AttributedPartyImpl.class, "attributedParty");
			termMap.put(AttributingPartyImpl.class, "attributingParty");
			termMap.put(CompensatedPartyImpl.class, "compensatedParty");
			termMap.put(CompensatingPartyImpl.class, "compensatingParty");
			termMap.put(ConsentedPartyImpl.class, "consentedParty");
			termMap.put(ConsentingPartyImpl.class, "consentingParty");
			termMap.put(ContractedPartyImpl.class, "contractedParty");
			termMap.put(ContractingPartyImpl.class, "contractingParty");
			termMap.put(InformedPartyImpl.class, "informedParty");
			termMap.put(InformingPartyImpl.class, "informingParty");
			termMap.put(TrackedPartyImpl.class, "trackedParty");
			termMap.put(TrackingPartyImpl.class, "trackingParty");
			
			//LeftOperand
			termMap.put(AbsoluteAssetPositionImpl.class, "absolutePosition");
			termMap.put(AbsoluteAssetSizeImpl.class, "absoluteSize");
			termMap.put(AbsoluteSpatialAssetPositionImpl.class, "absoluteSpatialPosition");
			termMap.put(AbsoluteTemporalAssetPositionImpl.class, "absoluteTemporalPosition");
			termMap.put(AssetPercentageImpl.class, "percentage");
			termMap.put(CountImpl.class, "count");
			termMap.put(DateTimeImpl.class, "dateTime");
			termMap.put(DelayPeriodImpl.class, "delayPeriod");
			termMap.put(DeliveryChannelImpl.class, "deliveryChannel");
			termMap.put(ElapsedTimeImpl.class, "elapsedTime");
			termMap.put(EventImpl.class, "event");
			termMap.put(FileFormatImpl.class, "fileFormat");
			termMap.put(GeospatialCoordinatesImpl.class, "spatialCoordinates");
			termMap.put(GeospatialNamedAreaImpl.class, "spatial");
			termMap.put(IndustryContextImpl.class, "industry");
			termMap.put(LanguageImpl.class, "language");
			termMap.put(MediaContextImpl.class, "media");
			termMap.put(MeteredTimeImpl.class, "meteredTime");
			termMap.put(PaymentAmountImpl.class, "payAmount");
			termMap.put(ProductContextImpl.class, "product");
			termMap.put(PurposeImpl.class, "purpose");
			termMap.put(RecipientImpl.class, "recipient");
			termMap.put(RecurringTimeIntervalImpl.class, "timeInterval");
			termMap.put(RelativeAssetPositionImpl.class, "relativePosition");
			termMap.put(RelativeAssetSizeImpl.class, "relativeSize");
			termMap.put(RelativeSpatialAssetPositionImpl.class, "relativeSpatialPosition");
			termMap.put(RelativeTemporalAssetPositionImpl.class, "relativeTemporalPosition");
			termMap.put(RenditionResolutionImpl.class, "resolution");
			termMap.put(SystemDeviceImpl.class, "systemDevice");
			termMap.put(UnitOfCountImpl.class, "unitOfCount");
			termMap.put(VersionImpl.class, "version");
			termMap.put(VirtualItCommunicationLocationImpl.class, "virtualLocation");
			
			//Policy
			termMap.put(AssertionImpl.class, "Assertion");
			termMap.put(PrivacyImpl.class, "Privacy");
			termMap.put(RequestImpl.class, "Request");
			termMap.put(TicketImpl.class, "Ticket");
			
			//Relation
			termMap.put(OutputImpl.class, "output");
			
		} catch (Exception e){//For noSuchFieldException
			e.printStackTrace();
		}
	}
	
	
	/**
	 * Converts an Ecore-representation of a model element to an ODRL-java-based one.
	 * Deals with cases not covered by the used maps.
	 * 
	 * @param currentEObject {@link EObject} to be converted
	 * @param odrlParent {@link ODRLClassImpl} from which the method is called
	 * @param activityElement {@link Element} in which the input model element is contained
	 * @return ODRL-java-representation, or null if no ODRL-java-representation for input found
	 *
	 */
	private  Object specialCases(EObject currentEObject, ODRLClassImpl odrlParent, Element activityElement) {
		ODRLClassImpl newObject = null;
		String objectClassName = currentEObject.eClass().getName();
		if (objectClassName.equals(odrlPackage.getLogicalConstraint().getName())) {
			EStructuralFeature classFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getLogicalConstraint_LogicalOperator().getName());
			if (currentEObject.eGet(classFeature) instanceof EEnumLiteral classEnum) {
				if (classEnum.toString().equals(LogicalOperator.NULL.getName())) {//Operator Null: LogicalConstraint only used as wrapper for the constraint without added information (using a common super-datatype to make both eligible as value does not work with papyrus)
					if (getValue(currentEObject, odrlPackage.getLogicalConstraint_Constraints()) instanceof List constraintList) {
						List<ConstraintImpl> constraints = new ConstraintListImpl();
						constraints.addAll(addElement(constraintList, odrlParent, activityElement, ConstraintImpl.class));
						return constraints;//TODO watch out in with doubled parent-assignment.
					}//may need to be returned directly and not just assigned so that the fill-method is not called twice (in this method at the end and in the one called with the constraintList). Alternatively: alreadyProcessed-Boolean or something like that, that prevents adding parents and calling the fill()-method (should not prevent adding to the referenceList (as the called methods add with another key))
				} else {
					newObject=new LogicalConstraintImpl();
				}
			}
		}
		return newObject;
	}
	
	
	/**
	 * Converts an Ecore-representation of a model element to an ODRL-java-based one and fills its attributes, or returns the ODRL-java-based representation if it already exists.
	 * 
	 * 
	 * @param currentEObject model element to be converted
	 * @param odrlParent {@link ODRLClassImpl} from which the method is called
	 * @param activityElement {@link Element} in which the input model element is contained
	 * @return ODRL-java-representation, or null if no ODRL-java-representation for input found
	 */
	public  Object addElement(EObject currentEObject, ODRLClassImpl odrlParent, Element activityElement) {
		Object newObject = null;
		newObject = referencingMap.get(currentEObject);
		if (newObject != null) {//currentEObject is a stereotypeApplication that already was processed
			return newObject;
		}
		newObject = getOdrlObject(currentEObject, odrlParent, activityElement);
		if (newObject ==null) {
			newObject = specialCases(currentEObject, odrlParent, activityElement);
		}		
		if (newObject instanceof ODRLClassImpl newObjectOdrl) {
			newObjectOdrl.setHandler(this);//Possibly TODO Needs to be done before any further operations (as those operations rely on the . Currently not done in the constructor as that requires manual changes in all ODRL-classes every time the approach is changed
			newObjectOdrl.fill(currentEObject, activityElement); //TODO: Add boolean-return to fill to notify whether an object should be given back or not (since the ODRLClass-Creation based on Features always is executed no matter whether the object in question has a value with the feature)
		}
		
		return newObject;
	}
	
	/**
	 * Converts an Ecore-representation of a list to an ODRL-java-based one.
	 * 
	 * @param <T> the {@link Class} of the list elements
	 * @param currentList {@link List} to be converted
	 * @param odrlParent {@link ODRLClassImpl} from which the method is called
	 * @param activityElement {@link Element} in which the input model element is contained
	 * @param type the {@link Class} of the list elements
	 * @return list of type T, or null if no ODRL-java-representation of the type was found for one of the list elements
	 */
	public <T> List<T> addElement(List currentList, ODRLClassImpl odrlParent, Element activityElement, Class<T> type) {//No check for several layers of lists as that case does not occur in the current model
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
	
	
	/**
	 * Returns input string
	 * @param currentObject {@link String} to be returned
	 * @param odrlParent
	 * @param activityElement
	 * @return the string
	 */
	private String addElement(String currentObject, ODRLClassImpl odrlParent, Element activityElement) {
		return currentObject;
	}
	
	
	/**
	 * Gets the value referred to by the {@link EStructuralFeature} of an {@link EObject} by name.
	 * That means the {@link EStructuralFeature} does not actually need to be a {@link EStructuralFeature} of the {@link EObject}'s {@link EClass}, just share the name of one.
	 * 
	 * @param eObject {@link EObject} containing the feature
	 * @param feature {@link EStructuralFeature} used to access the value
	 * @return value referred by same-name {@link EStructuralFeature} of the {@link EObject}, or null if no such feature exists
	 */
	public static Object getValue(EObject eObject, EStructuralFeature feature) {
		return eObject.eGet(eObject.eClass().getEStructuralFeature(feature.getName()));
	}
	
	
	/**
	 * Converts an Ecore-representation of a model element to an ODRL-java-based one.
	 * Deals with cases covered by the used maps.
	 * 
	 * @param currentEObject model element to be converted
	 * @param odrlParent {@link ODRLClassImpl} from which the method is called
	 * @param activityElement {@link Element} in which the input model element is contained
	 * @return ODRL-java-representation, or null if no ODRL-java-representation for input found
	 */
	public  ODRLClassImpl getOdrlObject(EObject eObject, ODRLClassImpl odrlParent, EObject activityElement) {
		if (referencingMap.get(eObject)!=null) {
			return referencingMap.get(eObject);
		}
		Class<? extends ODRLClassImpl> odrlClassImpl = null;
		if (eObject instanceof EEnumLiteral enumLiteral) {//Object represented as Enumeration in the read model
			Map<String, Class<? extends ODRLClassImpl>> literalMap = enumMap.get(enumLiteral.getEEnum().getName());
			if (literalMap!= null) {
				odrlClassImpl = literalMap.get(enumLiteral.getName());
			}
		} else if (eObject instanceof EStructuralFeature eFeature) {//Object represented as feature in the read model
			String featureName = eFeature.getName();
			String owningClassName = eFeature.getEContainingClass().getName();
			StringTuple tuple = new StringTuple(owningClassName,featureName);
			odrlClassImpl = featureMap.get(tuple);
		} else {//Object represented as other type of EObject
			String className = eObject.eClass().getName();
			odrlClassImpl = classMap.get(className);
			if (odrlClassImpl==null) {
				String typeFeatureString = typeEnumMap1.get(className);
				EStructuralFeature typeFeature = eObject.eClass().getEStructuralFeature(typeFeatureString);
				if (typeFeature != null) {
					Object typeValue = eObject.eGet(typeFeature);
					if (typeValue instanceof EEnumLiteral enumLiteral) {
						Map<String, Class<? extends ODRLClassImpl>> typeEnums = typeEnumMap2.get(className);
						if (typeEnums != null) {
							odrlClassImpl = typeEnums.get(enumLiteral.getName());
						}
					}
				}
			}
		}
		if (odrlClassImpl!=null && ODRLClassImpl.class.isAssignableFrom(odrlClassImpl)) {
			Class<ODRLClassImpl> newClass = (Class<ODRLClassImpl>) odrlClassImpl;
			try {
			ODRLClassImpl newObject = newClass.getConstructor().newInstance();
			return newObject;
			} catch(Exception e) {
				e.printStackTrace();
			}
		}
		return null;
	}
	
	
	public Object printMap(Object obj) {
		if (obj instanceof ODRLClassImpl odrlObj) {
			try {
				return odrlObj.createMap(new HashSet<ODRLClassImpl>());
			} catch (NoSuchFieldException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (SecurityException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return null;
	}
	
	
	/**
	 * Creates a JSON-LD-structured map from an {@link ODRLClassImpl} by passing the task to it.
	 * @param object {@link ODRLClassImpl} to create map from
	 * @param circlePreventionSet {@link Set} to prevent infinite loops
	 * @return JSON-LD-structured map
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public Object createMap(ODRLClassImpl object, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException { //TODO handle Exceptions at lower level
		if (object == null) {//TODO tritt u.a. bei function mit leerer Party auf. Sollte bei Policyüberprüfung abgefangen werden
			return null;
		}
		return object.createMap(circlePreventionSet);
	}
	
	/**
	 * Returns input string
	 * @param string {@link String} to be returned
	 * @param circlePreventionSet
	 * @return the string
	 */
	public String createMap(String string,  Set<ODRLClassImpl> circlePreventionSet) {
		return string;
	}
	
	
	/**
	 * Creates a JSON-LD-structured list from a {@link List} of {@link ODRLClassImpl}.
	 * @param <T> 
	 * @param list {@link List} to crate the JSOn-LD-structure from
	 * @param circlePreventionSet {@link Set} to prevent infinite loops
	 * @return JSON-LD-structured list
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public <T> List<Object>  createMap(List<T> list,  Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		List<Object> newList = new LinkedList<>();
		for (T object : list) {
			if (object instanceof ODRLClassImpl odrlObject) {
				Object conversionresult = odrlObject.createMap(circlePreventionSet);
				if (conversionresult != null) {
					newList.add(conversionresult);
				}
			} else if (object != null) {
					newList.add(object);//TODO propably remove this case
			}
		}
		return newList;
	}
	
	
	/**
	 * Creates a JSON-LD-structured representation from an {@link Object}.
	 * Passes the task to other methods depending on the type of the {@link Object}.
	 * 
	 * @param object {@link Object} to create the map from
	 * @param circlePreventionSet {@link Set} to prevent infinite loops
	 * @return JSON-LD-structured representation, or null if object-input is not of a valid type
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public Object createMap(Object object,  Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		if (object instanceof ODRLClassImpl odrlObject) {
			return createMap(odrlObject, circlePreventionSet);
		} else if (object instanceof String stringObject) {
			return createMap(stringObject, circlePreventionSet);
		} else if (object instanceof List<?> list) {
			return createMap(list, circlePreventionSet);
		}			
		return null;
	}

	
	public Map<Object, String> getTermMap() {
		return termMap;
	}
	public void addToReferencingMap(EObject stereotypeApplication, ODRLClassImpl createdObject) {
		referencingMap.put(stereotypeApplication, createdObject);
	}
	
}
