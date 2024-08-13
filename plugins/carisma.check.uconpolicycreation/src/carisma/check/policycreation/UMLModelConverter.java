package carisma.check.policycreation;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.uml2.uml.Element;
import org.json.JSONObject;

import ODRLCommonVocabulary.ConstraintOperator;
import ODRLCommonVocabulary.LeftOperand;
import ODRLCommonVocabulary.LogicalOperator;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.PartyFunctionType;
import ODRLCommonVocabulary.PolicyType;
import carisma.check.policycreation.profileclasses.ODRLClass;
import carisma.check.policycreation.profileclasses.common.action.AcceptTracking;
import carisma.check.policycreation.profileclasses.common.action.Aggregate;
import carisma.check.policycreation.profileclasses.common.action.Annotate;
import carisma.check.policycreation.profileclasses.common.action.Anonymize;
import carisma.check.policycreation.profileclasses.common.action.Archive;
import carisma.check.policycreation.profileclasses.common.action.Attribute;
import carisma.check.policycreation.profileclasses.common.action.Attribution;
import carisma.check.policycreation.profileclasses.common.action.CommercialUse;
import carisma.check.policycreation.profileclasses.common.action.Compensate;
import carisma.check.policycreation.profileclasses.common.action.ConcurrentUse;
import carisma.check.policycreation.profileclasses.common.action.Delete;
import carisma.check.policycreation.profileclasses.common.action.DerivativeWorks;
import carisma.check.policycreation.profileclasses.common.action.Derive;
import carisma.check.policycreation.profileclasses.common.action.Digitize;
import carisma.check.policycreation.profileclasses.common.action.Display;
import carisma.check.policycreation.profileclasses.common.action.Distribute;
import carisma.check.policycreation.profileclasses.common.action.Distribution;
import carisma.check.policycreation.profileclasses.common.action.EnsureExclusivity;
import carisma.check.policycreation.profileclasses.common.action.Execute;
import carisma.check.policycreation.profileclasses.common.action.Extract;
import carisma.check.policycreation.profileclasses.common.action.Give;
import carisma.check.policycreation.profileclasses.common.action.GrantUse;
import carisma.check.policycreation.profileclasses.common.action.Include;
import carisma.check.policycreation.profileclasses.common.action.Index;
import carisma.check.policycreation.profileclasses.common.action.Inform;
import carisma.check.policycreation.profileclasses.common.action.Install;
import carisma.check.policycreation.profileclasses.common.action.Modify;
import carisma.check.policycreation.profileclasses.common.action.Move;
import carisma.check.policycreation.profileclasses.common.action.NextPolicy;
import carisma.check.policycreation.profileclasses.common.action.Notice;
import carisma.check.policycreation.profileclasses.common.action.ObtainConsent;
import carisma.check.policycreation.profileclasses.common.action.Play;
import carisma.check.policycreation.profileclasses.common.action.Present;
import carisma.check.policycreation.profileclasses.common.action.Print;
import carisma.check.policycreation.profileclasses.common.action.Read;
import carisma.check.policycreation.profileclasses.common.action.Reproduce;
import carisma.check.policycreation.profileclasses.common.action.Reproduction;
import carisma.check.policycreation.profileclasses.common.action.ReviewPolicy;
import carisma.check.policycreation.profileclasses.common.action.Sell;
import carisma.check.policycreation.profileclasses.common.action.ShareAlike;
import carisma.check.policycreation.profileclasses.common.action.Sharing;
import carisma.check.policycreation.profileclasses.common.action.SourceCode;
import carisma.check.policycreation.profileclasses.common.action.Stream;
import carisma.check.policycreation.profileclasses.common.action.Synchronize;
import carisma.check.policycreation.profileclasses.common.action.TextToSpeech;
import carisma.check.policycreation.profileclasses.common.action.Transform;
import carisma.check.policycreation.profileclasses.common.action.Translate;
import carisma.check.policycreation.profileclasses.common.action.Uninstall;
import carisma.check.policycreation.profileclasses.common.action.Watermark;
import carisma.check.policycreation.profileclasses.common.function.AttributedParty;
import carisma.check.policycreation.profileclasses.common.function.AttributingParty;
import carisma.check.policycreation.profileclasses.common.function.CompensatedParty;
import carisma.check.policycreation.profileclasses.common.function.CompensatingParty;
import carisma.check.policycreation.profileclasses.common.function.ConsentedParty;
import carisma.check.policycreation.profileclasses.common.function.ConsentingParty;
import carisma.check.policycreation.profileclasses.common.function.ContractedParty;
import carisma.check.policycreation.profileclasses.common.function.ContractingParty;
import carisma.check.policycreation.profileclasses.common.function.InformedParty;
import carisma.check.policycreation.profileclasses.common.function.InformingParty;
import carisma.check.policycreation.profileclasses.common.function.TrackedParty;
import carisma.check.policycreation.profileclasses.common.function.TrackingParty;
import carisma.check.policycreation.profileclasses.common.leftoperand.AbsoluteAssetPosition;
import carisma.check.policycreation.profileclasses.common.leftoperand.AbsoluteAssetSize;
import carisma.check.policycreation.profileclasses.common.leftoperand.AbsoluteSpatialAssetPosition;
import carisma.check.policycreation.profileclasses.common.leftoperand.AbsoluteTemporalAssetPosition;
import carisma.check.policycreation.profileclasses.common.leftoperand.AssetPercentage;
import carisma.check.policycreation.profileclasses.common.leftoperand.Count;
import carisma.check.policycreation.profileclasses.common.leftoperand.DateTime;
import carisma.check.policycreation.profileclasses.common.leftoperand.DelayPeriod;
import carisma.check.policycreation.profileclasses.common.leftoperand.DeliveryChannel;
import carisma.check.policycreation.profileclasses.common.leftoperand.ElapsedTime;
import carisma.check.policycreation.profileclasses.common.leftoperand.Event;
import carisma.check.policycreation.profileclasses.common.leftoperand.FileFormat;
import carisma.check.policycreation.profileclasses.common.leftoperand.GeospatialCoordinates;
import carisma.check.policycreation.profileclasses.common.leftoperand.GeospatialNamedArea;
import carisma.check.policycreation.profileclasses.common.leftoperand.IndustryContext;
import carisma.check.policycreation.profileclasses.common.leftoperand.Language;
import carisma.check.policycreation.profileclasses.common.leftoperand.MediaContext;
import carisma.check.policycreation.profileclasses.common.leftoperand.MeteredTime;
import carisma.check.policycreation.profileclasses.common.leftoperand.PaymentAmount;
import carisma.check.policycreation.profileclasses.common.leftoperand.ProductContext;
import carisma.check.policycreation.profileclasses.common.leftoperand.Purpose;
import carisma.check.policycreation.profileclasses.common.leftoperand.Recipient;
import carisma.check.policycreation.profileclasses.common.leftoperand.RecurringTimeInterval;
import carisma.check.policycreation.profileclasses.common.leftoperand.RelativeAssetPosition;
import carisma.check.policycreation.profileclasses.common.leftoperand.RelativeAssetSize;
import carisma.check.policycreation.profileclasses.common.leftoperand.RelativeSpatialAssetPosition;
import carisma.check.policycreation.profileclasses.common.leftoperand.RelativeTemporalAssetPosition;
import carisma.check.policycreation.profileclasses.common.leftoperand.RenditionResolution;
import carisma.check.policycreation.profileclasses.common.leftoperand.SystemDevice;
import carisma.check.policycreation.profileclasses.common.leftoperand.UnitOfCount;
import carisma.check.policycreation.profileclasses.common.leftoperand.Version;
import carisma.check.policycreation.profileclasses.common.leftoperand.VirtualItCommunicationLocation;
import carisma.check.policycreation.profileclasses.common.policy.Assertion;
import carisma.check.policycreation.profileclasses.common.policy.Privacy;
import carisma.check.policycreation.profileclasses.common.policy.Request;
import carisma.check.policycreation.profileclasses.common.policy.Ticket;
import carisma.check.policycreation.profileclasses.common.relation.Output;
import carisma.check.policycreation.profileclasses.core.action.Action;
import carisma.check.policycreation.profileclasses.core.action.TransferOwnership;
import carisma.check.policycreation.profileclasses.core.action.Use;
import carisma.check.policycreation.profileclasses.core.asset.Asset;
import carisma.check.policycreation.profileclasses.core.asset.AssetCollection;
import carisma.check.policycreation.profileclasses.core.conflict.ConflictStrategy;
import carisma.check.policycreation.profileclasses.core.conflict.Permit;
import carisma.check.policycreation.profileclasses.core.conflict.Prohibit;
import carisma.check.policycreation.profileclasses.core.conflict.VoidPolicy;
import carisma.check.policycreation.profileclasses.core.constraints.Constraint;
import carisma.check.policycreation.profileclasses.core.constraints.ConstraintList;
import carisma.check.policycreation.profileclasses.core.constraints.LogicalConstraint;
import carisma.check.policycreation.profileclasses.core.failure.Consequence;
import carisma.check.policycreation.profileclasses.core.failure.Remedy;
import carisma.check.policycreation.profileclasses.core.function.Assignee;
import carisma.check.policycreation.profileclasses.core.function.Assigner;
import carisma.check.policycreation.profileclasses.core.operand.And;
import carisma.check.policycreation.profileclasses.core.operand.AndSequence;
import carisma.check.policycreation.profileclasses.core.operand.Or;
import carisma.check.policycreation.profileclasses.core.operand.Xone;
import carisma.check.policycreation.profileclasses.core.operator.EqualTo;
import carisma.check.policycreation.profileclasses.core.operator.GreaterEq;
import carisma.check.policycreation.profileclasses.core.operator.GreaterThan;
import carisma.check.policycreation.profileclasses.core.operator.HasPart;
import carisma.check.policycreation.profileclasses.core.operator.IsA;
import carisma.check.policycreation.profileclasses.core.operator.IsAllOf;
import carisma.check.policycreation.profileclasses.core.operator.IsAnyOf;
import carisma.check.policycreation.profileclasses.core.operator.IsNoneOf;
import carisma.check.policycreation.profileclasses.core.operator.IsPartOf;
import carisma.check.policycreation.profileclasses.core.operator.LessThan;
import carisma.check.policycreation.profileclasses.core.operator.LessThanEq;
import carisma.check.policycreation.profileclasses.core.operator.NotEqualTo;
import carisma.check.policycreation.profileclasses.core.party.Party;
import carisma.check.policycreation.profileclasses.core.party.PartyCollection;
import carisma.check.policycreation.profileclasses.core.policy.Agreement;
import carisma.check.policycreation.profileclasses.core.policy.Offer;
import carisma.check.policycreation.profileclasses.core.policy.Policy;
import carisma.check.policycreation.profileclasses.core.policy.Set;
import carisma.check.policycreation.profileclasses.core.relation.Relation;
import carisma.check.policycreation.profileclasses.core.relation.Target;
import carisma.check.policycreation.profileclasses.core.rule.Duty;
import carisma.check.policycreation.profileclasses.core.rule.Permission;
import carisma.check.policycreation.profileclasses.core.rule.Prohibition;
import carisma.check.policycreation.profileclasses.core.rule.Rule;

public class UMLModelConverter {
	/**
	 * Map of {@link EEnum}s and their {@link EEnumLiteral}s to the {@link ODRLClass} they represent, by name.
	 */
	private final Map<String,Map<String,Class<? extends ODRLClass>>> enumMap = new HashMap<>();
	/**
	 * Map of {@link EClass}es to the {@link EStructuralFeature} containing their ODRL-type-defining {@link EEnumLiteral}, by name.
	 */
	private final Map<String,String> typeEnumMap1 = new HashMap<>();
	/**
	 * Map of {@link EClass}es and the {@link EEnumLiteral} defining their ODRL-type to the {@link ODRLClass} they represent, by name.
	 */
	private final Map<String,Map<String,Class<? extends ODRLClass>>> typeEnumMap2 = new HashMap<>();//TODO: potential problem: if several Enumerations with same-name Literals are valid as value of the structural feature their literals are not distinguishable with the current approach
	/**
	 * Map of {@link EClass}es to the {@link ODRLClass} they represent, by name.
	 */
	private final Map<String,Class<? extends ODRLClass>> classMap = new HashMap<>();
	/**
	 * Map of {@link EClass}es and their {@link EStructuralFeature}s (wrapped in a {@link StringTuple}) to the {@link ODRLClass} they represent, by name.
	 */
	private final Map<StringTuple,Class<? extends ODRLClass>> featureMap = new HashMap<>();//Mapping of EStructuralFeatures to the ODRLClassImpl-Objects they represent
	
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
	
	private Map<EObject,ODRLClass> referencingMap = new HashMap<>();//Currently: Save top-level elements (stereotype applications) as they may be referred by several objects, others may not. (If more Elements should be accessed: Save with unique EObject, watch out for uniqueness of enums (may need to be saved as triple)
	//Also save lists, not just their elements
	private ODRLClass policyRoot;
	private java.util.Set<ODRLClass> handledOdrlClasses = new HashSet<>();
	private List<Object> topLevelMapElements = new LinkedList<>();
	private List<Map<String,Object>> contexts = new LinkedList<>();
	
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
	
	public UMLModelConverter() {};
	public UMLModelConverter(List<String> contextPaths) throws IOException {
		for(String contextPath : contextPaths) {
			ClassLoader classloader = Thread.currentThread().getContextClassLoader();
			InputStream is = classloader.getResourceAsStream(contextPath);
			
			
			try(BufferedReader br = new BufferedReader(new InputStreamReader(is))) {
			    StringBuilder sb = new StringBuilder();
			    String line = br.readLine();

			    while (line != null) {
			        sb.append(line);
			        sb.append(System.lineSeparator());
			        line = br.readLine();
			    }
			    JSONObject contextWithString = new JSONObject(sb.toString());
			    System.out.println("contextWithString: " + contextWithString.toString(2));
//			    Map<String,Object> contextMap = new HashMap<>();
//			    if (contextWithString.get("@context") instanceof JSONObject contextJO) {
//			    	contextMap.put("@context",contextJO.toMap());
//			    } //else throw exception
//			    if (contextWithString.get("@id") instanceof String conIdString) {
//			    	contextMap.put("@id",conIdString);
//			    }
			    System.out.println("context map: " + contextWithString.toMap().toString());
			    this.contexts.add(contextWithString.toMap());
			    System.out.println("actual context list: " + this.contexts.toString());
			    System.out.println("handlerID: "+ this);
			}
		}
	}
	public UMLModelConverter(String contextPath) throws IOException {
		this(Arrays.asList(contextPath));
		
	}
	
	 {
		
		enumMap.put(ODRLCommonVocabulary.ConflictStrategy.class.getSimpleName(), Map.ofEntries(
				Map.entry(ODRLCommonVocabulary.ConflictStrategy.PERMIT.getName(),Permit.class),
				Map.entry(ODRLCommonVocabulary.ConflictStrategy.PROHIBIT.getName(), Prohibit.class),
				Map.entry(ODRLCommonVocabulary.ConflictStrategy.VOID_POLICY.getName(), VoidPolicy.class)				
				));
		enumMap.put(ODRLCommonVocabulary.Action.class.getSimpleName(), Map.ofEntries(
				Map.entry(ODRLCommonVocabulary.Action.ACCEPT_TRACKING.getName(), AcceptTracking.class),
				Map.entry(ODRLCommonVocabulary.Action.AGGREGATE.getName(), Aggregate.class),
				Map.entry(ODRLCommonVocabulary.Action.ANNOTATE.getName(), Annotate.class),
				Map.entry(ODRLCommonVocabulary.Action.ANONYMIZE.getName(), Anonymize.class),
				Map.entry(ODRLCommonVocabulary.Action.ARCHIVE.getName(), Archive.class),
				Map.entry(ODRLCommonVocabulary.Action.ATTRIBUTE.getName(), Attribute.class),
				Map.entry(ODRLCommonVocabulary.Action.CC_ATTRIBUTION.getName(), Attribution.class),
				Map.entry(ODRLCommonVocabulary.Action.CC_COMMERCIAL_USE.getName(), CommercialUse.class),
				Map.entry(ODRLCommonVocabulary.Action.CC_DERIVATIVE_WORKS.getName(), DerivativeWorks.class),
				Map.entry(ODRLCommonVocabulary.Action.CC_DISTRIBUTION.getName(), Distribution.class),
				Map.entry(ODRLCommonVocabulary.Action.CC_NOTICE.getName(), Notice.class),
				Map.entry(ODRLCommonVocabulary.Action.CC_REPRODUCTION.getName(), Reproduction.class),
				Map.entry(ODRLCommonVocabulary.Action.CC_SHARE_ALIKE.getName(), ShareAlike.class),
				Map.entry(ODRLCommonVocabulary.Action.CC_SHARING.getName(), Sharing.class),
				Map.entry(ODRLCommonVocabulary.Action.CC_SOURCE_CODE.getName(), SourceCode.class),
				Map.entry(ODRLCommonVocabulary.Action.COMPENSATE.getName(), Compensate.class),
				Map.entry(ODRLCommonVocabulary.Action.CONCURRENT_USE.getName(), ConcurrentUse.class),
				Map.entry(ODRLCommonVocabulary.Action.DELETE.getName(), Delete.class),
				Map.entry(ODRLCommonVocabulary.Action.DERIVE.getName(), Derive.class),
				Map.entry(ODRLCommonVocabulary.Action.DIGITIZE.getName(), Digitize.class),
				Map.entry(ODRLCommonVocabulary.Action.DISPLAY.getName(), Display.class),
				Map.entry(ODRLCommonVocabulary.Action.DISTRIBUTE.getName(), Distribute.class),
				Map.entry(ODRLCommonVocabulary.Action.ENSURE_EXCLUSIVITY.getName(), EnsureExclusivity.class),
				Map.entry(ODRLCommonVocabulary.Action.EXECUTE.getName(), Execute.class),
				Map.entry(ODRLCommonVocabulary.Action.EXTRACT.getName(), Extract.class),
				Map.entry(ODRLCommonVocabulary.Action.GIVE.getName(), Give.class),
				Map.entry(ODRLCommonVocabulary.Action.GRANT_USE.getName(), GrantUse.class),
				Map.entry(ODRLCommonVocabulary.Action.INCLUDE.getName(), Include.class),
				Map.entry(ODRLCommonVocabulary.Action.INDEX.getName(), Index.class),
				Map.entry(ODRLCommonVocabulary.Action.INFORM.getName(), Inform.class),
				Map.entry(ODRLCommonVocabulary.Action.INSTALL.getName(), Install.class),
				Map.entry(ODRLCommonVocabulary.Action.MODIFY.getName(), Modify.class),
				Map.entry(ODRLCommonVocabulary.Action.MOVE.getName(), Move.class),
				Map.entry(ODRLCommonVocabulary.Action.NEXT_POLICY.getName(), NextPolicy.class),
				Map.entry(ODRLCommonVocabulary.Action.OBTAIN_CONSENT.getName(), ObtainConsent.class),
				Map.entry(ODRLCommonVocabulary.Action.PLAY.getName(), Play.class),
				Map.entry(ODRLCommonVocabulary.Action.PRESENT.getName(), Present.class),
				Map.entry(ODRLCommonVocabulary.Action.PRINT.getName(), Print.class),
				Map.entry(ODRLCommonVocabulary.Action.READ.getName(), Read.class),
				Map.entry(ODRLCommonVocabulary.Action.REPRODUCE.getName(), Reproduce.class),
				Map.entry(ODRLCommonVocabulary.Action.REVIEW_POLICY.getName(), ReviewPolicy.class),
				Map.entry(ODRLCommonVocabulary.Action.SELL.getName(), Sell.class),
				Map.entry(ODRLCommonVocabulary.Action.STREAM.getName(), Stream.class),
				Map.entry(ODRLCommonVocabulary.Action.SYNCHRONIZE.getName(), Synchronize.class),
				Map.entry(ODRLCommonVocabulary.Action.TEXT_TO_SPEECH.getName(), TextToSpeech.class),
				Map.entry(ODRLCommonVocabulary.Action.TRANSFER.getName(), TransferOwnership.class),
				Map.entry(ODRLCommonVocabulary.Action.TRANSFORM.getName(), Transform.class),
				Map.entry(ODRLCommonVocabulary.Action.TRANSLATE.getName(), Translate.class),
				Map.entry(ODRLCommonVocabulary.Action.UNINSTALL.getName(), Uninstall.class),
				Map.entry(ODRLCommonVocabulary.Action.USE.getName(), Use.class),
				Map.entry(ODRLCommonVocabulary.Action.WATERMARK.getName(), Watermark.class)
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
				Map.entry(LeftOperand.ABSOLUTE_SPATIAL_POSITION.getName(), AbsoluteSpatialAssetPosition.class),//TODO correct spelling from spartial to spatial
				Map.entry(LeftOperand.ABSOLUTE_TEMPORAL_POSITION.getName(), AbsoluteTemporalAssetPosition.class),
				Map.entry(LeftOperand.PERCENTAGE.getName(), AssetPercentage.class),
				Map.entry(LeftOperand.COUNT.getName(), Count.class),
				Map.entry(LeftOperand.DATE_TIME.getName(), DateTime.class),
				Map.entry(LeftOperand.DELAY_PERIOD.getName(), DelayPeriod.class),
				Map.entry(LeftOperand.DELIVERY_CHANNEL.getName(), DeliveryChannel.class),
				Map.entry(LeftOperand.ELAPSED_TIME.getName(), ElapsedTime.class),
				Map.entry(LeftOperand.EVENT.getName(), Event.class),
				Map.entry(LeftOperand.FILE_FORMAT.getName(), FileFormat.class),
				Map.entry(LeftOperand.SPATIAL_COORDINATES.getName(), GeospatialCoordinates.class),//TODO correct spelling from spartial to spatial
				Map.entry(LeftOperand.SPATIAL.getName(), GeospatialNamedArea.class),//TODO correct spelling from spartial to spatial
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
				Map.entry(LeftOperand.RELATIVE_SPATIAL_POSITION.getName(), RelativeSpatialAssetPosition.class),//TODO correct spelling from spartial to spatial
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
		//Explicit Relations were removed from the diagram
//		typeEnumMap1.put(odrlPackage.getAssetRelation().getName(), odrlPackage.getAssetRelation_Type().getName());
//		typeEnumMap2.put(odrlPackage.getAssetRelation().getName(), Map.ofEntries(
//				Map.entry(AssetRelationType.TARGET.getName(), TargetImpl.class),
//				Map.entry(AssetRelationType.OUTPUT.getName(), OutputImpl.class)
//				));
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
		
		//Missing: LogicalConstraint (is in specialCases)
		
		
		try {
			//ODRL-Core
			//Action
			termMap.put(Action.class, "Action");//Currently abstract
			termMap.put(Action.class.getDeclaredField("refinement"), "refinement");
			termMap.put(TransferOwnership.class, "transfer");
			termMap.put(Use.class, "use");
			//Asset
			termMap.put(Asset.class, "Asset");
			termMap.put(Asset.class.getDeclaredField("uid"), "uid");
			termMap.put(AssetCollection.class, "AssetCollection");
			termMap.put(AssetCollection.class.getDeclaredField("source"), "source");
			termMap.put(AssetCollection.class.getDeclaredField("refinement"), "refinement");
			//Conflict
			termMap.put(ConflictStrategy.class, "ConflictTerm");//Currently abstract
			termMap.put(Permit.class, "perm");
			termMap.put(Prohibition.class, "prohibit");
			termMap.put(VoidPolicy.class, "invalid");
			//Constraint
			termMap.put(Constraint.class, "Constraint");
			termMap.put(Constraint.class.getDeclaredField("uid"), "uid");
			termMap.put(Constraint.class.getDeclaredField("leftOperand"), "leftOperand");
			termMap.put(Constraint.class.getDeclaredField("operator"), "operator");
//			termMap.put(Constraint.class.getDeclaredField("rightOperand"), "rightOperand"); //TODO removed to put dataType into rightOperands instead of contraint
//			termMap.put(Constraint.class.getDeclaredField("rightOperandReference"), "rightOperandReference"); //TODO removed to put dataType into rightOperands instead of contraint
			//termMap.put(Constraint.class.getDeclaredField("dataType"), "dataType"); //TODO removed to put the type into the operands instead of the constraint
			termMap.put(Constraint.class.getDeclaredField("unit"), "unit");
			termMap.put(Constraint.class.getDeclaredField("status"), "status");
			termMap.put(LogicalConstraint.class, "LogicalConstraint");
			termMap.put(LogicalConstraint.class.getDeclaredField("uid"), "uid");
			//termMap.put(LogicalConstraintImpl.class.getDeclaredField("operand"), "operand");//TODO Possibly remove as only subproperties of operand are used
			//Failure
			//termMap.put(FailureImpl.class, ""); //Is (Sub)-Property, does not exist as class in the model
			termMap.put(Consequence.class, "consequence"); //Is (Sub)-Property, does not exist as class in the model
			termMap.put(Remedy.class, "remedy"); //Is (Sub)-Property, does not exist as class in the model
			//Function
			//termMap.put(FunctionImpl.class, "function"); //Is (Sub)-Property, does not exist as class in the model
			termMap.put(Assignee.class, "assignee"); //Is (Sub)-Property, does not exist as class in the model
			termMap.put(Assigner.class, "assigner"); //Is (Sub)-Property, does not exist as class in the model
			//LeftOperand
			termMap.put(LeftOperand.class, "LeftOperand");//Currently abstract
			//Operand
			//termMap.put(OperandImpl.class, "");
			//termMap.put(Operand.class.getDeclaredField("constraints"), "TODO:Remove operand from termMap");//TODO:Remove, just here for testing
			termMap.put(And.class, "and");
			termMap.put(AndSequence.class, "andSequence");
			termMap.put(Or.class, "or");
			termMap.put(Xone.class, "xone");
			//Operator
			//termMap.put(OperatorImpl.class, "");
			termMap.put(EqualTo.class, "eq");
			termMap.put(GreaterEq.class, "gteq");
			termMap.put(GreaterThan.class, "gt");
			termMap.put(HasPart.class, "hasPart");
			termMap.put(IsA.class, "isA");
			termMap.put(IsAllOf.class, "isAllOf");
			termMap.put(IsAnyOf.class, "isAnyOf");
			termMap.put(IsNoneOf.class, "isNoneOf");
			termMap.put(IsPartOf.class, "isPartOf");
			termMap.put(LessThanEq.class, "lteq");
			termMap.put(LessThan.class, "lt");
			termMap.put(NotEqualTo.class, "neq");
			//Party
			termMap.put(Party.class, "Party");
			termMap.put(Party.class.getDeclaredField("uid"), "uid");
			termMap.put(PartyCollection.class, "PartyCollection");
			termMap.put(PartyCollection.class.getDeclaredField("refinement"), "refinement");
			termMap.put(PartyCollection.class.getDeclaredField("source"), "source");
			//Policy
			termMap.put(Policy.class, "Policy");
			termMap.put(Policy.class.getDeclaredField("uid"), "uid");
			termMap.put(Policy.class.getDeclaredField("conflictStrategy"), "conflict");
			//termMap.put(Policy.class.getDeclaredField("profiles"), "profile");//TODO removed to change single-element-list to single element in manual process
			termMap.put(Policy.class.getDeclaredField("inheritsFrom"), "inheritFrom");
			termMap.put(Policy.class.getDeclaredField("permission"), "permission");
			termMap.put(Policy.class.getDeclaredField("obligation"), "obligation");
			termMap.put(Policy.class.getDeclaredField("prohibition"), "prohibition");
			termMap.put(Agreement.class, "Agreement");
			termMap.put(Offer.class, "Offer");
			termMap.put(Set.class, "Set");
			//Relation
			termMap.put(Relation.class, "relation"); //Is (Sub)-Property, does not exist as class in the model
			termMap.put(Target.class, "target"); //Is (Sub)-Property, does not exist as class in the model
			//RightOperand//TODO
			//
			//Rule
			termMap.put(Rule.class, "Rule");//Currently abstract
			termMap.put(Rule.class.getDeclaredField("uid"), "uid");
			//termMap.put(Rule.class.getDeclaredField("involvedParties"), ""); //handled through value types
			//termMap.put(Rule.class.getDeclaredField("involvedAssets"), ""); //handled through value types
			termMap.put(Rule.class.getDeclaredField("action"), "action");
			termMap.put(Rule.class.getDeclaredField("constraint"), "constraint");
			termMap.put(Duty.class, "Duty");
			termMap.put(Duty.class.getDeclaredField("consequence"), "consequence");//TODO: maybe change to failure
			termMap.put(Permission.class, "Permission");
			termMap.put(Permission.class.getDeclaredField("duties"), "duty");
			termMap.put(Prohibition.class, "Prohibition");
			termMap.put(Prohibition.class.getDeclaredField("remedy"), "remedy");//TODO: maybe change to failure
			
			//ODRL-Common
			//Action
			termMap.put(AcceptTracking.class, "acceptTracking");
			termMap.put(Aggregate.class, "aggregate");
			termMap.put(Annotate.class, "annotate");
			termMap.put(Anonymize.class, "anonymize");
			termMap.put(Archive.class, "archive");
			termMap.put(Attribute.class, "attribute");
			termMap.put(Attribution.class, "cc:Attribution");//TODO: maybe deal with creative commons terms differently (identify creative commons terms by the cc: in their ID currently)
			termMap.put(CommercialUse.class, "cc:CommericalUse");
			termMap.put(Compensate.class, "compensate");
			termMap.put(ConcurrentUse.class, "concurrentUse");
			termMap.put(Delete.class, "delete");
			termMap.put(DerivativeWorks.class, "cc:DerivativeWorks");
			termMap.put(Derive.class, "derive");
			termMap.put(Digitize.class, "digitize");
			termMap.put(Display.class, "display");
			termMap.put(Distribute.class, "distribute");
			termMap.put(Distribution.class, "cc:Distribution");
			termMap.put(EnsureExclusivity.class, "ensureExclusivity");
			termMap.put(Execute.class, "execute");
			termMap.put(Extract.class, "extract");
			termMap.put(Give.class, "give");
			termMap.put(GrantUse.class, "grantUse");
			termMap.put(Include.class, "include");
			termMap.put(Index.class, "index");
			termMap.put(Inform.class, "inform");
			termMap.put(Install.class, "install");
			termMap.put(Modify.class, "modify");
			termMap.put(Move.class, "move");
			termMap.put(NextPolicy.class, "nextPolicy");
			termMap.put(Notice.class, "cc:Notice");
			termMap.put(ObtainConsent.class, "obtainConsent");
			termMap.put(Play.class, "play");
			termMap.put(Present.class, "present");
			termMap.put(Print.class, "print");
			termMap.put(Read.class, "read");
			termMap.put(Reproduce.class, "reproduce");
			termMap.put(Reproduction.class, "cc:Reproduction");
			termMap.put(ReviewPolicy.class, "reviewPolicy");
			termMap.put(Sell.class, "sell");
			termMap.put(ShareAlike.class, "cc:ShareAlike");
			termMap.put(Sharing.class, "cc:Sharing");
			termMap.put(SourceCode.class, "cc:SourceCode");
			termMap.put(Stream.class, "stream");
			termMap.put(Synchronize.class, "synchronize");
			termMap.put(TextToSpeech.class, "textToSpeech");
			termMap.put(Transform.class, "transform");
			termMap.put(Translate.class, "translate");
			termMap.put(Uninstall.class, "uninstall");
			termMap.put(Watermark.class, "watermark");
			
			//Function
			termMap.put(AttributedParty.class, "attributedParty");
			termMap.put(AttributingParty.class, "attributingParty");
			termMap.put(CompensatedParty.class, "compensatedParty");
			termMap.put(CompensatingParty.class, "compensatingParty");
			termMap.put(ConsentedParty.class, "consentedParty");
			termMap.put(ConsentingParty.class, "consentingParty");
			termMap.put(ContractedParty.class, "contractedParty");
			termMap.put(ContractingParty.class, "contractingParty");
			termMap.put(InformedParty.class, "informedParty");
			termMap.put(InformingParty.class, "informingParty");
			termMap.put(TrackedParty.class, "trackedParty");
			termMap.put(TrackingParty.class, "trackingParty");
			
			//LeftOperand
			termMap.put(AbsoluteAssetPosition.class, "absolutePosition");
			termMap.put(AbsoluteAssetSize.class, "absoluteSize");
			termMap.put(AbsoluteSpatialAssetPosition.class, "absoluteSpatialPosition");
			termMap.put(AbsoluteTemporalAssetPosition.class, "absoluteTemporalPosition");
			termMap.put(AssetPercentage.class, "percentage");
			termMap.put(Count.class, "count");
			termMap.put(DateTime.class, "dateTime");
			termMap.put(DelayPeriod.class, "delayPeriod");
			termMap.put(DeliveryChannel.class, "deliveryChannel");
			termMap.put(ElapsedTime.class, "elapsedTime");
			termMap.put(Event.class, "event");
			termMap.put(FileFormat.class, "fileFormat");
			termMap.put(GeospatialCoordinates.class, "spatialCoordinates");
			termMap.put(GeospatialNamedArea.class, "spatial");
			termMap.put(IndustryContext.class, "industry");
			termMap.put(Language.class, "language");
			termMap.put(MediaContext.class, "media");
			termMap.put(MeteredTime.class, "meteredTime");
			termMap.put(PaymentAmount.class, "payAmount");
			termMap.put(ProductContext.class, "product");
			termMap.put(Purpose.class, "purpose");
			termMap.put(Recipient.class, "recipient");
			termMap.put(RecurringTimeInterval.class, "timeInterval");
			termMap.put(RelativeAssetPosition.class, "relativePosition");
			termMap.put(RelativeAssetSize.class, "relativeSize");
			termMap.put(RelativeSpatialAssetPosition.class, "relativeSpatialPosition");
			termMap.put(RelativeTemporalAssetPosition.class, "relativeTemporalPosition");
			termMap.put(RenditionResolution.class, "resolution");
			termMap.put(SystemDevice.class, "systemDevice");
			termMap.put(UnitOfCount.class, "unitOfCount");
			termMap.put(Version.class, "version");
			termMap.put(VirtualItCommunicationLocation.class, "virtualLocation");
			
			//Policy
			termMap.put(Assertion.class, "Assertion");
			termMap.put(Privacy.class, "Privacy");
			termMap.put(Request.class, "Request");
			termMap.put(Ticket.class, "Ticket");
			
			//Relation
			termMap.put(Output.class, "output");
			
		} catch (Exception e){//For noSuchFieldException
			e.printStackTrace();
		}
	}
	
	
	/**
	 * Converts an Ecore-representation of a model element to an ODRL-java-based one.
	 * Deals with cases not covered by the used maps.
	 * 
	 * @param currentEObject {@link EObject} to be converted
	 * @param odrlParent {@link ODRLClass} from which the method is called
	 * @param activityElement {@link Element} in which the input model element is contained
	 * @return ODRL-java-representation, or null if no ODRL-java-representation for input found
	 *
	 */
	private  Object specialCases(EObject currentEObject, ODRLClass odrlParent, Element activityElement) {
		ODRLClass newObject = null;
		String objectClassName = currentEObject.eClass().getName();
		if (objectClassName.equals(odrlPackage.getLogicalConstraint().getName())) {
			EStructuralFeature classFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getLogicalConstraint_LogicalOperator().getName());
			if (currentEObject.eGet(classFeature) instanceof EEnumLiteral classEnum) {
				if (classEnum.toString().equals(LogicalOperator.NULL.getName())) {//Operator Null: LogicalConstraint only used as wrapper for the constraint without added information (using a common super-datatype to make both eligible as value does not work with papyrus)
					if (getValue(currentEObject, odrlPackage.getLogicalConstraint_Constraints()) instanceof List constraintList) {
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
	
	
	/**
	 * Converts an Ecore-representation of a model element to an ODRL-java-based one and fills its attributes, or returns the ODRL-java-based representation if it already exists.
	 * 
	 * 
	 * @param currentEObject model element to be converted
	 * @param odrlParent {@link ODRLClass} from which the method is called
	 * @param activityElement {@link Element} in which the input model element is contained
	 * @return ODRL-java-representation, or null if no ODRL-java-representation for input found
	 */
	public  Object addElement(EObject currentEObject, ODRLClass odrlParent, Element activityElement) {
		Object newObject = null;
		newObject = referencingMap.get(currentEObject);
		if (newObject != null) {//currentEObject is a stereotypeApplication that already was processed
			return newObject;
		}
		newObject = getOdrlObject(currentEObject, odrlParent, activityElement);
		if (newObject ==null) {
			newObject = specialCases(currentEObject, odrlParent, activityElement);
		}		
		if (newObject instanceof ODRLClass newObjectOdrl) {
			newObjectOdrl.setHandler(this);//Possibly TODO Needs to be done before any further operations (as those operations rely on the . Currently not done in the constructor as that requires manual changes in all ODRL-classes every time the approach is changed
			if (odrlParent != null) {
				newObjectOdrl.addReferredBy(odrlParent);
			}
			newObjectOdrl.fill(currentEObject, activityElement); //TODO: possibly add boolean-return to fill to notify whether an object should be given back or not (since the ODRLClass-Creation based on Features always is executed no matter whether the object in question has a value with the feature)
		}
		
		return newObject;
	}
	
	/**
	 * Converts an Ecore-representation of a list to an ODRL-java-based one.
	 * 
	 * @param <T> the {@link Class} of the list elements
	 * @param currentList {@link List} to be converted
	 * @param odrlParent {@link ODRLClass} from which the method is called
	 * @param activityElement {@link Element} in which the input model element is contained
	 * @param type the {@link Class} of the list elements
	 * @return list of type T, or null if no ODRL-java-representation of the type was found for one of the list elements
	 */
	public <T> List<T> addElement(List currentList, ODRLClass odrlParent, Element activityElement, Class<T> type) {//No check for several layers of lists as that case does not occur in the current model
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
	private String addElement(String currentObject, ODRLClass odrlParent, Element activityElement) {
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
	 * @param odrlParent {@link ODRLClass} from which the method is called
	 * @param activityElement {@link Element} in which the input model element is contained
	 * @return ODRL-java-representation, or null if no ODRL-java-representation for input found
	 */
	public  ODRLClass getOdrlObject(EObject eObject, ODRLClass odrlParent, EObject activityElement) {
		if (referencingMap.get(eObject)!=null) {
			return referencingMap.get(eObject);
		}
		Class<? extends ODRLClass> odrlClassImpl = null;
		if (eObject instanceof EEnumLiteral enumLiteral) {//Object represented as Enumeration in the read model
			Map<String, Class<? extends ODRLClass>> literalMap = enumMap.get(enumLiteral.getEEnum().getName());
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
						Map<String, Class<? extends ODRLClass>> typeEnums = typeEnumMap2.get(className);
						if (typeEnums != null) {
							odrlClassImpl = typeEnums.get(enumLiteral.getName());
						}
					}
				}
			}
		}
		if (odrlClassImpl!=null && ODRLClass.class.isAssignableFrom(odrlClassImpl)) {
			Class<ODRLClass> newClass = (Class<ODRLClass>) odrlClassImpl;
			try {
			ODRLClass newObject = newClass.getConstructor().newInstance();
			return newObject;
			} catch(Exception e) {
				e.printStackTrace();
			}
		}
		return null;
	}
	
	
	public Object printMap(Object obj) {
		if (obj instanceof ODRLClass odrlObj) {
			try {
				return odrlObj.createMap(new HashSet<ODRLClass>());
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
	 * Creates a JSON-LD-structured map from an {@link ODRLClass} by passing the task to it.
	 * @param object {@link ODRLClass} to create map from
	 * @param circlePreventionSet {@link java.util.Set} to prevent infinite loops
	 * @return JSON-LD-structured map
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public Object createMap(ODRLClass object, java.util.Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException { //TODO handle Exceptions at lower level
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
	public String createMap(String string,  java.util.Set<ODRLClass> circlePreventionSet) {
		return string;
	}
	
	
	/**
	 * Creates a JSON-LD-structured list from a {@link List} of {@link ODRLClass}.
	 * @param <T> 
	 * @param list {@link List} to crate the JSOn-LD-structure from
	 * @param circlePreventionSet {@link java.util.Set} to prevent infinite loops
	 * @return JSON-LD-structured list
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public <T> List<Object>  createMap(List<T> list,  java.util.Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		List<Object> newList = new LinkedList<>();
		for (T object : list) {
			if (object instanceof ODRLClass odrlObject) {
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
	 * @param circlePreventionSet {@link java.util.Set} to prevent infinite loops
	 * @return JSON-LD-structured representation, or null if object-input is not of a valid type
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public Object createMap(Object object,  java.util.Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		if (object instanceof ODRLClass odrlObject) {
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
	public void addToReferencingMap(EObject stereotypeApplication, ODRLClass createdObject) {
		referencingMap.put(stereotypeApplication, createdObject);
	}


	public ODRLClass getPolicyRoot() {
		return policyRoot;
	}

	public void setPolicyRoot(ODRLClass root) {
		this.policyRoot = root;
	}

	public java.util.Set<ODRLClass> getHandledOdrlClasses() {
		return handledOdrlClasses;
	}

	public void setHandledOdrlClasses(java.util.Set<ODRLClass> handledOdrlClasses) {
		this.handledOdrlClasses = handledOdrlClasses;
	}
	public void addToHandledOdrlClasses(ODRLClass handledClass) {
		this.handledOdrlClasses.add(handledClass);
	}

	public List<Object> getTopLevelMapElements() {
		return topLevelMapElements;
	}

	public void setTopLevelMapElements(List<Object> topLevelMapElements) {
		this.topLevelMapElements = topLevelMapElements;
	}
	public void addToTopLevelMapElements(Object addElement) {
		topLevelMapElements.add(addElement);
	}


	public List<Map<String, Object>> getContexts() {
		return contexts;
	}


	public void setContexts(List<Map<String, Object>> contexts) {
		this.contexts = contexts;
	}
	
	public void addContext(Map<String,Object> context) {
		contexts.add(context);
	}
	
	public String applyContext(String inputString) {
		System.out.println("handlerID: "+ this);
		System.out.println("Contexts in Method: " + contexts.toString());
		for (Map<String,Object> contextWithId : contexts) {
			System.out.println("Context in Method: " + contextWithId.toString());
			System.out.println("ContextPrint: "+ new JSONObject(contextWithId).toString(3));
			if (contextWithId.get("@context") instanceof Map<?,?> contextMap && contextMap.get(inputString) instanceof String extendedString) {
					return extendedString;
			}
		}
		return null;
	}
	public Map<String,Object> getContextMap() {
		Object contextResult = null;
		if (contexts.size()==1) {
			Map<String,Object> context = contexts.get(0);
			contextResult = context.get("@id");
			if (!(contextResult instanceof String)) {
				contextResult = context.get("@context");
			}
		}
		
		else if (contexts.size()>1){
			List<Object> contextList = new LinkedList<>();
			for (Map<String,Object> context : contexts) {
				Object contextLocalResult = context.get("@id");
				if (!(contextLocalResult instanceof String)) {
					contextLocalResult = context.get("@context");
				}
				contextList.add(contextLocalResult);
			}
			contextResult = contextList;
		}
		Map<String,Object> resultMap = new HashMap<>();
		resultMap.put("@context", contextResult);
		return resultMap;
	}



	
}
