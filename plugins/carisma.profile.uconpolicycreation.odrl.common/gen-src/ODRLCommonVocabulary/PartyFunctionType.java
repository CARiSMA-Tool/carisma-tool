/**
 */
package ODRLCommonVocabulary;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Party Function Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getPartyFunctionType()
 * @model
 * @generated
 */
public enum PartyFunctionType implements Enumerator {
	/**
	 * The '<em><b>Null</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #NULL_VALUE
	 * @generated
	 * @ordered
	 */
	NULL(0, "Null", "Null"),

	/**
	 * The '<em><b>Assignee</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ASSIGNEE_VALUE
	 * @generated
	 * @ordered
	 */
	ASSIGNEE(1, "assignee", "assignee"),

	/**
	 * The '<em><b>Assigner</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ASSIGNER_VALUE
	 * @generated
	 * @ordered
	 */
	ASSIGNER(2, "assigner", "assigner"),

	/**
	 * The '<em><b>Attributed Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ATTRIBUTED_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	ATTRIBUTED_PARTY(3, "attributedParty", "attributedParty"),

	/**
	 * The '<em><b>Attributing Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ATTRIBUTING_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	ATTRIBUTING_PARTY(4, "attributingParty", "attributingParty"), /**
	 * The '<em><b>Compensated Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #COMPENSATED_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	COMPENSATED_PARTY(5, "compensatedParty", "compensatedParty"),

	/**
	 * The '<em><b>Compensating Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #COMPENSATING_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	COMPENSATING_PARTY(6, "compensatingParty", "compensatingParty"),

	/**
	 * The '<em><b>Consented Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CONSENTED_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	CONSENTED_PARTY(7, "consentedParty", "consentedParty"),

	/**
	 * The '<em><b>Consenting Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CONSENTING_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	CONSENTING_PARTY(8, "consentingParty", "consentingParty"),

	/**
	 * The '<em><b>Contracted Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CONTRACTED_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	CONTRACTED_PARTY(9, "contractedParty", "contractedParty"),

	/**
	 * The '<em><b>Contracting Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CONTRACTING_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	CONTRACTING_PARTY(10, "contractingParty", "contractingParty"),

	/**
	 * The '<em><b>Informed Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #INFORMED_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	INFORMED_PARTY(11, "informedParty", "informedParty"),

	/**
	 * The '<em><b>Informing Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #INFORMING_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	INFORMING_PARTY(12, "informingParty", "informingParty"),

	/**
	 * The '<em><b>Tracked Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TRACKED_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	TRACKED_PARTY(13, "trackedParty", "trackedParty"),

	/**
	 * The '<em><b>Tracking Party</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TRACKING_PARTY_VALUE
	 * @generated
	 * @ordered
	 */
	TRACKING_PARTY(14, "trackingParty", "trackingParty");

	/**
	 * The '<em><b>Null</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #NULL
	 * @model name="Null"
	 * @generated
	 * @ordered
	 */
	public static final int NULL_VALUE = 0;

	/**
	 * The '<em><b>Assignee</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ASSIGNEE
	 * @model name="assignee"
	 * @generated
	 * @ordered
	 */
	public static final int ASSIGNEE_VALUE = 1;

	/**
	 * The '<em><b>Assigner</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ASSIGNER
	 * @model name="assigner"
	 * @generated
	 * @ordered
	 */
	public static final int ASSIGNER_VALUE = 2;

	/**
	 * The '<em><b>Attributed Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ATTRIBUTED_PARTY
	 * @model name="attributedParty"
	 * @generated
	 * @ordered
	 */
	public static final int ATTRIBUTED_PARTY_VALUE = 3;

	/**
	 * The '<em><b>Attributing Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ATTRIBUTING_PARTY
	 * @model name="attributingParty"
	 * @generated
	 * @ordered
	 */
	public static final int ATTRIBUTING_PARTY_VALUE = 4;

	/**
	 * The '<em><b>Compensated Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #COMPENSATED_PARTY
	 * @model name="compensatedParty"
	 * @generated
	 * @ordered
	 */
	public static final int COMPENSATED_PARTY_VALUE = 5;

	/**
	 * The '<em><b>Compensating Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #COMPENSATING_PARTY
	 * @model name="compensatingParty"
	 * @generated
	 * @ordered
	 */
	public static final int COMPENSATING_PARTY_VALUE = 6;

	/**
	 * The '<em><b>Consented Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CONSENTED_PARTY
	 * @model name="consentedParty"
	 * @generated
	 * @ordered
	 */
	public static final int CONSENTED_PARTY_VALUE = 7;

	/**
	 * The '<em><b>Consenting Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CONSENTING_PARTY
	 * @model name="consentingParty"
	 * @generated
	 * @ordered
	 */
	public static final int CONSENTING_PARTY_VALUE = 8;

	/**
	 * The '<em><b>Contracted Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CONTRACTED_PARTY
	 * @model name="contractedParty"
	 * @generated
	 * @ordered
	 */
	public static final int CONTRACTED_PARTY_VALUE = 9;

	/**
	 * The '<em><b>Contracting Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #CONTRACTING_PARTY
	 * @model name="contractingParty"
	 * @generated
	 * @ordered
	 */
	public static final int CONTRACTING_PARTY_VALUE = 10;

	/**
	 * The '<em><b>Informed Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #INFORMED_PARTY
	 * @model name="informedParty"
	 * @generated
	 * @ordered
	 */
	public static final int INFORMED_PARTY_VALUE = 11;

	/**
	 * The '<em><b>Informing Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #INFORMING_PARTY
	 * @model name="informingParty"
	 * @generated
	 * @ordered
	 */
	public static final int INFORMING_PARTY_VALUE = 12;

	/**
	 * The '<em><b>Tracked Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TRACKED_PARTY
	 * @model name="trackedParty"
	 * @generated
	 * @ordered
	 */
	public static final int TRACKED_PARTY_VALUE = 13;

	/**
	 * The '<em><b>Tracking Party</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TRACKING_PARTY
	 * @model name="trackingParty"
	 * @generated
	 * @ordered
	 */
	public static final int TRACKING_PARTY_VALUE = 14;

	/**
	 * An array of all the '<em><b>Party Function Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final PartyFunctionType[] VALUES_ARRAY =
		new PartyFunctionType[] {
			NULL,
			ASSIGNEE,
			ASSIGNER,
			ATTRIBUTED_PARTY,
			ATTRIBUTING_PARTY,
			COMPENSATED_PARTY,
			COMPENSATING_PARTY,
			CONSENTED_PARTY,
			CONSENTING_PARTY,
			CONTRACTED_PARTY,
			CONTRACTING_PARTY,
			INFORMED_PARTY,
			INFORMING_PARTY,
			TRACKED_PARTY,
			TRACKING_PARTY,
		};

	/**
	 * A public read-only list of all the '<em><b>Party Function Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List<PartyFunctionType> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Party Function Type</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param literal the literal.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static PartyFunctionType get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			PartyFunctionType result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Party Function Type</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param name the name.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static PartyFunctionType getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			PartyFunctionType result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Party Function Type</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the integer value.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static PartyFunctionType get(int value) {
		switch (value) {
			case NULL_VALUE: return NULL;
			case ASSIGNEE_VALUE: return ASSIGNEE;
			case ASSIGNER_VALUE: return ASSIGNER;
			case ATTRIBUTED_PARTY_VALUE: return ATTRIBUTED_PARTY;
			case ATTRIBUTING_PARTY_VALUE: return ATTRIBUTING_PARTY;
			case COMPENSATED_PARTY_VALUE: return COMPENSATED_PARTY;
			case COMPENSATING_PARTY_VALUE: return COMPENSATING_PARTY;
			case CONSENTED_PARTY_VALUE: return CONSENTED_PARTY;
			case CONSENTING_PARTY_VALUE: return CONSENTING_PARTY;
			case CONTRACTED_PARTY_VALUE: return CONTRACTED_PARTY;
			case CONTRACTING_PARTY_VALUE: return CONTRACTING_PARTY;
			case INFORMED_PARTY_VALUE: return INFORMED_PARTY;
			case INFORMING_PARTY_VALUE: return INFORMING_PARTY;
			case TRACKED_PARTY_VALUE: return TRACKED_PARTY;
			case TRACKING_PARTY_VALUE: return TRACKING_PARTY;
		}
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final int value;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String name;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private final String literal;

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private PartyFunctionType(int value, String name, String literal) {
		this.value = value;
		this.name = name;
		this.literal = literal;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public int getValue() {
	  return value;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getName() {
	  return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String getLiteral() {
	  return literal;
	}

	/**
	 * Returns the literal value of the enumerator, which is its string representation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		return literal;
	}
	
} //PartyFunctionType
