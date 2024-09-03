/**
 */
package ODRLCommonVocabulary;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Policy Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getPolicyType()
 * @model
 * @generated
 */
public enum PolicyType implements Enumerator {
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
	 * The '<em><b>Agreement</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #AGREEMENT_VALUE
	 * @generated
	 * @ordered
	 */
	AGREEMENT(1, "Agreement", "Agreement"),

	/**
	 * The '<em><b>Assertion</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ASSERTION_VALUE
	 * @generated
	 * @ordered
	 */
	ASSERTION(2, "Assertion", "Assertion"),

	/**
	 * The '<em><b>Offer</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #OFFER_VALUE
	 * @generated
	 * @ordered
	 */
	OFFER(3, "Offer", "Offer"),

	/**
	 * The '<em><b>Privacy</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PRIVACY_VALUE
	 * @generated
	 * @ordered
	 */
	PRIVACY(4, "Privacy", "Privacy"),

	/**
	 * The '<em><b>Request</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #REQUEST_VALUE
	 * @generated
	 * @ordered
	 */
	REQUEST(5, "Request", "Request"),

	/**
	 * The '<em><b>Set</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SET_VALUE
	 * @generated
	 * @ordered
	 */
	SET(6, "Set", "Set"),

	/**
	 * The '<em><b>Ticket</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TICKET_VALUE
	 * @generated
	 * @ordered
	 */
	TICKET(7, "Ticket", "Ticket");

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
	 * The '<em><b>Agreement</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #AGREEMENT
	 * @model name="Agreement"
	 * @generated
	 * @ordered
	 */
	public static final int AGREEMENT_VALUE = 1;

	/**
	 * The '<em><b>Assertion</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ASSERTION
	 * @model name="Assertion"
	 * @generated
	 * @ordered
	 */
	public static final int ASSERTION_VALUE = 2;

	/**
	 * The '<em><b>Offer</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #OFFER
	 * @model name="Offer"
	 * @generated
	 * @ordered
	 */
	public static final int OFFER_VALUE = 3;

	/**
	 * The '<em><b>Privacy</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PRIVACY
	 * @model name="Privacy"
	 * @generated
	 * @ordered
	 */
	public static final int PRIVACY_VALUE = 4;

	/**
	 * The '<em><b>Request</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #REQUEST
	 * @model name="Request"
	 * @generated
	 * @ordered
	 */
	public static final int REQUEST_VALUE = 5;

	/**
	 * The '<em><b>Set</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SET
	 * @model name="Set"
	 * @generated
	 * @ordered
	 */
	public static final int SET_VALUE = 6;

	/**
	 * The '<em><b>Ticket</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TICKET
	 * @model name="Ticket"
	 * @generated
	 * @ordered
	 */
	public static final int TICKET_VALUE = 7;

	/**
	 * An array of all the '<em><b>Policy Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final PolicyType[] VALUES_ARRAY =
		new PolicyType[] {
			NULL,
			AGREEMENT,
			ASSERTION,
			OFFER,
			PRIVACY,
			REQUEST,
			SET,
			TICKET,
		};

	/**
	 * A public read-only list of all the '<em><b>Policy Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List<PolicyType> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Policy Type</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param literal the literal.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static PolicyType get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			PolicyType result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Policy Type</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param name the name.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static PolicyType getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			PolicyType result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Policy Type</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the integer value.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static PolicyType get(int value) {
		switch (value) {
			case NULL_VALUE: return NULL;
			case AGREEMENT_VALUE: return AGREEMENT;
			case ASSERTION_VALUE: return ASSERTION;
			case OFFER_VALUE: return OFFER;
			case PRIVACY_VALUE: return PRIVACY;
			case REQUEST_VALUE: return REQUEST;
			case SET_VALUE: return SET;
			case TICKET_VALUE: return TICKET;
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
	private PolicyType(int value, String name, String literal) {
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
	
} //PolicyType
