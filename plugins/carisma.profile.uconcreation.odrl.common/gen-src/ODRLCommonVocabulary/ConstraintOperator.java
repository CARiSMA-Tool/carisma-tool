/**
 */
package ODRLCommonVocabulary;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Constraint Operator</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstraintOperator()
 * @model
 * @generated
 */
public enum ConstraintOperator implements Enumerator {
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
	 * The '<em><b>Eq</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #EQ_VALUE
	 * @generated
	 * @ordered
	 */
	EQ(1, "eq", "eq"),

	/**
	 * The '<em><b>Gt</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #GT_VALUE
	 * @generated
	 * @ordered
	 */
	GT(2, "gt", "gt"),

	/**
	 * The '<em><b>Gteq</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #GTEQ_VALUE
	 * @generated
	 * @ordered
	 */
	GTEQ(3, "gteq", "gteq"),

	/**
	 * The '<em><b>Has Part</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #HAS_PART_VALUE
	 * @generated
	 * @ordered
	 */
	HAS_PART(4, "hasPart", "hasPart"),

	/**
	 * The '<em><b>Is A</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #IS_A_VALUE
	 * @generated
	 * @ordered
	 */
	IS_A(5, "isA", "isA"),

	/**
	 * The '<em><b>Is All Of</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #IS_ALL_OF_VALUE
	 * @generated
	 * @ordered
	 */
	IS_ALL_OF(6, "isAllOf", "isAllOf"),

	/**
	 * The '<em><b>Is Any Of</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #IS_ANY_OF_VALUE
	 * @generated
	 * @ordered
	 */
	IS_ANY_OF(7, "isAnyOf", "isAnyOf"),

	/**
	 * The '<em><b>Is None Of</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #IS_NONE_OF_VALUE
	 * @generated
	 * @ordered
	 */
	IS_NONE_OF(8, "isNoneOf", "isNoneOf"),

	/**
	 * The '<em><b>Is Part Of</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #IS_PART_OF_VALUE
	 * @generated
	 * @ordered
	 */
	IS_PART_OF(9, "isPartOf", "isPartOf"),

	/**
	 * The '<em><b>Lt</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #LT_VALUE
	 * @generated
	 * @ordered
	 */
	LT(10, "lt", "lt"),

	/**
	 * The '<em><b>Lteq</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #LTEQ_VALUE
	 * @generated
	 * @ordered
	 */
	LTEQ(11, "lteq", "lteq"),

	/**
	 * The '<em><b>Neq</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #NEQ_VALUE
	 * @generated
	 * @ordered
	 */
	NEQ(12, "neq", "neq");

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
	 * The '<em><b>Eq</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #EQ
	 * @model name="eq"
	 * @generated
	 * @ordered
	 */
	public static final int EQ_VALUE = 1;

	/**
	 * The '<em><b>Gt</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #GT
	 * @model name="gt"
	 * @generated
	 * @ordered
	 */
	public static final int GT_VALUE = 2;

	/**
	 * The '<em><b>Gteq</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #GTEQ
	 * @model name="gteq"
	 * @generated
	 * @ordered
	 */
	public static final int GTEQ_VALUE = 3;

	/**
	 * The '<em><b>Has Part</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #HAS_PART
	 * @model name="hasPart"
	 * @generated
	 * @ordered
	 */
	public static final int HAS_PART_VALUE = 4;

	/**
	 * The '<em><b>Is A</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #IS_A
	 * @model name="isA"
	 * @generated
	 * @ordered
	 */
	public static final int IS_A_VALUE = 5;

	/**
	 * The '<em><b>Is All Of</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #IS_ALL_OF
	 * @model name="isAllOf"
	 * @generated
	 * @ordered
	 */
	public static final int IS_ALL_OF_VALUE = 6;

	/**
	 * The '<em><b>Is Any Of</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #IS_ANY_OF
	 * @model name="isAnyOf"
	 * @generated
	 * @ordered
	 */
	public static final int IS_ANY_OF_VALUE = 7;

	/**
	 * The '<em><b>Is None Of</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #IS_NONE_OF
	 * @model name="isNoneOf"
	 * @generated
	 * @ordered
	 */
	public static final int IS_NONE_OF_VALUE = 8;

	/**
	 * The '<em><b>Is Part Of</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #IS_PART_OF
	 * @model name="isPartOf"
	 * @generated
	 * @ordered
	 */
	public static final int IS_PART_OF_VALUE = 9;

	/**
	 * The '<em><b>Lt</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #LT
	 * @model name="lt"
	 * @generated
	 * @ordered
	 */
	public static final int LT_VALUE = 10;

	/**
	 * The '<em><b>Lteq</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #LTEQ
	 * @model name="lteq"
	 * @generated
	 * @ordered
	 */
	public static final int LTEQ_VALUE = 11;

	/**
	 * The '<em><b>Neq</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #NEQ
	 * @model name="neq"
	 * @generated
	 * @ordered
	 */
	public static final int NEQ_VALUE = 12;

	/**
	 * An array of all the '<em><b>Constraint Operator</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final ConstraintOperator[] VALUES_ARRAY =
		new ConstraintOperator[] {
			NULL,
			EQ,
			GT,
			GTEQ,
			HAS_PART,
			IS_A,
			IS_ALL_OF,
			IS_ANY_OF,
			IS_NONE_OF,
			IS_PART_OF,
			LT,
			LTEQ,
			NEQ,
		};

	/**
	 * A public read-only list of all the '<em><b>Constraint Operator</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List<ConstraintOperator> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Constraint Operator</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param literal the literal.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static ConstraintOperator get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			ConstraintOperator result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Constraint Operator</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param name the name.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static ConstraintOperator getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			ConstraintOperator result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Constraint Operator</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the integer value.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static ConstraintOperator get(int value) {
		switch (value) {
			case NULL_VALUE: return NULL;
			case EQ_VALUE: return EQ;
			case GT_VALUE: return GT;
			case GTEQ_VALUE: return GTEQ;
			case HAS_PART_VALUE: return HAS_PART;
			case IS_A_VALUE: return IS_A;
			case IS_ALL_OF_VALUE: return IS_ALL_OF;
			case IS_ANY_OF_VALUE: return IS_ANY_OF;
			case IS_NONE_OF_VALUE: return IS_NONE_OF;
			case IS_PART_OF_VALUE: return IS_PART_OF;
			case LT_VALUE: return LT;
			case LTEQ_VALUE: return LTEQ;
			case NEQ_VALUE: return NEQ;
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
	private ConstraintOperator(int value, String name, String literal) {
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
	
} //ConstraintOperator
