/**
 */
package ODRLCommonVocabulary;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Enumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Left Operand</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getLeftOperand()
 * @model
 * @generated
 */
public enum LeftOperand implements Enumerator {
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
	 * The '<em><b>Absolute Size</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ABSOLUTE_SIZE_VALUE
	 * @generated
	 * @ordered
	 */
	ABSOLUTE_SIZE(1, "absoluteSize", "absoluteSize"),

	/**
	 * The '<em><b>Absolute Spartial Position</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ABSOLUTE_SPARTIAL_POSITION_VALUE
	 * @generated
	 * @ordered
	 */
	ABSOLUTE_SPARTIAL_POSITION(2, "absoluteSpartialPosition", "absoluteSpartialPosition"),

	/**
	 * The '<em><b>Absolute Temporal Position</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ABSOLUTE_TEMPORAL_POSITION_VALUE
	 * @generated
	 * @ordered
	 */
	ABSOLUTE_TEMPORAL_POSITION(3, "absoluteTemporalPosition", "absoluteTemporalPosition"),

	/**
	 * The '<em><b>Count</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #COUNT_VALUE
	 * @generated
	 * @ordered
	 */
	COUNT(4, "count", "count"),

	/**
	 * The '<em><b>Date Time</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #DATE_TIME_VALUE
	 * @generated
	 * @ordered
	 */
	DATE_TIME(5, "dateTime", "dateTime"),

	/**
	 * The '<em><b>Delay Period</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #DELAY_PERIOD_VALUE
	 * @generated
	 * @ordered
	 */
	DELAY_PERIOD(6, "delayPeriod", "delayPeriod"),

	/**
	 * The '<em><b>Delivery Channel</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #DELIVERY_CHANNEL_VALUE
	 * @generated
	 * @ordered
	 */
	DELIVERY_CHANNEL(7, "deliveryChannel", "deliveryChannel"),

	/**
	 * The '<em><b>Device</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #DEVICE_VALUE
	 * @generated
	 * @ordered
	 */
	DEVICE(8, "device", "device"),

	/**
	 * The '<em><b>Elapsed Time</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ELAPSED_TIME_VALUE
	 * @generated
	 * @ordered
	 */
	ELAPSED_TIME(9, "elapsedTime", "elapsedTime"),

	/**
	 * The '<em><b>Event</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #EVENT_VALUE
	 * @generated
	 * @ordered
	 */
	EVENT(10, "event", "event"),

	/**
	 * The '<em><b>File Format</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FILE_FORMAT_VALUE
	 * @generated
	 * @ordered
	 */
	FILE_FORMAT(11, "fileFormat", "fileFormat"),

	/**
	 * The '<em><b>Industry</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #INDUSTRY_VALUE
	 * @generated
	 * @ordered
	 */
	INDUSTRY(12, "industry", "industry"),

	/**
	 * The '<em><b>Language</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #LANGUAGE_VALUE
	 * @generated
	 * @ordered
	 */
	LANGUAGE(13, "language", "language"),

	/**
	 * The '<em><b>Media</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #MEDIA_VALUE
	 * @generated
	 * @ordered
	 */
	MEDIA(14, "media", "media"),

	/**
	 * The '<em><b>Metered Time</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #METERED_TIME_VALUE
	 * @generated
	 * @ordered
	 */
	METERED_TIME(15, "meteredTime", "meteredTime"),

	/**
	 * The '<em><b>Pay Amount</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PAY_AMOUNT_VALUE
	 * @generated
	 * @ordered
	 */
	PAY_AMOUNT(16, "payAmount", "payAmount"),

	/**
	 * The '<em><b>Percentage</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PERCENTAGE_VALUE
	 * @generated
	 * @ordered
	 */
	PERCENTAGE(17, "percentage", "percentage"),

	/**
	 * The '<em><b>Product</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PRODUCT_VALUE
	 * @generated
	 * @ordered
	 */
	PRODUCT(18, "product", "product"),

	/**
	 * The '<em><b>Purpose</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PURPOSE_VALUE
	 * @generated
	 * @ordered
	 */
	PURPOSE(19, "purpose", "purpose"),

	/**
	 * The '<em><b>Recipient</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RECIPIENT_VALUE
	 * @generated
	 * @ordered
	 */
	RECIPIENT(20, "recipient", "recipient"),

	/**
	 * The '<em><b>Relative Position</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RELATIVE_POSITION_VALUE
	 * @generated
	 * @ordered
	 */
	RELATIVE_POSITION(21, "relativePosition", "relativePosition"),

	/**
	 * The '<em><b>Relative Size</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RELATIVE_SIZE_VALUE
	 * @generated
	 * @ordered
	 */
	RELATIVE_SIZE(22, "relativeSize", "relativeSize"),

	/**
	 * The '<em><b>Relative Spartial Position</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RELATIVE_SPARTIAL_POSITION_VALUE
	 * @generated
	 * @ordered
	 */
	RELATIVE_SPARTIAL_POSITION(23, "relativeSpartialPosition", "relativeSpartialPosition"),

	/**
	 * The '<em><b>Relative Temporal Position</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RELATIVE_TEMPORAL_POSITION_VALUE
	 * @generated
	 * @ordered
	 */
	RELATIVE_TEMPORAL_POSITION(24, "relativeTemporalPosition", "relativeTemporalPosition"),

	/**
	 * The '<em><b>Resolution</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RESOLUTION_VALUE
	 * @generated
	 * @ordered
	 */
	RESOLUTION(25, "resolution", "resolution"),

	/**
	 * The '<em><b>Spartial</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SPARTIAL_VALUE
	 * @generated
	 * @ordered
	 */
	SPARTIAL(26, "spartial", "spartial"),

	/**
	 * The '<em><b>Spartial Coordinates</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SPARTIAL_COORDINATES_VALUE
	 * @generated
	 * @ordered
	 */
	SPARTIAL_COORDINATES(27, "spartialCoordinates", "spartialCoordinates"),

	/**
	 * The '<em><b>System</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SYSTEM_VALUE
	 * @generated
	 * @ordered
	 */
	SYSTEM(28, "system", "system"),

	/**
	 * The '<em><b>System Device</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SYSTEM_DEVICE_VALUE
	 * @generated
	 * @ordered
	 */
	SYSTEM_DEVICE(29, "systemDevice", "systemDevice"),

	/**
	 * The '<em><b>Time Interval</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TIME_INTERVAL_VALUE
	 * @generated
	 * @ordered
	 */
	TIME_INTERVAL(30, "timeInterval", "timeInterval"),

	/**
	 * The '<em><b>Unit Of Count</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #UNIT_OF_COUNT_VALUE
	 * @generated
	 * @ordered
	 */
	UNIT_OF_COUNT(31, "unitOfCount", "unitOfCount"),

	/**
	 * The '<em><b>Version</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VERSION_VALUE
	 * @generated
	 * @ordered
	 */
	VERSION(32, "version", "version"),

	/**
	 * The '<em><b>Virtual Location</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VIRTUAL_LOCATION_VALUE
	 * @generated
	 * @ordered
	 */
	VIRTUAL_LOCATION(33, "virtualLocation", "virtualLocation");

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
	 * The '<em><b>Absolute Size</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ABSOLUTE_SIZE
	 * @model name="absoluteSize"
	 * @generated
	 * @ordered
	 */
	public static final int ABSOLUTE_SIZE_VALUE = 1;

	/**
	 * The '<em><b>Absolute Spartial Position</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ABSOLUTE_SPARTIAL_POSITION
	 * @model name="absoluteSpartialPosition"
	 * @generated
	 * @ordered
	 */
	public static final int ABSOLUTE_SPARTIAL_POSITION_VALUE = 2;

	/**
	 * The '<em><b>Absolute Temporal Position</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ABSOLUTE_TEMPORAL_POSITION
	 * @model name="absoluteTemporalPosition"
	 * @generated
	 * @ordered
	 */
	public static final int ABSOLUTE_TEMPORAL_POSITION_VALUE = 3;

	/**
	 * The '<em><b>Count</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #COUNT
	 * @model name="count"
	 * @generated
	 * @ordered
	 */
	public static final int COUNT_VALUE = 4;

	/**
	 * The '<em><b>Date Time</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #DATE_TIME
	 * @model name="dateTime"
	 * @generated
	 * @ordered
	 */
	public static final int DATE_TIME_VALUE = 5;

	/**
	 * The '<em><b>Delay Period</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #DELAY_PERIOD
	 * @model name="delayPeriod"
	 * @generated
	 * @ordered
	 */
	public static final int DELAY_PERIOD_VALUE = 6;

	/**
	 * The '<em><b>Delivery Channel</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #DELIVERY_CHANNEL
	 * @model name="deliveryChannel"
	 * @generated
	 * @ordered
	 */
	public static final int DELIVERY_CHANNEL_VALUE = 7;

	/**
	 * The '<em><b>Device</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #DEVICE
	 * @model name="device"
	 * @generated
	 * @ordered
	 */
	public static final int DEVICE_VALUE = 8;

	/**
	 * The '<em><b>Elapsed Time</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #ELAPSED_TIME
	 * @model name="elapsedTime"
	 * @generated
	 * @ordered
	 */
	public static final int ELAPSED_TIME_VALUE = 9;

	/**
	 * The '<em><b>Event</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #EVENT
	 * @model name="event"
	 * @generated
	 * @ordered
	 */
	public static final int EVENT_VALUE = 10;

	/**
	 * The '<em><b>File Format</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #FILE_FORMAT
	 * @model name="fileFormat"
	 * @generated
	 * @ordered
	 */
	public static final int FILE_FORMAT_VALUE = 11;

	/**
	 * The '<em><b>Industry</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #INDUSTRY
	 * @model name="industry"
	 * @generated
	 * @ordered
	 */
	public static final int INDUSTRY_VALUE = 12;

	/**
	 * The '<em><b>Language</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #LANGUAGE
	 * @model name="language"
	 * @generated
	 * @ordered
	 */
	public static final int LANGUAGE_VALUE = 13;

	/**
	 * The '<em><b>Media</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #MEDIA
	 * @model name="media"
	 * @generated
	 * @ordered
	 */
	public static final int MEDIA_VALUE = 14;

	/**
	 * The '<em><b>Metered Time</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #METERED_TIME
	 * @model name="meteredTime"
	 * @generated
	 * @ordered
	 */
	public static final int METERED_TIME_VALUE = 15;

	/**
	 * The '<em><b>Pay Amount</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PAY_AMOUNT
	 * @model name="payAmount"
	 * @generated
	 * @ordered
	 */
	public static final int PAY_AMOUNT_VALUE = 16;

	/**
	 * The '<em><b>Percentage</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PERCENTAGE
	 * @model name="percentage"
	 * @generated
	 * @ordered
	 */
	public static final int PERCENTAGE_VALUE = 17;

	/**
	 * The '<em><b>Product</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PRODUCT
	 * @model name="product"
	 * @generated
	 * @ordered
	 */
	public static final int PRODUCT_VALUE = 18;

	/**
	 * The '<em><b>Purpose</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #PURPOSE
	 * @model name="purpose"
	 * @generated
	 * @ordered
	 */
	public static final int PURPOSE_VALUE = 19;

	/**
	 * The '<em><b>Recipient</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RECIPIENT
	 * @model name="recipient"
	 * @generated
	 * @ordered
	 */
	public static final int RECIPIENT_VALUE = 20;

	/**
	 * The '<em><b>Relative Position</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RELATIVE_POSITION
	 * @model name="relativePosition"
	 * @generated
	 * @ordered
	 */
	public static final int RELATIVE_POSITION_VALUE = 21;

	/**
	 * The '<em><b>Relative Size</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RELATIVE_SIZE
	 * @model name="relativeSize"
	 * @generated
	 * @ordered
	 */
	public static final int RELATIVE_SIZE_VALUE = 22;

	/**
	 * The '<em><b>Relative Spartial Position</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RELATIVE_SPARTIAL_POSITION
	 * @model name="relativeSpartialPosition"
	 * @generated
	 * @ordered
	 */
	public static final int RELATIVE_SPARTIAL_POSITION_VALUE = 23;

	/**
	 * The '<em><b>Relative Temporal Position</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RELATIVE_TEMPORAL_POSITION
	 * @model name="relativeTemporalPosition"
	 * @generated
	 * @ordered
	 */
	public static final int RELATIVE_TEMPORAL_POSITION_VALUE = 24;

	/**
	 * The '<em><b>Resolution</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #RESOLUTION
	 * @model name="resolution"
	 * @generated
	 * @ordered
	 */
	public static final int RESOLUTION_VALUE = 25;

	/**
	 * The '<em><b>Spartial</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SPARTIAL
	 * @model name="spartial"
	 * @generated
	 * @ordered
	 */
	public static final int SPARTIAL_VALUE = 26;

	/**
	 * The '<em><b>Spartial Coordinates</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SPARTIAL_COORDINATES
	 * @model name="spartialCoordinates"
	 * @generated
	 * @ordered
	 */
	public static final int SPARTIAL_COORDINATES_VALUE = 27;

	/**
	 * The '<em><b>System</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SYSTEM
	 * @model name="system"
	 * @generated
	 * @ordered
	 */
	public static final int SYSTEM_VALUE = 28;

	/**
	 * The '<em><b>System Device</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SYSTEM_DEVICE
	 * @model name="systemDevice"
	 * @generated
	 * @ordered
	 */
	public static final int SYSTEM_DEVICE_VALUE = 29;

	/**
	 * The '<em><b>Time Interval</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #TIME_INTERVAL
	 * @model name="timeInterval"
	 * @generated
	 * @ordered
	 */
	public static final int TIME_INTERVAL_VALUE = 30;

	/**
	 * The '<em><b>Unit Of Count</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #UNIT_OF_COUNT
	 * @model name="unitOfCount"
	 * @generated
	 * @ordered
	 */
	public static final int UNIT_OF_COUNT_VALUE = 31;

	/**
	 * The '<em><b>Version</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VERSION
	 * @model name="version"
	 * @generated
	 * @ordered
	 */
	public static final int VERSION_VALUE = 32;

	/**
	 * The '<em><b>Virtual Location</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #VIRTUAL_LOCATION
	 * @model name="virtualLocation"
	 * @generated
	 * @ordered
	 */
	public static final int VIRTUAL_LOCATION_VALUE = 33;

	/**
	 * An array of all the '<em><b>Left Operand</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final LeftOperand[] VALUES_ARRAY =
		new LeftOperand[] {
			NULL,
			ABSOLUTE_SIZE,
			ABSOLUTE_SPARTIAL_POSITION,
			ABSOLUTE_TEMPORAL_POSITION,
			COUNT,
			DATE_TIME,
			DELAY_PERIOD,
			DELIVERY_CHANNEL,
			DEVICE,
			ELAPSED_TIME,
			EVENT,
			FILE_FORMAT,
			INDUSTRY,
			LANGUAGE,
			MEDIA,
			METERED_TIME,
			PAY_AMOUNT,
			PERCENTAGE,
			PRODUCT,
			PURPOSE,
			RECIPIENT,
			RELATIVE_POSITION,
			RELATIVE_SIZE,
			RELATIVE_SPARTIAL_POSITION,
			RELATIVE_TEMPORAL_POSITION,
			RESOLUTION,
			SPARTIAL,
			SPARTIAL_COORDINATES,
			SYSTEM,
			SYSTEM_DEVICE,
			TIME_INTERVAL,
			UNIT_OF_COUNT,
			VERSION,
			VIRTUAL_LOCATION,
		};

	/**
	 * A public read-only list of all the '<em><b>Left Operand</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List<LeftOperand> VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Left Operand</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param literal the literal.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static LeftOperand get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			LeftOperand result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Left Operand</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param name the name.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static LeftOperand getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			LeftOperand result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Left Operand</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the integer value.
	 * @return the matching enumerator or <code>null</code>.
	 * @generated
	 */
	public static LeftOperand get(int value) {
		switch (value) {
			case NULL_VALUE: return NULL;
			case ABSOLUTE_SIZE_VALUE: return ABSOLUTE_SIZE;
			case ABSOLUTE_SPARTIAL_POSITION_VALUE: return ABSOLUTE_SPARTIAL_POSITION;
			case ABSOLUTE_TEMPORAL_POSITION_VALUE: return ABSOLUTE_TEMPORAL_POSITION;
			case COUNT_VALUE: return COUNT;
			case DATE_TIME_VALUE: return DATE_TIME;
			case DELAY_PERIOD_VALUE: return DELAY_PERIOD;
			case DELIVERY_CHANNEL_VALUE: return DELIVERY_CHANNEL;
			case DEVICE_VALUE: return DEVICE;
			case ELAPSED_TIME_VALUE: return ELAPSED_TIME;
			case EVENT_VALUE: return EVENT;
			case FILE_FORMAT_VALUE: return FILE_FORMAT;
			case INDUSTRY_VALUE: return INDUSTRY;
			case LANGUAGE_VALUE: return LANGUAGE;
			case MEDIA_VALUE: return MEDIA;
			case METERED_TIME_VALUE: return METERED_TIME;
			case PAY_AMOUNT_VALUE: return PAY_AMOUNT;
			case PERCENTAGE_VALUE: return PERCENTAGE;
			case PRODUCT_VALUE: return PRODUCT;
			case PURPOSE_VALUE: return PURPOSE;
			case RECIPIENT_VALUE: return RECIPIENT;
			case RELATIVE_POSITION_VALUE: return RELATIVE_POSITION;
			case RELATIVE_SIZE_VALUE: return RELATIVE_SIZE;
			case RELATIVE_SPARTIAL_POSITION_VALUE: return RELATIVE_SPARTIAL_POSITION;
			case RELATIVE_TEMPORAL_POSITION_VALUE: return RELATIVE_TEMPORAL_POSITION;
			case RESOLUTION_VALUE: return RESOLUTION;
			case SPARTIAL_VALUE: return SPARTIAL;
			case SPARTIAL_COORDINATES_VALUE: return SPARTIAL_COORDINATES;
			case SYSTEM_VALUE: return SYSTEM;
			case SYSTEM_DEVICE_VALUE: return SYSTEM_DEVICE;
			case TIME_INTERVAL_VALUE: return TIME_INTERVAL;
			case UNIT_OF_COUNT_VALUE: return UNIT_OF_COUNT;
			case VERSION_VALUE: return VERSION;
			case VIRTUAL_LOCATION_VALUE: return VIRTUAL_LOCATION;
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
	private LeftOperand(int value, String name, String literal) {
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
	
} //LeftOperand
