package carisma.evolution.uml2.umlchange;

/**
 * This class holds regular expressions describing the UMLchange syntax.
 * They can be used to validate the UMLchange grammar.
 * @author dwarzecha
 *
 */
public final class UMLchangeSyntax {
    
    /**
     * String representing closing parenthesis.
     */
    private static final String CLOSING_PARENTHESIS = ")*$";
	
    /**
     * String representing opening Curly brackets '{'.
     */
    private static final String OPEN_C_BRACETS = "\\{";
    
    /**
     * String representing closing Curly brackets '{'.
     */
    private static final String CLOSE_C_BRACETS = "\\}";
            
	/**
	 * An UMLchange refID.
	 * Examples:
	 * - someRef
	 * - changeThis
	 * - addThat
	 */
	public static final String REGEX_REFID = "[a-zA-Z]+";
	
	/**
	 * Shorthand for the ref id followed by an equals sign.
	 * Examples:
	 * - someRef=
	 * - changeThis=
	 * - addThat=
	 */
	public static final String REGEX_REFID_PREFIX = REGEX_REFID + "\\=";
	
	/**
	 * A stereotype name.
	 * Examples:
	 * - critical
	 * - secure dependency
	 * - authorized-status
	 */
	public static final String REGEX_STEREOTYPE = "[a-zA-Z\\- ]+";
	
	/**
	 * A tag name.
	 * Examples:
	 * - high
	 * - adversary
	 * - permission
	 */
	public static final String REGEX_TAG = "[a-zA-Z]+";
	
	/**
	 * An ext tag entry.
	 * Examples:
	 * - someRef=critical
	 * - changeThis=secure links.adversary
	 * - addThat=authorized-status.permission
	 */
	public static final String REGEX_EXT_VALUE = "^" + REGEX_REFID_PREFIX + REGEX_STEREOTYPE + "(\\." + REGEX_TAG + "){0,1}$";
	
	/**
	 * An UML metaclass name.
	 * Examples:
	 * - Stereotype
	 * - Class
	 * - Operation
	 */
	public static final String REGEX_METACLASS = "[a-zA-Z]+";
	
	/**
	 * An element name.
	 * Examples:
	 * - Class1
	 * - Some_Element
	 * - The_1stElement
	 */
	public static final String REGEX_ELEMENTNAME = "\\w+";
	
	/**
	 * A qualified element name.
	 * Examples:
	 * - Some::Class::I::Think::Of
	 * - There::It::Is234
	 * - something::in_this_package
	 */
	public static final String REGEX_QUALIFIED_NAME = REGEX_ELEMENTNAME + "(::" + REGEX_ELEMENTNAME + ")*";
	
	/**
	 * An attribute key.
	 * Examples:
	 * - name
	 * - source
	 * - value
	 */
	public static final String REGEX_KEY = "[a-zA-Z]+(?<!(^|[,\\(])contents)";
	
	/**
	 * The value of an attribute.
	 * Examples:
	 * ???
	 */
	//FIXME: WRONG! Even now
	public static final String REGEX_VALUE = ".+";
	
	/**
	 * The special contents key-value pair.
	 * Examples:
	 * - contents=<Stereotype(name=critical)>
	 * - contents=<Class(name=SomeClass,contents=<Stereotype(name=secure dependency,contents=<Property(name=adversary)>)>)> 
	 */
	public static final String REGEX_CONTENTS_PAIR = "contents\\=<(.*)>";
	
	/**
	 * An UMLchange key-value-pair.
	 * Examples:
	 * - contents=<Class(name=SomeClass,contents=<Stereotype(name=secure dependency,contents=<Property(name=adversary)>)>)>
	 * - name=SomeClass
	 */
	public static final String REGEX_KEYVALUEPAIR = "(" + REGEX_CONTENTS_PAIR + "|" + REGEX_KEY + "\\=" + REGEX_VALUE + ")";
	/**
	 * They key-value-pair collection of a Simple Element Description.
	 * Examples:
	 * - (name=SomeDependency,source=There::This::Class,target=And::There2,contents=<Stereotype(name=high)>)
	 */
	public static final String REGEX_SED_PAIRS = "\\(" + "(" + REGEX_KEYVALUEPAIR + ")+" + "\\)";
	/**
	 * A full Simple Element Description describing a new model element.
	 * Examples:
	 * - Stereotype(name=critical)
	 * - Class(name=SomeClass,contents=<Stereotype(name=secure dependency,contents=<Property(name=adversary,value=custom)>)>)
	 */
	public static final String REGEX_SED_FULL = REGEX_METACLASS + REGEX_SED_PAIRS;
	
	/**
	 * A namespace description for describing complex changes.
	 * Examples:
	 * - @ someNamespace (minus the whitespace)
	 */
	public static final String REGEX_NAMESPACE_DESCRIPTION = "@\\w+";
	
	/**
	 * An alternative for the new tag.
	 * Examples:
	 * - {Stereotype(name=secrecy),Stereotype(name=integrity)}
	 * - {Class(name=SomeClass,contents=<Stereotype(name=secure dependency,contents=<Property(name=adversary,value=custom)>)>)}
	 * - {@someNamespace},{@otherNamespace}
	 * - {@someNamespace},{Stereotype(name=secrecy),Stereotype(name=integrity)}
	 */
	public static final String REGEX_NEW_ALTERNATIVE = "\\{(" + REGEX_SED_FULL + "(," + REGEX_SED_FULL + ")*" + "|" + REGEX_NAMESPACE_DESCRIPTION + ")\\}";
	
	/**
	 * A complete value for the new tag.
	 * Examples:
	 * - someRef={Stereotype(name=secrecy),Stereotype(name=integrity)},{Class(name=SomeClass)}
	 */
	public static final String REGEX_NEW_VALUE = "^" + REGEX_REFID_PREFIX + REGEX_NEW_ALTERNATIVE + "(," + REGEX_NEW_ALTERNATIVE + CLOSING_PARENTHESIS;
	
	/**
	 * A target description for the to tag, with and without changing some values.
	 * Examples:
	 * - move::this::here(change=that)
	 * - copy::this::there
	 */
	public static final String REGEX_TO_TARGET = REGEX_QUALIFIED_NAME + "(" + REGEX_SED_PAIRS + "){0,1}";
	
	/**
	 * An alternative for the <<move>> to tag. Only one target in the alternative allowed.
	 * Examples:
	 * - {move::this::here(change=that)}
	 * - {move::this::there}
	 */
	public static final String REGEX_MOVE_TO_ALTERNATIVE = OPEN_C_BRACETS + REGEX_TO_TARGET + CLOSE_C_BRACETS;
	
	/**
	 * An alternative for the <<copy>> to tag. Multiple targets in one alternative allowed.
	 * Examples:
	 * - {copy::this::here(change=that)}
	 * - {copy::this::there}
	 * - {copy::this::here(change=that),copy::this:also::here}
	 */
	public static final String REGEX_COPY_TO_ALTERNATIVE = OPEN_C_BRACETS + REGEX_TO_TARGET + "(," + REGEX_TO_TARGET + ")*" + CLOSE_C_BRACETS;
	
	/**
	 * A complete value for the <<move>> to tag.
	 * Examples:
	 * - someRef={move::this::here(change=that)}
	 * - altMoves={move::this::there},{or::move::it::here}
	 */
	public static final String REGEX_MOVE_TO_VALUE = "^" + REGEX_REFID_PREFIX + REGEX_MOVE_TO_ALTERNATIVE + "(," + REGEX_MOVE_TO_ALTERNATIVE + CLOSING_PARENTHESIS;
	
	/**
	 * A complete value for the <<copy>> to tag.
	 * Examples:
	 * - someRef={copy::this::here(change=that),and::to::this::package}
	 * - altMoves={copy::this::there},{or::copy::it::here}
	 */
	public static final String REGEX_COPY_TO_VALUE = "^" + REGEX_REFID_PREFIX + REGEX_COPY_TO_ALTERNATIVE + "(," + REGEX_COPY_TO_ALTERNATIVE + CLOSING_PARENTHESIS;
	
	/**
	 * An alternative in the values tag.
	 * Examples:
	 * - {(change=this,and=that)}
	 */
	public static final String REGEX_VALUES_ALTERNATIVE = OPEN_C_BRACETS + REGEX_SED_PAIRS + CLOSE_C_BRACETS;
	
	/**
	 * A complete value for the <<edit>> values tag.
	 * Examples:
	 * - editThisThing={(change=this)},{(or=this)}
	 */
	public static final String REGEX_VALUES_VALUE = "^" + REGEX_REFID_PREFIX + REGEX_VALUES_ALTERNATIVE + "(," + REGEX_VALUES_ALTERNATIVE + CLOSING_PARENTHESIS;
	
	/**
	 * A simple element description in a pattern tag entry. Can omit the key value pairs.
	 * Examples:
	 * - Stereotype(name=critical)
	 * - Class(contents=<Stereotype>)
	 */
	public static final String REGEX_SED_PATTERN = REGEX_METACLASS + "(" + REGEX_SED_PAIRS + "){0,1}";
	/**
	 * An entry in the pattern tag. Examples see REGEX_SED_PATTERN.
	 */
	public static final String REGEX_PATTERN_VALUE = "^" + REGEX_REFID_PREFIX + REGEX_SED_PATTERN + "$";
	
	/**
	 * An alternative of the adopter tag can be empty or contains a simple element description.
	 * Examples:
	 * - {}
	 * - {Class(name=SomeClass)} 
	 */
	public static final String REGEX_ADOPTER_ALTERNATIVE = OPEN_C_BRACETS + "(" + REGEX_SED_FULL + "){0,1}" + CLOSE_C_BRACETS;
	
	/**
	 * An entry of the adopter tag.
	 * Examples:
	 * refId={},{Class(name=SomeClass)}
	 * keepThis={Dependency(name=receivingDep)}
	 */
	public static final String REGEX_ADOPTER_VALUE = "^" + REGEX_REFID_PREFIX + REGEX_ADOPTER_ALTERNATIVE + "(," + REGEX_ADOPTER_ALTERNATIVE + CLOSING_PARENTHESIS;
	
	/**
	 * The constraint types allowed in UMLchange constraints.
	 */
	public static final String REGEX_CONSTRAINT_TYPE = "(AND|NOT|REQ)";
	
	/**
	 * An UMLchange constraint.
	 * Examples:
	 * - AND(thisChange)
	 * - OR(thatChange)
	 */
	public static final String REGEX_CONSTRAINT = REGEX_CONSTRAINT_TYPE + "\\(" + REGEX_REFID + "\\)";
	
	/**
	 * Shorthand to match the referenced change id of a constraint.
	 * Examples:
	 * - AND(thisChange) match-group: thisChange
	 */
	public static final String REGEX_CONSTRAINT_REFERENCE = "(?<=" + REGEX_CONSTRAINT_TYPE + "\\()" + REGEX_REFID + "(?=\\))";
	
	/**
	 * Shorthand to match the namespaces in a tag.
	 * Examples:
	 * - {@ namespace} match-group: namespace (whitout the whitespace after the @)
	 */
	public static final String REGEX_NAMESPACE_REFERENCE = "(?<=\\{@)\\w+(?=\\})";
	/**
	 * A value of the constraint tag.
	 * Examples:
	 * - constrainThis=AND(onlyWithThat),OR(chooseThis)
	 */
	public static final String REGEX_CONSTRAINT_VALUE = "^" + REGEX_REFID_PREFIX + REGEX_CONSTRAINT + "(," + REGEX_CONSTRAINT + CLOSING_PARENTHESIS;
	
	/** 
	 * Private constructor. UMLchangeSyntax never needs to be initialized.
	 */
	private UMLchangeSyntax() {
		
	}
}
