/**
 */
package UMLsec;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see UMLsec.UMLsecFactory
 * @model kind="package"
 * @generated
 */
public interface UMLsecPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "UMLsec";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.umlsec.de/profiles/UMLsec";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "UMLsec";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	UMLsecPackage eINSTANCE = UMLsec.impl.UMLsecPackageImpl.init();

	/**
	 * The meta object id for the '{@link UMLsec.impl.protectedactionImpl <em>protectedaction</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.protectedactionImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getprotectedaction()
	 * @generated
	 */
	int PROTECTEDACTION = 0;

	/**
	 * The feature id for the '<em><b>Permission</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROTECTEDACTION__PERMISSION = 0;

	/**
	 * The feature id for the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROTECTEDACTION__BASE_ACTION = 1;

	/**
	 * The feature id for the '<em><b>Base State</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROTECTEDACTION__BASE_STATE = 2;

	/**
	 * The number of structural features of the '<em>protectedaction</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROTECTEDACTION_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>protectedaction</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROTECTEDACTION_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.rbacImpl <em>rbac</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.rbacImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getrbac()
	 * @generated
	 */
	int RBAC = 1;

	/**
	 * The feature id for the '<em><b>Protectedactions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RBAC__PROTECTEDACTIONS = 0;

	/**
	 * The feature id for the '<em><b>Role</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RBAC__ROLE = 1;

	/**
	 * The feature id for the '<em><b>Right</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RBAC__RIGHT = 2;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RBAC__BASE_PACKAGE = 3;

	/**
	 * The number of structural features of the '<em>rbac</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RBAC_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>rbac</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int RBAC_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.SAPTransactionImpl <em>SAP Transaction</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.SAPTransactionImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getSAPTransaction()
	 * @generated
	 */
	int SAP_TRANSACTION = 2;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SAP_TRANSACTION__ID = 0;

	/**
	 * The feature id for the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SAP_TRANSACTION__BASE_ACTION = 1;

	/**
	 * The number of structural features of the '<em>SAP Transaction</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SAP_TRANSACTION_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>SAP Transaction</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SAP_TRANSACTION_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.allowedusersImpl <em>allowedusers</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.allowedusersImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getallowedusers()
	 * @generated
	 */
	int ALLOWEDUSERS = 3;

	/**
	 * The feature id for the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ALLOWEDUSERS__BASE_ACTION = 0;

	/**
	 * The feature id for the '<em><b>Users</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ALLOWEDUSERS__USERS = 1;

	/**
	 * The number of structural features of the '<em>allowedusers</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ALLOWEDUSERS_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>allowedusers</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ALLOWEDUSERS_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.seperationofdutyImpl <em>seperationofduty</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.seperationofdutyImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getseperationofduty()
	 * @generated
	 */
	int SEPERATIONOFDUTY = 4;

	/**
	 * The feature id for the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SEPERATIONOFDUTY__BASE_ACTION = 0;

	/**
	 * The feature id for the '<em><b>Activity</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SEPERATIONOFDUTY__ACTIVITY = 1;

	/**
	 * The number of structural features of the '<em>seperationofduty</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SEPERATIONOFDUTY_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>seperationofduty</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SEPERATIONOFDUTY_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.usedbyImpl <em>usedby</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.usedbyImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getusedby()
	 * @generated
	 */
	int USEDBY = 5;

	/**
	 * The feature id for the '<em><b>User</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USEDBY__USER = 0;

	/**
	 * The feature id for the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USEDBY__BASE_ACTION = 1;

	/**
	 * The feature id for the '<em><b>Base State</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USEDBY__BASE_STATE = 2;

	/**
	 * The number of structural features of the '<em>usedby</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USEDBY_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>usedby</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USEDBY_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.identifiableImpl <em>identifiable</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.identifiableImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getidentifiable()
	 * @generated
	 */
	int IDENTIFIABLE = 6;

	/**
	 * The feature id for the '<em><b>Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IDENTIFIABLE__ID = 0;

	/**
	 * The feature id for the '<em><b>Base Element</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IDENTIFIABLE__BASE_ELEMENT = 1;

	/**
	 * The number of structural features of the '<em>identifiable</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IDENTIFIABLE_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>identifiable</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int IDENTIFIABLE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.securedependencyImpl <em>securedependency</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.securedependencyImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getsecuredependency()
	 * @generated
	 */
	int SECUREDEPENDENCY = 7;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECUREDEPENDENCY__BASE_PACKAGE = 0;

	/**
	 * The number of structural features of the '<em>securedependency</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECUREDEPENDENCY_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>securedependency</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECUREDEPENDENCY_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.callImpl <em>call</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.callImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getcall()
	 * @generated
	 */
	int CALL = 8;

	/**
	 * The feature id for the '<em><b>Base Dependency</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CALL__BASE_DEPENDENCY = 0;

	/**
	 * The number of structural features of the '<em>call</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CALL_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>call</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CALL_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.sendImpl <em>send</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.sendImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getsend()
	 * @generated
	 */
	int SEND = 9;

	/**
	 * The feature id for the '<em><b>Base Dependency</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SEND__BASE_DEPENDENCY = 0;

	/**
	 * The number of structural features of the '<em>send</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SEND_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>send</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SEND_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.secrecyImpl <em>secrecy</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.secrecyImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getsecrecy()
	 * @generated
	 */
	int SECRECY = 10;

	/**
	 * The feature id for the '<em><b>Base Dependency</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECY__BASE_DEPENDENCY = 0;

	/**
	 * The feature id for the '<em><b>Base Connector</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECY__BASE_CONNECTOR = 1;

	/**
	 * The number of structural features of the '<em>secrecy</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECY_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>secrecy</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECY_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.integrityImpl <em>integrity</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.integrityImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getintegrity()
	 * @generated
	 */
	int INTEGRITY = 11;

	/**
	 * The feature id for the '<em><b>Base Dependency</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INTEGRITY__BASE_DEPENDENCY = 0;

	/**
	 * The feature id for the '<em><b>Base Connector</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INTEGRITY__BASE_CONNECTOR = 1;

	/**
	 * The number of structural features of the '<em>integrity</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INTEGRITY_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>integrity</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INTEGRITY_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.highImpl <em>high</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.highImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#gethigh()
	 * @generated
	 */
	int HIGH = 12;

	/**
	 * The feature id for the '<em><b>Base Dependency</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int HIGH__BASE_DEPENDENCY = 0;

	/**
	 * The feature id for the '<em><b>Base Connector</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int HIGH__BASE_CONNECTOR = 1;

	/**
	 * The number of structural features of the '<em>high</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int HIGH_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>high</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int HIGH_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.encryptedImpl <em>encrypted</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.encryptedImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getencrypted()
	 * @generated
	 */
	int ENCRYPTED = 13;

	/**
	 * The feature id for the '<em><b>Base Communication Path</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTED__BASE_COMMUNICATION_PATH = 0;

	/**
	 * The number of structural features of the '<em>encrypted</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTED_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>encrypted</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTED_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.wireImpl <em>wire</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.wireImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getwire()
	 * @generated
	 */
	int WIRE = 14;

	/**
	 * The feature id for the '<em><b>Base Communication Path</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WIRE__BASE_COMMUNICATION_PATH = 0;

	/**
	 * The number of structural features of the '<em>wire</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WIRE_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>wire</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int WIRE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.InternetImpl <em>Internet</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.InternetImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getInternet()
	 * @generated
	 */
	int INTERNET = 15;

	/**
	 * The feature id for the '<em><b>Base Communication Path</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INTERNET__BASE_COMMUNICATION_PATH = 0;

	/**
	 * The number of structural features of the '<em>Internet</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INTERNET_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Internet</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int INTERNET_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.LANImpl <em>LAN</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.LANImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getLAN()
	 * @generated
	 */
	int LAN = 16;

	/**
	 * The feature id for the '<em><b>Base Communication Path</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LAN__BASE_COMMUNICATION_PATH = 0;

	/**
	 * The feature id for the '<em><b>Base Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LAN__BASE_NODE = 1;

	/**
	 * The number of structural features of the '<em>LAN</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LAN_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>LAN</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LAN_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.smartcardImpl <em>smartcard</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.smartcardImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getsmartcard()
	 * @generated
	 */
	int SMARTCARD = 17;

	/**
	 * The feature id for the '<em><b>Base Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SMARTCARD__BASE_NODE = 0;

	/**
	 * The number of structural features of the '<em>smartcard</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SMARTCARD_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>smartcard</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SMARTCARD_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.POSdeviceImpl <em>PO Sdevice</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.POSdeviceImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getPOSdevice()
	 * @generated
	 */
	int PO_SDEVICE = 18;

	/**
	 * The feature id for the '<em><b>Base Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PO_SDEVICE__BASE_NODE = 0;

	/**
	 * The number of structural features of the '<em>PO Sdevice</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PO_SDEVICE_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>PO Sdevice</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PO_SDEVICE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.issuernodeImpl <em>issuernode</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.issuernodeImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getissuernode()
	 * @generated
	 */
	int ISSUERNODE = 19;

	/**
	 * The feature id for the '<em><b>Base Node</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ISSUERNODE__BASE_NODE = 0;

	/**
	 * The number of structural features of the '<em>issuernode</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ISSUERNODE_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>issuernode</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ISSUERNODE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.criticalImpl <em>critical</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.criticalImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getcritical()
	 * @generated
	 */
	int CRITICAL = 20;

	/**
	 * The feature id for the '<em><b>Secrecy</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL__SECRECY = 0;

	/**
	 * The feature id for the '<em><b>Integrity</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL__INTEGRITY = 1;

	/**
	 * The feature id for the '<em><b>High</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL__HIGH = 2;

	/**
	 * The feature id for the '<em><b>Base Class</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL__BASE_CLASS = 3;

	/**
	 * The feature id for the '<em><b>Base Component</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL__BASE_COMPONENT = 4;

	/**
	 * The feature id for the '<em><b>Fresh</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL__FRESH = 5;

	/**
	 * The feature id for the '<em><b>Authenticity</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL__AUTHENTICITY = 6;

	/**
	 * The feature id for the '<em><b>Base Instance Specification</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL__BASE_INSTANCE_SPECIFICATION = 7;

	/**
	 * The feature id for the '<em><b>Base Classifier</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL__BASE_CLASSIFIER = 8;

	/**
	 * The feature id for the '<em><b>Privacy</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL__PRIVACY = 9;

	/**
	 * The number of structural features of the '<em>critical</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL_FEATURE_COUNT = 10;

	/**
	 * The number of operations of the '<em>critical</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CRITICAL_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.datasecurityImpl <em>datasecurity</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.datasecurityImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getdatasecurity()
	 * @generated
	 */
	int DATASECURITY = 21;

	/**
	 * The feature id for the '<em><b>Adversary</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATASECURITY__ADVERSARY = 0;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATASECURITY__BASE_PACKAGE = 1;

	/**
	 * The feature id for the '<em><b>Authenticity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATASECURITY__AUTHENTICITY = 2;

	/**
	 * The feature id for the '<em><b>Integrity</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATASECURITY__INTEGRITY = 3;

	/**
	 * The number of structural features of the '<em>datasecurity</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATASECURITY_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>datasecurity</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATASECURITY_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.securelinksImpl <em>securelinks</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.securelinksImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getsecurelinks()
	 * @generated
	 */
	int SECURELINKS = 22;

	/**
	 * The feature id for the '<em><b>Adversary</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKS__ADVERSARY = 0;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKS__BASE_PACKAGE = 1;

	/**
	 * The number of structural features of the '<em>securelinks</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKS_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>securelinks</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKS_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.fairexchangeImpl <em>fairexchange</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.fairexchangeImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getfairexchange()
	 * @generated
	 */
	int FAIREXCHANGE = 23;

	/**
	 * The feature id for the '<em><b>Start</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FAIREXCHANGE__START = 0;

	/**
	 * The feature id for the '<em><b>Stop</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FAIREXCHANGE__STOP = 1;

	/**
	 * The feature id for the '<em><b>Adversary</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FAIREXCHANGE__ADVERSARY = 2;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FAIREXCHANGE__BASE_PACKAGE = 3;

	/**
	 * The number of structural features of the '<em>fairexchange</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FAIREXCHANGE_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>fairexchange</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int FAIREXCHANGE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.provableImpl <em>provable</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.provableImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getprovable()
	 * @generated
	 */
	int PROVABLE = 24;

	/**
	 * The feature id for the '<em><b>Action</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVABLE__ACTION = 0;

	/**
	 * The feature id for the '<em><b>Adversary</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVABLE__ADVERSARY = 1;

	/**
	 * The feature id for the '<em><b>Cert</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVABLE__CERT = 2;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVABLE__BASE_PACKAGE = 3;

	/**
	 * The number of structural features of the '<em>provable</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVABLE_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>provable</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVABLE_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.nodownflowImpl <em>nodownflow</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.nodownflowImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getnodownflow()
	 * @generated
	 */
	int NODOWNFLOW = 25;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NODOWNFLOW__BASE_PACKAGE = 0;

	/**
	 * The number of structural features of the '<em>nodownflow</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NODOWNFLOW_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>nodownflow</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NODOWNFLOW_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.noupflowImpl <em>noupflow</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.noupflowImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getnoupflow()
	 * @generated
	 */
	int NOUPFLOW = 26;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NOUPFLOW__BASE_PACKAGE = 0;

	/**
	 * The number of structural features of the '<em>noupflow</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NOUPFLOW_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>noupflow</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int NOUPFLOW_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.guardedaccessImpl <em>guardedaccess</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.guardedaccessImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getguardedaccess()
	 * @generated
	 */
	int GUARDEDACCESS = 27;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GUARDEDACCESS__BASE_PACKAGE = 0;

	/**
	 * The number of structural features of the '<em>guardedaccess</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GUARDEDACCESS_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>guardedaccess</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GUARDEDACCESS_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.guardedImpl <em>guarded</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.guardedImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getguarded()
	 * @generated
	 */
	int GUARDED = 28;

	/**
	 * The feature id for the '<em><b>Base Classifier</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GUARDED__BASE_CLASSIFIER = 0;

	/**
	 * The feature id for the '<em><b>Base Instance Specification</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GUARDED__BASE_INSTANCE_SPECIFICATION = 1;

	/**
	 * The feature id for the '<em><b>Guard</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GUARDED__GUARD = 2;

	/**
	 * The number of structural features of the '<em>guarded</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GUARDED_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>guarded</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int GUARDED_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.authorizedstatusImpl <em>authorizedstatus</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.authorizedstatusImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getauthorizedstatus()
	 * @generated
	 */
	int AUTHORIZEDSTATUS = 29;

	/**
	 * The feature id for the '<em><b>Permission</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AUTHORIZEDSTATUS__PERMISSION = 0;

	/**
	 * The feature id for the '<em><b>Base State</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AUTHORIZEDSTATUS__BASE_STATE = 1;

	/**
	 * The number of structural features of the '<em>authorizedstatus</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AUTHORIZEDSTATUS_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>authorizedstatus</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int AUTHORIZEDSTATUS_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.lockedstatusImpl <em>lockedstatus</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.lockedstatusImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getlockedstatus()
	 * @generated
	 */
	int LOCKEDSTATUS = 30;

	/**
	 * The feature id for the '<em><b>Base State</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOCKEDSTATUS__BASE_STATE = 0;

	/**
	 * The number of structural features of the '<em>lockedstatus</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOCKEDSTATUS_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>lockedstatus</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LOCKEDSTATUS_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.requiresImpl <em>requires</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.requiresImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getrequires()
	 * @generated
	 */
	int REQUIRES = 31;

	/**
	 * The feature id for the '<em><b>Base Action</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REQUIRES__BASE_ACTION = 0;

	/**
	 * The feature id for the '<em><b>Actions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REQUIRES__ACTIONS = 1;

	/**
	 * The number of structural features of the '<em>requires</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REQUIRES_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>requires</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int REQUIRES_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsec.impl.privacyImpl <em>privacy</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsec.impl.privacyImpl
	 * @see UMLsec.impl.UMLsecPackageImpl#getprivacy()
	 * @generated
	 */
	int PRIVACY = 32;

	/**
	 * The feature id for the '<em><b>Base Dependency</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PRIVACY__BASE_DEPENDENCY = 0;

	/**
	 * The feature id for the '<em><b>Base Connector</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PRIVACY__BASE_CONNECTOR = 1;

	/**
	 * The number of structural features of the '<em>privacy</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PRIVACY_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>privacy</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PRIVACY_OPERATION_COUNT = 0;


	/**
	 * Returns the meta object for class '{@link UMLsec.protectedaction <em>protectedaction</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>protectedaction</em>'.
	 * @see UMLsec.protectedaction
	 * @generated
	 */
	EClass getprotectedaction();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.protectedaction#getPermission <em>Permission</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Permission</em>'.
	 * @see UMLsec.protectedaction#getPermission()
	 * @see #getprotectedaction()
	 * @generated
	 */
	EAttribute getprotectedaction_Permission();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.protectedaction#getBase_Action <em>Base Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Action</em>'.
	 * @see UMLsec.protectedaction#getBase_Action()
	 * @see #getprotectedaction()
	 * @generated
	 */
	EReference getprotectedaction_Base_Action();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.protectedaction#getBase_State <em>Base State</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base State</em>'.
	 * @see UMLsec.protectedaction#getBase_State()
	 * @see #getprotectedaction()
	 * @generated
	 */
	EReference getprotectedaction_Base_State();

	/**
	 * Returns the meta object for class '{@link UMLsec.rbac <em>rbac</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>rbac</em>'.
	 * @see UMLsec.rbac
	 * @generated
	 */
	EClass getrbac();

	/**
	 * Returns the meta object for the reference list '{@link UMLsec.rbac#getProtectedactions <em>Protectedactions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Protectedactions</em>'.
	 * @see UMLsec.rbac#getProtectedactions()
	 * @see #getrbac()
	 * @generated
	 */
	EReference getrbac_Protectedactions();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.rbac#getRole <em>Role</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Role</em>'.
	 * @see UMLsec.rbac#getRole()
	 * @see #getrbac()
	 * @generated
	 */
	EAttribute getrbac_Role();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.rbac#getRight <em>Right</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Right</em>'.
	 * @see UMLsec.rbac#getRight()
	 * @see #getrbac()
	 * @generated
	 */
	EAttribute getrbac_Right();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.rbac#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see UMLsec.rbac#getBase_Package()
	 * @see #getrbac()
	 * @generated
	 */
	EReference getrbac_Base_Package();

	/**
	 * Returns the meta object for class '{@link UMLsec.SAPTransaction <em>SAP Transaction</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>SAP Transaction</em>'.
	 * @see UMLsec.SAPTransaction
	 * @generated
	 */
	EClass getSAPTransaction();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.SAPTransaction#getId <em>Id</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Id</em>'.
	 * @see UMLsec.SAPTransaction#getId()
	 * @see #getSAPTransaction()
	 * @generated
	 */
	EAttribute getSAPTransaction_Id();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.SAPTransaction#getBase_Action <em>Base Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Action</em>'.
	 * @see UMLsec.SAPTransaction#getBase_Action()
	 * @see #getSAPTransaction()
	 * @generated
	 */
	EReference getSAPTransaction_Base_Action();

	/**
	 * Returns the meta object for class '{@link UMLsec.allowedusers <em>allowedusers</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>allowedusers</em>'.
	 * @see UMLsec.allowedusers
	 * @generated
	 */
	EClass getallowedusers();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.allowedusers#getBase_Action <em>Base Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Action</em>'.
	 * @see UMLsec.allowedusers#getBase_Action()
	 * @see #getallowedusers()
	 * @generated
	 */
	EReference getallowedusers_Base_Action();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.allowedusers#getUsers <em>Users</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Users</em>'.
	 * @see UMLsec.allowedusers#getUsers()
	 * @see #getallowedusers()
	 * @generated
	 */
	EAttribute getallowedusers_Users();

	/**
	 * Returns the meta object for class '{@link UMLsec.seperationofduty <em>seperationofduty</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>seperationofduty</em>'.
	 * @see UMLsec.seperationofduty
	 * @generated
	 */
	EClass getseperationofduty();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.seperationofduty#getBase_Action <em>Base Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Action</em>'.
	 * @see UMLsec.seperationofduty#getBase_Action()
	 * @see #getseperationofduty()
	 * @generated
	 */
	EReference getseperationofduty_Base_Action();

	/**
	 * Returns the meta object for the reference list '{@link UMLsec.seperationofduty#getActivity <em>Activity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Activity</em>'.
	 * @see UMLsec.seperationofduty#getActivity()
	 * @see #getseperationofduty()
	 * @generated
	 */
	EReference getseperationofduty_Activity();

	/**
	 * Returns the meta object for class '{@link UMLsec.usedby <em>usedby</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>usedby</em>'.
	 * @see UMLsec.usedby
	 * @generated
	 */
	EClass getusedby();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.usedby#getUser <em>User</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>User</em>'.
	 * @see UMLsec.usedby#getUser()
	 * @see #getusedby()
	 * @generated
	 */
	EAttribute getusedby_User();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.usedby#getBase_Action <em>Base Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Action</em>'.
	 * @see UMLsec.usedby#getBase_Action()
	 * @see #getusedby()
	 * @generated
	 */
	EReference getusedby_Base_Action();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.usedby#getBase_State <em>Base State</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base State</em>'.
	 * @see UMLsec.usedby#getBase_State()
	 * @see #getusedby()
	 * @generated
	 */
	EReference getusedby_Base_State();

	/**
	 * Returns the meta object for class '{@link UMLsec.identifiable <em>identifiable</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>identifiable</em>'.
	 * @see UMLsec.identifiable
	 * @generated
	 */
	EClass getidentifiable();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.identifiable#getId <em>Id</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Id</em>'.
	 * @see UMLsec.identifiable#getId()
	 * @see #getidentifiable()
	 * @generated
	 */
	EAttribute getidentifiable_Id();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.identifiable#getBase_Element <em>Base Element</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Element</em>'.
	 * @see UMLsec.identifiable#getBase_Element()
	 * @see #getidentifiable()
	 * @generated
	 */
	EReference getidentifiable_Base_Element();

	/**
	 * Returns the meta object for class '{@link UMLsec.securedependency <em>securedependency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>securedependency</em>'.
	 * @see UMLsec.securedependency
	 * @generated
	 */
	EClass getsecuredependency();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.securedependency#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see UMLsec.securedependency#getBase_Package()
	 * @see #getsecuredependency()
	 * @generated
	 */
	EReference getsecuredependency_Base_Package();

	/**
	 * Returns the meta object for class '{@link UMLsec.call <em>call</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>call</em>'.
	 * @see UMLsec.call
	 * @generated
	 */
	EClass getcall();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.call#getBase_Dependency <em>Base Dependency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Dependency</em>'.
	 * @see UMLsec.call#getBase_Dependency()
	 * @see #getcall()
	 * @generated
	 */
	EReference getcall_Base_Dependency();

	/**
	 * Returns the meta object for class '{@link UMLsec.send <em>send</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>send</em>'.
	 * @see UMLsec.send
	 * @generated
	 */
	EClass getsend();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.send#getBase_Dependency <em>Base Dependency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Dependency</em>'.
	 * @see UMLsec.send#getBase_Dependency()
	 * @see #getsend()
	 * @generated
	 */
	EReference getsend_Base_Dependency();

	/**
	 * Returns the meta object for class '{@link UMLsec.secrecy <em>secrecy</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>secrecy</em>'.
	 * @see UMLsec.secrecy
	 * @generated
	 */
	EClass getsecrecy();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.secrecy#getBase_Dependency <em>Base Dependency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Dependency</em>'.
	 * @see UMLsec.secrecy#getBase_Dependency()
	 * @see #getsecrecy()
	 * @generated
	 */
	EReference getsecrecy_Base_Dependency();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.secrecy#getBase_Connector <em>Base Connector</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Connector</em>'.
	 * @see UMLsec.secrecy#getBase_Connector()
	 * @see #getsecrecy()
	 * @generated
	 */
	EReference getsecrecy_Base_Connector();

	/**
	 * Returns the meta object for class '{@link UMLsec.integrity <em>integrity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>integrity</em>'.
	 * @see UMLsec.integrity
	 * @generated
	 */
	EClass getintegrity();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.integrity#getBase_Dependency <em>Base Dependency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Dependency</em>'.
	 * @see UMLsec.integrity#getBase_Dependency()
	 * @see #getintegrity()
	 * @generated
	 */
	EReference getintegrity_Base_Dependency();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.integrity#getBase_Connector <em>Base Connector</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Connector</em>'.
	 * @see UMLsec.integrity#getBase_Connector()
	 * @see #getintegrity()
	 * @generated
	 */
	EReference getintegrity_Base_Connector();

	/**
	 * Returns the meta object for class '{@link UMLsec.high <em>high</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>high</em>'.
	 * @see UMLsec.high
	 * @generated
	 */
	EClass gethigh();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.high#getBase_Dependency <em>Base Dependency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Dependency</em>'.
	 * @see UMLsec.high#getBase_Dependency()
	 * @see #gethigh()
	 * @generated
	 */
	EReference gethigh_Base_Dependency();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.high#getBase_Connector <em>Base Connector</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Connector</em>'.
	 * @see UMLsec.high#getBase_Connector()
	 * @see #gethigh()
	 * @generated
	 */
	EReference gethigh_Base_Connector();

	/**
	 * Returns the meta object for class '{@link UMLsec.encrypted <em>encrypted</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>encrypted</em>'.
	 * @see UMLsec.encrypted
	 * @generated
	 */
	EClass getencrypted();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.encrypted#getBase_CommunicationPath <em>Base Communication Path</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Communication Path</em>'.
	 * @see UMLsec.encrypted#getBase_CommunicationPath()
	 * @see #getencrypted()
	 * @generated
	 */
	EReference getencrypted_Base_CommunicationPath();

	/**
	 * Returns the meta object for class '{@link UMLsec.wire <em>wire</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>wire</em>'.
	 * @see UMLsec.wire
	 * @generated
	 */
	EClass getwire();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.wire#getBase_CommunicationPath <em>Base Communication Path</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Communication Path</em>'.
	 * @see UMLsec.wire#getBase_CommunicationPath()
	 * @see #getwire()
	 * @generated
	 */
	EReference getwire_Base_CommunicationPath();

	/**
	 * Returns the meta object for class '{@link UMLsec.Internet <em>Internet</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Internet</em>'.
	 * @see UMLsec.Internet
	 * @generated
	 */
	EClass getInternet();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.Internet#getBase_CommunicationPath <em>Base Communication Path</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Communication Path</em>'.
	 * @see UMLsec.Internet#getBase_CommunicationPath()
	 * @see #getInternet()
	 * @generated
	 */
	EReference getInternet_Base_CommunicationPath();

	/**
	 * Returns the meta object for class '{@link UMLsec.LAN <em>LAN</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>LAN</em>'.
	 * @see UMLsec.LAN
	 * @generated
	 */
	EClass getLAN();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.LAN#getBase_CommunicationPath <em>Base Communication Path</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Communication Path</em>'.
	 * @see UMLsec.LAN#getBase_CommunicationPath()
	 * @see #getLAN()
	 * @generated
	 */
	EReference getLAN_Base_CommunicationPath();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.LAN#getBase_Node <em>Base Node</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Node</em>'.
	 * @see UMLsec.LAN#getBase_Node()
	 * @see #getLAN()
	 * @generated
	 */
	EReference getLAN_Base_Node();

	/**
	 * Returns the meta object for class '{@link UMLsec.smartcard <em>smartcard</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>smartcard</em>'.
	 * @see UMLsec.smartcard
	 * @generated
	 */
	EClass getsmartcard();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.smartcard#getBase_Node <em>Base Node</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Node</em>'.
	 * @see UMLsec.smartcard#getBase_Node()
	 * @see #getsmartcard()
	 * @generated
	 */
	EReference getsmartcard_Base_Node();

	/**
	 * Returns the meta object for class '{@link UMLsec.POSdevice <em>PO Sdevice</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>PO Sdevice</em>'.
	 * @see UMLsec.POSdevice
	 * @generated
	 */
	EClass getPOSdevice();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.POSdevice#getBase_Node <em>Base Node</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Node</em>'.
	 * @see UMLsec.POSdevice#getBase_Node()
	 * @see #getPOSdevice()
	 * @generated
	 */
	EReference getPOSdevice_Base_Node();

	/**
	 * Returns the meta object for class '{@link UMLsec.issuernode <em>issuernode</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>issuernode</em>'.
	 * @see UMLsec.issuernode
	 * @generated
	 */
	EClass getissuernode();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.issuernode#getBase_Node <em>Base Node</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Node</em>'.
	 * @see UMLsec.issuernode#getBase_Node()
	 * @see #getissuernode()
	 * @generated
	 */
	EReference getissuernode_Base_Node();

	/**
	 * Returns the meta object for class '{@link UMLsec.critical <em>critical</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>critical</em>'.
	 * @see UMLsec.critical
	 * @generated
	 */
	EClass getcritical();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.critical#getSecrecy <em>Secrecy</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Secrecy</em>'.
	 * @see UMLsec.critical#getSecrecy()
	 * @see #getcritical()
	 * @generated
	 */
	EAttribute getcritical_Secrecy();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.critical#getIntegrity <em>Integrity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Integrity</em>'.
	 * @see UMLsec.critical#getIntegrity()
	 * @see #getcritical()
	 * @generated
	 */
	EAttribute getcritical_Integrity();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.critical#getHigh <em>High</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>High</em>'.
	 * @see UMLsec.critical#getHigh()
	 * @see #getcritical()
	 * @generated
	 */
	EAttribute getcritical_High();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.critical#getBase_Class <em>Base Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Class</em>'.
	 * @see UMLsec.critical#getBase_Class()
	 * @see #getcritical()
	 * @generated
	 */
	EReference getcritical_Base_Class();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.critical#getBase_Component <em>Base Component</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Component</em>'.
	 * @see UMLsec.critical#getBase_Component()
	 * @see #getcritical()
	 * @generated
	 */
	EReference getcritical_Base_Component();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.critical#getFresh <em>Fresh</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Fresh</em>'.
	 * @see UMLsec.critical#getFresh()
	 * @see #getcritical()
	 * @generated
	 */
	EAttribute getcritical_Fresh();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.critical#getAuthenticity <em>Authenticity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Authenticity</em>'.
	 * @see UMLsec.critical#getAuthenticity()
	 * @see #getcritical()
	 * @generated
	 */
	EAttribute getcritical_Authenticity();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.critical#getBase_InstanceSpecification <em>Base Instance Specification</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Instance Specification</em>'.
	 * @see UMLsec.critical#getBase_InstanceSpecification()
	 * @see #getcritical()
	 * @generated
	 */
	EReference getcritical_Base_InstanceSpecification();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.critical#getBase_Classifier <em>Base Classifier</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Classifier</em>'.
	 * @see UMLsec.critical#getBase_Classifier()
	 * @see #getcritical()
	 * @generated
	 */
	EReference getcritical_Base_Classifier();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.critical#getPrivacy <em>Privacy</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Privacy</em>'.
	 * @see UMLsec.critical#getPrivacy()
	 * @see #getcritical()
	 * @generated
	 */
	EAttribute getcritical_Privacy();

	/**
	 * Returns the meta object for class '{@link UMLsec.datasecurity <em>datasecurity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>datasecurity</em>'.
	 * @see UMLsec.datasecurity
	 * @generated
	 */
	EClass getdatasecurity();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.datasecurity#getAdversary <em>Adversary</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Adversary</em>'.
	 * @see UMLsec.datasecurity#getAdversary()
	 * @see #getdatasecurity()
	 * @generated
	 */
	EAttribute getdatasecurity_Adversary();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.datasecurity#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see UMLsec.datasecurity#getBase_Package()
	 * @see #getdatasecurity()
	 * @generated
	 */
	EReference getdatasecurity_Base_Package();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.datasecurity#getAuthenticity <em>Authenticity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Authenticity</em>'.
	 * @see UMLsec.datasecurity#getAuthenticity()
	 * @see #getdatasecurity()
	 * @generated
	 */
	EAttribute getdatasecurity_Authenticity();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.datasecurity#getIntegrity <em>Integrity</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Integrity</em>'.
	 * @see UMLsec.datasecurity#getIntegrity()
	 * @see #getdatasecurity()
	 * @generated
	 */
	EAttribute getdatasecurity_Integrity();

	/**
	 * Returns the meta object for class '{@link UMLsec.securelinks <em>securelinks</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>securelinks</em>'.
	 * @see UMLsec.securelinks
	 * @generated
	 */
	EClass getsecurelinks();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.securelinks#getAdversary <em>Adversary</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Adversary</em>'.
	 * @see UMLsec.securelinks#getAdversary()
	 * @see #getsecurelinks()
	 * @generated
	 */
	EAttribute getsecurelinks_Adversary();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.securelinks#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see UMLsec.securelinks#getBase_Package()
	 * @see #getsecurelinks()
	 * @generated
	 */
	EReference getsecurelinks_Base_Package();

	/**
	 * Returns the meta object for class '{@link UMLsec.fairexchange <em>fairexchange</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>fairexchange</em>'.
	 * @see UMLsec.fairexchange
	 * @generated
	 */
	EClass getfairexchange();

	/**
	 * Returns the meta object for the reference list '{@link UMLsec.fairexchange#getStart <em>Start</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Start</em>'.
	 * @see UMLsec.fairexchange#getStart()
	 * @see #getfairexchange()
	 * @generated
	 */
	EReference getfairexchange_Start();

	/**
	 * Returns the meta object for the reference list '{@link UMLsec.fairexchange#getStop <em>Stop</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Stop</em>'.
	 * @see UMLsec.fairexchange#getStop()
	 * @see #getfairexchange()
	 * @generated
	 */
	EReference getfairexchange_Stop();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.fairexchange#getAdversary <em>Adversary</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Adversary</em>'.
	 * @see UMLsec.fairexchange#getAdversary()
	 * @see #getfairexchange()
	 * @generated
	 */
	EAttribute getfairexchange_Adversary();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.fairexchange#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see UMLsec.fairexchange#getBase_Package()
	 * @see #getfairexchange()
	 * @generated
	 */
	EReference getfairexchange_Base_Package();

	/**
	 * Returns the meta object for class '{@link UMLsec.provable <em>provable</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>provable</em>'.
	 * @see UMLsec.provable
	 * @generated
	 */
	EClass getprovable();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.provable#getAction <em>Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Action</em>'.
	 * @see UMLsec.provable#getAction()
	 * @see #getprovable()
	 * @generated
	 */
	EAttribute getprovable_Action();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.provable#getAdversary <em>Adversary</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Adversary</em>'.
	 * @see UMLsec.provable#getAdversary()
	 * @see #getprovable()
	 * @generated
	 */
	EAttribute getprovable_Adversary();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.provable#getCert <em>Cert</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Cert</em>'.
	 * @see UMLsec.provable#getCert()
	 * @see #getprovable()
	 * @generated
	 */
	EAttribute getprovable_Cert();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.provable#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see UMLsec.provable#getBase_Package()
	 * @see #getprovable()
	 * @generated
	 */
	EReference getprovable_Base_Package();

	/**
	 * Returns the meta object for class '{@link UMLsec.nodownflow <em>nodownflow</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>nodownflow</em>'.
	 * @see UMLsec.nodownflow
	 * @generated
	 */
	EClass getnodownflow();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.nodownflow#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see UMLsec.nodownflow#getBase_Package()
	 * @see #getnodownflow()
	 * @generated
	 */
	EReference getnodownflow_Base_Package();

	/**
	 * Returns the meta object for class '{@link UMLsec.noupflow <em>noupflow</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>noupflow</em>'.
	 * @see UMLsec.noupflow
	 * @generated
	 */
	EClass getnoupflow();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.noupflow#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see UMLsec.noupflow#getBase_Package()
	 * @see #getnoupflow()
	 * @generated
	 */
	EReference getnoupflow_Base_Package();

	/**
	 * Returns the meta object for class '{@link UMLsec.guardedaccess <em>guardedaccess</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>guardedaccess</em>'.
	 * @see UMLsec.guardedaccess
	 * @generated
	 */
	EClass getguardedaccess();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.guardedaccess#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see UMLsec.guardedaccess#getBase_Package()
	 * @see #getguardedaccess()
	 * @generated
	 */
	EReference getguardedaccess_Base_Package();

	/**
	 * Returns the meta object for class '{@link UMLsec.guarded <em>guarded</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>guarded</em>'.
	 * @see UMLsec.guarded
	 * @generated
	 */
	EClass getguarded();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.guarded#getBase_Classifier <em>Base Classifier</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Classifier</em>'.
	 * @see UMLsec.guarded#getBase_Classifier()
	 * @see #getguarded()
	 * @generated
	 */
	EReference getguarded_Base_Classifier();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.guarded#getBase_InstanceSpecification <em>Base Instance Specification</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Instance Specification</em>'.
	 * @see UMLsec.guarded#getBase_InstanceSpecification()
	 * @see #getguarded()
	 * @generated
	 */
	EReference getguarded_Base_InstanceSpecification();

	/**
	 * Returns the meta object for the attribute list '{@link UMLsec.guarded#getGuard <em>Guard</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Guard</em>'.
	 * @see UMLsec.guarded#getGuard()
	 * @see #getguarded()
	 * @generated
	 */
	EAttribute getguarded_Guard();

	/**
	 * Returns the meta object for class '{@link UMLsec.authorizedstatus <em>authorizedstatus</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>authorizedstatus</em>'.
	 * @see UMLsec.authorizedstatus
	 * @generated
	 */
	EClass getauthorizedstatus();

	/**
	 * Returns the meta object for the attribute '{@link UMLsec.authorizedstatus#getPermission <em>Permission</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Permission</em>'.
	 * @see UMLsec.authorizedstatus#getPermission()
	 * @see #getauthorizedstatus()
	 * @generated
	 */
	EAttribute getauthorizedstatus_Permission();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.authorizedstatus#getBase_State <em>Base State</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base State</em>'.
	 * @see UMLsec.authorizedstatus#getBase_State()
	 * @see #getauthorizedstatus()
	 * @generated
	 */
	EReference getauthorizedstatus_Base_State();

	/**
	 * Returns the meta object for class '{@link UMLsec.lockedstatus <em>lockedstatus</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>lockedstatus</em>'.
	 * @see UMLsec.lockedstatus
	 * @generated
	 */
	EClass getlockedstatus();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.lockedstatus#getBase_State <em>Base State</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base State</em>'.
	 * @see UMLsec.lockedstatus#getBase_State()
	 * @see #getlockedstatus()
	 * @generated
	 */
	EReference getlockedstatus_Base_State();

	/**
	 * Returns the meta object for class '{@link UMLsec.requires <em>requires</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>requires</em>'.
	 * @see UMLsec.requires
	 * @generated
	 */
	EClass getrequires();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.requires#getBase_Action <em>Base Action</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Action</em>'.
	 * @see UMLsec.requires#getBase_Action()
	 * @see #getrequires()
	 * @generated
	 */
	EReference getrequires_Base_Action();

	/**
	 * Returns the meta object for the reference list '{@link UMLsec.requires#getActions <em>Actions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference list '<em>Actions</em>'.
	 * @see UMLsec.requires#getActions()
	 * @see #getrequires()
	 * @generated
	 */
	EReference getrequires_Actions();

	/**
	 * Returns the meta object for class '{@link UMLsec.privacy <em>privacy</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>privacy</em>'.
	 * @see UMLsec.privacy
	 * @generated
	 */
	EClass getprivacy();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.privacy#getBase_Dependency <em>Base Dependency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Dependency</em>'.
	 * @see UMLsec.privacy#getBase_Dependency()
	 * @see #getprivacy()
	 * @generated
	 */
	EReference getprivacy_Base_Dependency();

	/**
	 * Returns the meta object for the reference '{@link UMLsec.privacy#getBase_Connector <em>Base Connector</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Connector</em>'.
	 * @see UMLsec.privacy#getBase_Connector()
	 * @see #getprivacy()
	 * @generated
	 */
	EReference getprivacy_Base_Connector();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	UMLsecFactory getUMLsecFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each operation of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link UMLsec.impl.protectedactionImpl <em>protectedaction</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.protectedactionImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getprotectedaction()
		 * @generated
		 */
		EClass PROTECTEDACTION = eINSTANCE.getprotectedaction();

		/**
		 * The meta object literal for the '<em><b>Permission</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PROTECTEDACTION__PERMISSION = eINSTANCE.getprotectedaction_Permission();

		/**
		 * The meta object literal for the '<em><b>Base Action</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PROTECTEDACTION__BASE_ACTION = eINSTANCE.getprotectedaction_Base_Action();

		/**
		 * The meta object literal for the '<em><b>Base State</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PROTECTEDACTION__BASE_STATE = eINSTANCE.getprotectedaction_Base_State();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.rbacImpl <em>rbac</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.rbacImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getrbac()
		 * @generated
		 */
		EClass RBAC = eINSTANCE.getrbac();

		/**
		 * The meta object literal for the '<em><b>Protectedactions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RBAC__PROTECTEDACTIONS = eINSTANCE.getrbac_Protectedactions();

		/**
		 * The meta object literal for the '<em><b>Role</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RBAC__ROLE = eINSTANCE.getrbac_Role();

		/**
		 * The meta object literal for the '<em><b>Right</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute RBAC__RIGHT = eINSTANCE.getrbac_Right();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference RBAC__BASE_PACKAGE = eINSTANCE.getrbac_Base_Package();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.SAPTransactionImpl <em>SAP Transaction</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.SAPTransactionImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getSAPTransaction()
		 * @generated
		 */
		EClass SAP_TRANSACTION = eINSTANCE.getSAPTransaction();

		/**
		 * The meta object literal for the '<em><b>Id</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SAP_TRANSACTION__ID = eINSTANCE.getSAPTransaction_Id();

		/**
		 * The meta object literal for the '<em><b>Base Action</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SAP_TRANSACTION__BASE_ACTION = eINSTANCE.getSAPTransaction_Base_Action();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.allowedusersImpl <em>allowedusers</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.allowedusersImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getallowedusers()
		 * @generated
		 */
		EClass ALLOWEDUSERS = eINSTANCE.getallowedusers();

		/**
		 * The meta object literal for the '<em><b>Base Action</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ALLOWEDUSERS__BASE_ACTION = eINSTANCE.getallowedusers_Base_Action();

		/**
		 * The meta object literal for the '<em><b>Users</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ALLOWEDUSERS__USERS = eINSTANCE.getallowedusers_Users();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.seperationofdutyImpl <em>seperationofduty</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.seperationofdutyImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getseperationofduty()
		 * @generated
		 */
		EClass SEPERATIONOFDUTY = eINSTANCE.getseperationofduty();

		/**
		 * The meta object literal for the '<em><b>Base Action</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SEPERATIONOFDUTY__BASE_ACTION = eINSTANCE.getseperationofduty_Base_Action();

		/**
		 * The meta object literal for the '<em><b>Activity</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SEPERATIONOFDUTY__ACTIVITY = eINSTANCE.getseperationofduty_Activity();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.usedbyImpl <em>usedby</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.usedbyImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getusedby()
		 * @generated
		 */
		EClass USEDBY = eINSTANCE.getusedby();

		/**
		 * The meta object literal for the '<em><b>User</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute USEDBY__USER = eINSTANCE.getusedby_User();

		/**
		 * The meta object literal for the '<em><b>Base Action</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference USEDBY__BASE_ACTION = eINSTANCE.getusedby_Base_Action();

		/**
		 * The meta object literal for the '<em><b>Base State</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference USEDBY__BASE_STATE = eINSTANCE.getusedby_Base_State();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.identifiableImpl <em>identifiable</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.identifiableImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getidentifiable()
		 * @generated
		 */
		EClass IDENTIFIABLE = eINSTANCE.getidentifiable();

		/**
		 * The meta object literal for the '<em><b>Id</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute IDENTIFIABLE__ID = eINSTANCE.getidentifiable_Id();

		/**
		 * The meta object literal for the '<em><b>Base Element</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference IDENTIFIABLE__BASE_ELEMENT = eINSTANCE.getidentifiable_Base_Element();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.securedependencyImpl <em>securedependency</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.securedependencyImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getsecuredependency()
		 * @generated
		 */
		EClass SECUREDEPENDENCY = eINSTANCE.getsecuredependency();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SECUREDEPENDENCY__BASE_PACKAGE = eINSTANCE.getsecuredependency_Base_Package();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.callImpl <em>call</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.callImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getcall()
		 * @generated
		 */
		EClass CALL = eINSTANCE.getcall();

		/**
		 * The meta object literal for the '<em><b>Base Dependency</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CALL__BASE_DEPENDENCY = eINSTANCE.getcall_Base_Dependency();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.sendImpl <em>send</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.sendImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getsend()
		 * @generated
		 */
		EClass SEND = eINSTANCE.getsend();

		/**
		 * The meta object literal for the '<em><b>Base Dependency</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SEND__BASE_DEPENDENCY = eINSTANCE.getsend_Base_Dependency();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.secrecyImpl <em>secrecy</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.secrecyImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getsecrecy()
		 * @generated
		 */
		EClass SECRECY = eINSTANCE.getsecrecy();

		/**
		 * The meta object literal for the '<em><b>Base Dependency</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SECRECY__BASE_DEPENDENCY = eINSTANCE.getsecrecy_Base_Dependency();

		/**
		 * The meta object literal for the '<em><b>Base Connector</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SECRECY__BASE_CONNECTOR = eINSTANCE.getsecrecy_Base_Connector();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.integrityImpl <em>integrity</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.integrityImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getintegrity()
		 * @generated
		 */
		EClass INTEGRITY = eINSTANCE.getintegrity();

		/**
		 * The meta object literal for the '<em><b>Base Dependency</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference INTEGRITY__BASE_DEPENDENCY = eINSTANCE.getintegrity_Base_Dependency();

		/**
		 * The meta object literal for the '<em><b>Base Connector</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference INTEGRITY__BASE_CONNECTOR = eINSTANCE.getintegrity_Base_Connector();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.highImpl <em>high</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.highImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#gethigh()
		 * @generated
		 */
		EClass HIGH = eINSTANCE.gethigh();

		/**
		 * The meta object literal for the '<em><b>Base Dependency</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference HIGH__BASE_DEPENDENCY = eINSTANCE.gethigh_Base_Dependency();

		/**
		 * The meta object literal for the '<em><b>Base Connector</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference HIGH__BASE_CONNECTOR = eINSTANCE.gethigh_Base_Connector();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.encryptedImpl <em>encrypted</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.encryptedImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getencrypted()
		 * @generated
		 */
		EClass ENCRYPTED = eINSTANCE.getencrypted();

		/**
		 * The meta object literal for the '<em><b>Base Communication Path</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ENCRYPTED__BASE_COMMUNICATION_PATH = eINSTANCE.getencrypted_Base_CommunicationPath();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.wireImpl <em>wire</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.wireImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getwire()
		 * @generated
		 */
		EClass WIRE = eINSTANCE.getwire();

		/**
		 * The meta object literal for the '<em><b>Base Communication Path</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference WIRE__BASE_COMMUNICATION_PATH = eINSTANCE.getwire_Base_CommunicationPath();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.InternetImpl <em>Internet</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.InternetImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getInternet()
		 * @generated
		 */
		EClass INTERNET = eINSTANCE.getInternet();

		/**
		 * The meta object literal for the '<em><b>Base Communication Path</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference INTERNET__BASE_COMMUNICATION_PATH = eINSTANCE.getInternet_Base_CommunicationPath();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.LANImpl <em>LAN</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.LANImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getLAN()
		 * @generated
		 */
		EClass LAN = eINSTANCE.getLAN();

		/**
		 * The meta object literal for the '<em><b>Base Communication Path</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference LAN__BASE_COMMUNICATION_PATH = eINSTANCE.getLAN_Base_CommunicationPath();

		/**
		 * The meta object literal for the '<em><b>Base Node</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference LAN__BASE_NODE = eINSTANCE.getLAN_Base_Node();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.smartcardImpl <em>smartcard</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.smartcardImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getsmartcard()
		 * @generated
		 */
		EClass SMARTCARD = eINSTANCE.getsmartcard();

		/**
		 * The meta object literal for the '<em><b>Base Node</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SMARTCARD__BASE_NODE = eINSTANCE.getsmartcard_Base_Node();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.POSdeviceImpl <em>PO Sdevice</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.POSdeviceImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getPOSdevice()
		 * @generated
		 */
		EClass PO_SDEVICE = eINSTANCE.getPOSdevice();

		/**
		 * The meta object literal for the '<em><b>Base Node</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PO_SDEVICE__BASE_NODE = eINSTANCE.getPOSdevice_Base_Node();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.issuernodeImpl <em>issuernode</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.issuernodeImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getissuernode()
		 * @generated
		 */
		EClass ISSUERNODE = eINSTANCE.getissuernode();

		/**
		 * The meta object literal for the '<em><b>Base Node</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ISSUERNODE__BASE_NODE = eINSTANCE.getissuernode_Base_Node();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.criticalImpl <em>critical</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.criticalImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getcritical()
		 * @generated
		 */
		EClass CRITICAL = eINSTANCE.getcritical();

		/**
		 * The meta object literal for the '<em><b>Secrecy</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CRITICAL__SECRECY = eINSTANCE.getcritical_Secrecy();

		/**
		 * The meta object literal for the '<em><b>Integrity</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CRITICAL__INTEGRITY = eINSTANCE.getcritical_Integrity();

		/**
		 * The meta object literal for the '<em><b>High</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CRITICAL__HIGH = eINSTANCE.getcritical_High();

		/**
		 * The meta object literal for the '<em><b>Base Class</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CRITICAL__BASE_CLASS = eINSTANCE.getcritical_Base_Class();

		/**
		 * The meta object literal for the '<em><b>Base Component</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CRITICAL__BASE_COMPONENT = eINSTANCE.getcritical_Base_Component();

		/**
		 * The meta object literal for the '<em><b>Fresh</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CRITICAL__FRESH = eINSTANCE.getcritical_Fresh();

		/**
		 * The meta object literal for the '<em><b>Authenticity</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CRITICAL__AUTHENTICITY = eINSTANCE.getcritical_Authenticity();

		/**
		 * The meta object literal for the '<em><b>Base Instance Specification</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CRITICAL__BASE_INSTANCE_SPECIFICATION = eINSTANCE.getcritical_Base_InstanceSpecification();

		/**
		 * The meta object literal for the '<em><b>Base Classifier</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CRITICAL__BASE_CLASSIFIER = eINSTANCE.getcritical_Base_Classifier();

		/**
		 * The meta object literal for the '<em><b>Privacy</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CRITICAL__PRIVACY = eINSTANCE.getcritical_Privacy();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.datasecurityImpl <em>datasecurity</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.datasecurityImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getdatasecurity()
		 * @generated
		 */
		EClass DATASECURITY = eINSTANCE.getdatasecurity();

		/**
		 * The meta object literal for the '<em><b>Adversary</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DATASECURITY__ADVERSARY = eINSTANCE.getdatasecurity_Adversary();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATASECURITY__BASE_PACKAGE = eINSTANCE.getdatasecurity_Base_Package();

		/**
		 * The meta object literal for the '<em><b>Authenticity</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DATASECURITY__AUTHENTICITY = eINSTANCE.getdatasecurity_Authenticity();

		/**
		 * The meta object literal for the '<em><b>Integrity</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DATASECURITY__INTEGRITY = eINSTANCE.getdatasecurity_Integrity();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.securelinksImpl <em>securelinks</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.securelinksImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getsecurelinks()
		 * @generated
		 */
		EClass SECURELINKS = eINSTANCE.getsecurelinks();

		/**
		 * The meta object literal for the '<em><b>Adversary</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURELINKS__ADVERSARY = eINSTANCE.getsecurelinks_Adversary();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SECURELINKS__BASE_PACKAGE = eINSTANCE.getsecurelinks_Base_Package();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.fairexchangeImpl <em>fairexchange</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.fairexchangeImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getfairexchange()
		 * @generated
		 */
		EClass FAIREXCHANGE = eINSTANCE.getfairexchange();

		/**
		 * The meta object literal for the '<em><b>Start</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference FAIREXCHANGE__START = eINSTANCE.getfairexchange_Start();

		/**
		 * The meta object literal for the '<em><b>Stop</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference FAIREXCHANGE__STOP = eINSTANCE.getfairexchange_Stop();

		/**
		 * The meta object literal for the '<em><b>Adversary</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute FAIREXCHANGE__ADVERSARY = eINSTANCE.getfairexchange_Adversary();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference FAIREXCHANGE__BASE_PACKAGE = eINSTANCE.getfairexchange_Base_Package();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.provableImpl <em>provable</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.provableImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getprovable()
		 * @generated
		 */
		EClass PROVABLE = eINSTANCE.getprovable();

		/**
		 * The meta object literal for the '<em><b>Action</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PROVABLE__ACTION = eINSTANCE.getprovable_Action();

		/**
		 * The meta object literal for the '<em><b>Adversary</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PROVABLE__ADVERSARY = eINSTANCE.getprovable_Adversary();

		/**
		 * The meta object literal for the '<em><b>Cert</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute PROVABLE__CERT = eINSTANCE.getprovable_Cert();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PROVABLE__BASE_PACKAGE = eINSTANCE.getprovable_Base_Package();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.nodownflowImpl <em>nodownflow</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.nodownflowImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getnodownflow()
		 * @generated
		 */
		EClass NODOWNFLOW = eINSTANCE.getnodownflow();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference NODOWNFLOW__BASE_PACKAGE = eINSTANCE.getnodownflow_Base_Package();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.noupflowImpl <em>noupflow</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.noupflowImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getnoupflow()
		 * @generated
		 */
		EClass NOUPFLOW = eINSTANCE.getnoupflow();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference NOUPFLOW__BASE_PACKAGE = eINSTANCE.getnoupflow_Base_Package();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.guardedaccessImpl <em>guardedaccess</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.guardedaccessImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getguardedaccess()
		 * @generated
		 */
		EClass GUARDEDACCESS = eINSTANCE.getguardedaccess();

		/**
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference GUARDEDACCESS__BASE_PACKAGE = eINSTANCE.getguardedaccess_Base_Package();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.guardedImpl <em>guarded</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.guardedImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getguarded()
		 * @generated
		 */
		EClass GUARDED = eINSTANCE.getguarded();

		/**
		 * The meta object literal for the '<em><b>Base Classifier</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference GUARDED__BASE_CLASSIFIER = eINSTANCE.getguarded_Base_Classifier();

		/**
		 * The meta object literal for the '<em><b>Base Instance Specification</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference GUARDED__BASE_INSTANCE_SPECIFICATION = eINSTANCE.getguarded_Base_InstanceSpecification();

		/**
		 * The meta object literal for the '<em><b>Guard</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute GUARDED__GUARD = eINSTANCE.getguarded_Guard();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.authorizedstatusImpl <em>authorizedstatus</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.authorizedstatusImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getauthorizedstatus()
		 * @generated
		 */
		EClass AUTHORIZEDSTATUS = eINSTANCE.getauthorizedstatus();

		/**
		 * The meta object literal for the '<em><b>Permission</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute AUTHORIZEDSTATUS__PERMISSION = eINSTANCE.getauthorizedstatus_Permission();

		/**
		 * The meta object literal for the '<em><b>Base State</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference AUTHORIZEDSTATUS__BASE_STATE = eINSTANCE.getauthorizedstatus_Base_State();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.lockedstatusImpl <em>lockedstatus</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.lockedstatusImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getlockedstatus()
		 * @generated
		 */
		EClass LOCKEDSTATUS = eINSTANCE.getlockedstatus();

		/**
		 * The meta object literal for the '<em><b>Base State</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference LOCKEDSTATUS__BASE_STATE = eINSTANCE.getlockedstatus_Base_State();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.requiresImpl <em>requires</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.requiresImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getrequires()
		 * @generated
		 */
		EClass REQUIRES = eINSTANCE.getrequires();

		/**
		 * The meta object literal for the '<em><b>Base Action</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference REQUIRES__BASE_ACTION = eINSTANCE.getrequires_Base_Action();

		/**
		 * The meta object literal for the '<em><b>Actions</b></em>' reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference REQUIRES__ACTIONS = eINSTANCE.getrequires_Actions();

		/**
		 * The meta object literal for the '{@link UMLsec.impl.privacyImpl <em>privacy</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsec.impl.privacyImpl
		 * @see UMLsec.impl.UMLsecPackageImpl#getprivacy()
		 * @generated
		 */
		EClass PRIVACY = eINSTANCE.getprivacy();

		/**
		 * The meta object literal for the '<em><b>Base Dependency</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PRIVACY__BASE_DEPENDENCY = eINSTANCE.getprivacy_Base_Dependency();

		/**
		 * The meta object literal for the '<em><b>Base Connector</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PRIVACY__BASE_CONNECTOR = eINSTANCE.getprivacy_Base_Connector();

	}

} //UMLsecPackage
