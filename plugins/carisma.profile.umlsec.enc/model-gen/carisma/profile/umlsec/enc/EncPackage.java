/**
 */
package carisma.profile.umlsec.enc;

import carisma.profile.umlsec.UmlsecPackage;

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
 * @see carisma.profile.umlsec.enc.EncFactory
 * @model kind="package"
 *        annotation="http://www.eclipse.org/uml2/2.0.0/UML originalName='UMLsecenc'"
 * @generated
 */
public interface EncPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "enc";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.umlsec.de/profiles/UMLsec/enc";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "UMLsecenc";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	EncPackage eINSTANCE = carisma.profile.umlsec.enc.impl.EncPackageImpl.init();

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.enc.impl.securelinksencImpl <em>securelinksenc</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.enc.impl.securelinksencImpl
	 * @see carisma.profile.umlsec.enc.impl.EncPackageImpl#getsecurelinksenc()
	 * @generated
	 */
	int SECURELINKSENC = 0;

	/**
	 * The feature id for the '<em><b>Adversary</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC__ADVERSARY = UmlsecPackage.SECURELINKS__ADVERSARY;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC__BASE_PACKAGE = UmlsecPackage.SECURELINKS__BASE_PACKAGE;

	/**
	 * The feature id for the '<em><b>Enc Adversary</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC__ENC_ADVERSARY = UmlsecPackage.SECURELINKS_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Enc Model</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC__ENC_MODEL = UmlsecPackage.SECURELINKS_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>securelinksenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC_FEATURE_COUNT = UmlsecPackage.SECURELINKS_FEATURE_COUNT + 2;

	/**
	 * The number of operations of the '<em>securelinksenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC_OPERATION_COUNT = UmlsecPackage.SECURELINKS_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.enc.impl.secrecyencImpl <em>secrecyenc</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.enc.impl.secrecyencImpl
	 * @see carisma.profile.umlsec.enc.impl.EncPackageImpl#getsecrecyenc()
	 * @generated
	 */
	int SECRECYENC = 1;

	/**
	 * The feature id for the '<em><b>Base Dependency</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECYENC__BASE_DEPENDENCY = UmlsecPackage.SECRECY__BASE_DEPENDENCY;

	/**
	 * The feature id for the '<em><b>Base Connector</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECYENC__BASE_CONNECTOR = UmlsecPackage.SECRECY__BASE_CONNECTOR;

	/**
	 * The feature id for the '<em><b>Time</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECYENC__TIME = UmlsecPackage.SECRECY_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>secrecyenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECYENC_FEATURE_COUNT = UmlsecPackage.SECRECY_FEATURE_COUNT + 1;

	/**
	 * The number of operations of the '<em>secrecyenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECYENC_OPERATION_COUNT = UmlsecPackage.SECRECY_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.enc.impl.encryptedencImpl <em>encryptedenc</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.enc.impl.encryptedencImpl
	 * @see carisma.profile.umlsec.enc.impl.EncPackageImpl#getencryptedenc()
	 * @generated
	 */
	int ENCRYPTEDENC = 2;

	/**
	 * The feature id for the '<em><b>Base Communication Path</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDENC__BASE_COMMUNICATION_PATH = UmlsecPackage.ENCRYPTED__BASE_COMMUNICATION_PATH;

	/**
	 * The feature id for the '<em><b>Alg</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDENC__ALG = UmlsecPackage.ENCRYPTED_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Keylength</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDENC__KEYLENGTH = UmlsecPackage.ENCRYPTED_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>encryptedenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDENC_FEATURE_COUNT = UmlsecPackage.ENCRYPTED_FEATURE_COUNT + 2;

	/**
	 * The number of operations of the '<em>encryptedenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDENC_OPERATION_COUNT = UmlsecPackage.ENCRYPTED_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.enc.impl.encryptedpersistenceImpl <em>encryptedpersistence</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.enc.impl.encryptedpersistenceImpl
	 * @see carisma.profile.umlsec.enc.impl.EncPackageImpl#getencryptedpersistence()
	 * @generated
	 */
	int ENCRYPTEDPERSISTENCE = 3;

	/**
	 * The feature id for the '<em><b>Base Class</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDPERSISTENCE__BASE_CLASS = 0;

	/**
	 * The feature id for the '<em><b>Alg</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDPERSISTENCE__ALG = 1;

	/**
	 * The feature id for the '<em><b>Keylength</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDPERSISTENCE__KEYLENGTH = 2;

	/**
	 * The number of structural features of the '<em>encryptedpersistence</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDPERSISTENCE_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>encryptedpersistence</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDPERSISTENCE_OPERATION_COUNT = 0;


	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.enc.securelinksenc <em>securelinksenc</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>securelinksenc</em>'.
	 * @see carisma.profile.umlsec.enc.securelinksenc
	 * @generated
	 */
	EClass getsecurelinksenc();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.enc.securelinksenc#getEncAdversary <em>Enc Adversary</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Enc Adversary</em>'.
	 * @see carisma.profile.umlsec.enc.securelinksenc#getEncAdversary()
	 * @see #getsecurelinksenc()
	 * @generated
	 */
	EAttribute getsecurelinksenc_EncAdversary();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.enc.securelinksenc#getEncModel <em>Enc Model</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Enc Model</em>'.
	 * @see carisma.profile.umlsec.enc.securelinksenc#getEncModel()
	 * @see #getsecurelinksenc()
	 * @generated
	 */
	EAttribute getsecurelinksenc_EncModel();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.enc.secrecyenc <em>secrecyenc</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>secrecyenc</em>'.
	 * @see carisma.profile.umlsec.enc.secrecyenc
	 * @generated
	 */
	EClass getsecrecyenc();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.enc.secrecyenc#getTime <em>Time</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Time</em>'.
	 * @see carisma.profile.umlsec.enc.secrecyenc#getTime()
	 * @see #getsecrecyenc()
	 * @generated
	 */
	EAttribute getsecrecyenc_Time();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.enc.encryptedenc <em>encryptedenc</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>encryptedenc</em>'.
	 * @see carisma.profile.umlsec.enc.encryptedenc
	 * @generated
	 */
	EClass getencryptedenc();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.enc.encryptedenc#getAlg <em>Alg</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Alg</em>'.
	 * @see carisma.profile.umlsec.enc.encryptedenc#getAlg()
	 * @see #getencryptedenc()
	 * @generated
	 */
	EAttribute getencryptedenc_Alg();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.enc.encryptedenc#getKeylength <em>Keylength</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Keylength</em>'.
	 * @see carisma.profile.umlsec.enc.encryptedenc#getKeylength()
	 * @see #getencryptedenc()
	 * @generated
	 */
	EAttribute getencryptedenc_Keylength();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.enc.encryptedpersistence <em>encryptedpersistence</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>encryptedpersistence</em>'.
	 * @see carisma.profile.umlsec.enc.encryptedpersistence
	 * @generated
	 */
	EClass getencryptedpersistence();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.enc.encryptedpersistence#getBase_Class <em>Base Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Class</em>'.
	 * @see carisma.profile.umlsec.enc.encryptedpersistence#getBase_Class()
	 * @see #getencryptedpersistence()
	 * @generated
	 */
	EReference getencryptedpersistence_Base_Class();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.enc.encryptedpersistence#getAlg <em>Alg</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Alg</em>'.
	 * @see carisma.profile.umlsec.enc.encryptedpersistence#getAlg()
	 * @see #getencryptedpersistence()
	 * @generated
	 */
	EAttribute getencryptedpersistence_Alg();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.enc.encryptedpersistence#getKeylength <em>Keylength</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Keylength</em>'.
	 * @see carisma.profile.umlsec.enc.encryptedpersistence#getKeylength()
	 * @see #getencryptedpersistence()
	 * @generated
	 */
	EAttribute getencryptedpersistence_Keylength();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	EncFactory getEncFactory();

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
		 * The meta object literal for the '{@link carisma.profile.umlsec.enc.impl.securelinksencImpl <em>securelinksenc</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.enc.impl.securelinksencImpl
		 * @see carisma.profile.umlsec.enc.impl.EncPackageImpl#getsecurelinksenc()
		 * @generated
		 */
		EClass SECURELINKSENC = eINSTANCE.getsecurelinksenc();

		/**
		 * The meta object literal for the '<em><b>Enc Adversary</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURELINKSENC__ENC_ADVERSARY = eINSTANCE.getsecurelinksenc_EncAdversary();

		/**
		 * The meta object literal for the '<em><b>Enc Model</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURELINKSENC__ENC_MODEL = eINSTANCE.getsecurelinksenc_EncModel();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.enc.impl.secrecyencImpl <em>secrecyenc</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.enc.impl.secrecyencImpl
		 * @see carisma.profile.umlsec.enc.impl.EncPackageImpl#getsecrecyenc()
		 * @generated
		 */
		EClass SECRECYENC = eINSTANCE.getsecrecyenc();

		/**
		 * The meta object literal for the '<em><b>Time</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECRECYENC__TIME = eINSTANCE.getsecrecyenc_Time();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.enc.impl.encryptedencImpl <em>encryptedenc</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.enc.impl.encryptedencImpl
		 * @see carisma.profile.umlsec.enc.impl.EncPackageImpl#getencryptedenc()
		 * @generated
		 */
		EClass ENCRYPTEDENC = eINSTANCE.getencryptedenc();

		/**
		 * The meta object literal for the '<em><b>Alg</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ENCRYPTEDENC__ALG = eINSTANCE.getencryptedenc_Alg();

		/**
		 * The meta object literal for the '<em><b>Keylength</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ENCRYPTEDENC__KEYLENGTH = eINSTANCE.getencryptedenc_Keylength();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.enc.impl.encryptedpersistenceImpl <em>encryptedpersistence</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.enc.impl.encryptedpersistenceImpl
		 * @see carisma.profile.umlsec.enc.impl.EncPackageImpl#getencryptedpersistence()
		 * @generated
		 */
		EClass ENCRYPTEDPERSISTENCE = eINSTANCE.getencryptedpersistence();

		/**
		 * The meta object literal for the '<em><b>Base Class</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ENCRYPTEDPERSISTENCE__BASE_CLASS = eINSTANCE.getencryptedpersistence_Base_Class();

		/**
		 * The meta object literal for the '<em><b>Alg</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ENCRYPTEDPERSISTENCE__ALG = eINSTANCE.getencryptedpersistence_Alg();

		/**
		 * The meta object literal for the '<em><b>Keylength</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute ENCRYPTEDPERSISTENCE__KEYLENGTH = eINSTANCE.getencryptedpersistence_Keylength();

	}

} //EncPackage
