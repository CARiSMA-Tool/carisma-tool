/**
 */
package UMLsecenc;

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
 * @see UMLsecenc.UMLsecencFactory
 * @model kind="package"
 * @generated
 */
public interface UMLsecencPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "UMLsecenc";

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
	UMLsecencPackage eINSTANCE = UMLsecenc.impl.UMLsecencPackageImpl.init();

	/**
	 * The meta object id for the '{@link UMLsecenc.impl.securelinksencImpl <em>securelinksenc</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsecenc.impl.securelinksencImpl
	 * @see UMLsecenc.impl.UMLsecencPackageImpl#getsecurelinksenc()
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
	int SECURELINKSENC__ADVERSARY = 0;

	/**
	 * The feature id for the '<em><b>Enc Adversary</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC__ENC_ADVERSARY = 1;

	/**
	 * The feature id for the '<em><b>Enc Model</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC__ENC_MODEL = 2;

	/**
	 * The feature id for the '<em><b>Base Package</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC__BASE_PACKAGE = 3;

	/**
	 * The number of structural features of the '<em>securelinksenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC_FEATURE_COUNT = 4;

	/**
	 * The number of operations of the '<em>securelinksenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECURELINKSENC_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsecenc.impl.secrecyencImpl <em>secrecyenc</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsecenc.impl.secrecyencImpl
	 * @see UMLsecenc.impl.UMLsecencPackageImpl#getsecrecyenc()
	 * @generated
	 */
	int SECRECYENC = 1;

	/**
	 * The feature id for the '<em><b>Time</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECYENC__TIME = 0;

	/**
	 * The feature id for the '<em><b>Base Dependency</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECYENC__BASE_DEPENDENCY = 1;

	/**
	 * The number of structural features of the '<em>secrecyenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECYENC_FEATURE_COUNT = 2;

	/**
	 * The number of operations of the '<em>secrecyenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int SECRECYENC_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link UMLsecenc.impl.encryptedencImpl <em>encryptedenc</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsecenc.impl.encryptedencImpl
	 * @see UMLsecenc.impl.UMLsecencPackageImpl#getencryptedenc()
	 * @generated
	 */
	int ENCRYPTEDENC = 2;

	/**
	 * The feature id for the '<em><b>Alg</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDENC__ALG = 0;

	/**
	 * The feature id for the '<em><b>Keylength</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDENC__KEYLENGTH = 1;

	/**
	 * The feature id for the '<em><b>Base Communication Path</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDENC__BASE_COMMUNICATION_PATH = 2;

	/**
	 * The number of structural features of the '<em>encryptedenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDENC_FEATURE_COUNT = 3;

	/**
	 * The number of operations of the '<em>encryptedenc</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ENCRYPTEDENC_OPERATION_COUNT = 0;


	/**
	 * The meta object id for the '{@link UMLsecenc.impl.encryptedpersistenceImpl <em>encryptedpersistence</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see UMLsecenc.impl.encryptedpersistenceImpl
	 * @see UMLsecenc.impl.UMLsecencPackageImpl#getencryptedpersistence()
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
	 * Returns the meta object for class '{@link UMLsecenc.securelinksenc <em>securelinksenc</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>securelinksenc</em>'.
	 * @see UMLsecenc.securelinksenc
	 * @generated
	 */
	EClass getsecurelinksenc();

	/**
	 * Returns the meta object for the attribute '{@link UMLsecenc.securelinksenc#getAdversary <em>Adversary</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Adversary</em>'.
	 * @see UMLsecenc.securelinksenc#getAdversary()
	 * @see #getsecurelinksenc()
	 * @generated
	 */
	EAttribute getsecurelinksenc_Adversary();

	/**
	 * Returns the meta object for the attribute '{@link UMLsecenc.securelinksenc#getEncAdversary <em>Enc Adversary</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Enc Adversary</em>'.
	 * @see UMLsecenc.securelinksenc#getEncAdversary()
	 * @see #getsecurelinksenc()
	 * @generated
	 */
	EAttribute getsecurelinksenc_EncAdversary();

	/**
	 * Returns the meta object for the attribute '{@link UMLsecenc.securelinksenc#getEncModel <em>Enc Model</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Enc Model</em>'.
	 * @see UMLsecenc.securelinksenc#getEncModel()
	 * @see #getsecurelinksenc()
	 * @generated
	 */
	EAttribute getsecurelinksenc_EncModel();

	/**
	 * Returns the meta object for the reference '{@link UMLsecenc.securelinksenc#getBase_Package <em>Base Package</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Package</em>'.
	 * @see UMLsecenc.securelinksenc#getBase_Package()
	 * @see #getsecurelinksenc()
	 * @generated
	 */
	EReference getsecurelinksenc_Base_Package();

	/**
	 * Returns the meta object for class '{@link UMLsecenc.secrecyenc <em>secrecyenc</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>secrecyenc</em>'.
	 * @see UMLsecenc.secrecyenc
	 * @generated
	 */
	EClass getsecrecyenc();

	/**
	 * Returns the meta object for the attribute '{@link UMLsecenc.secrecyenc#getTime <em>Time</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Time</em>'.
	 * @see UMLsecenc.secrecyenc#getTime()
	 * @see #getsecrecyenc()
	 * @generated
	 */
	EAttribute getsecrecyenc_Time();

	/**
	 * Returns the meta object for the reference '{@link UMLsecenc.secrecyenc#getBase_Dependency <em>Base Dependency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Dependency</em>'.
	 * @see UMLsecenc.secrecyenc#getBase_Dependency()
	 * @see #getsecrecyenc()
	 * @generated
	 */
	EReference getsecrecyenc_Base_Dependency();

	/**
	 * Returns the meta object for class '{@link UMLsecenc.encryptedenc <em>encryptedenc</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>encryptedenc</em>'.
	 * @see UMLsecenc.encryptedenc
	 * @generated
	 */
	EClass getencryptedenc();

	/**
	 * Returns the meta object for the attribute '{@link UMLsecenc.encryptedenc#getAlg <em>Alg</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Alg</em>'.
	 * @see UMLsecenc.encryptedenc#getAlg()
	 * @see #getencryptedenc()
	 * @generated
	 */
	EAttribute getencryptedenc_Alg();

	/**
	 * Returns the meta object for the attribute '{@link UMLsecenc.encryptedenc#getKeylength <em>Keylength</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Keylength</em>'.
	 * @see UMLsecenc.encryptedenc#getKeylength()
	 * @see #getencryptedenc()
	 * @generated
	 */
	EAttribute getencryptedenc_Keylength();

	/**
	 * Returns the meta object for the reference '{@link UMLsecenc.encryptedenc#getBase_CommunicationPath <em>Base Communication Path</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Communication Path</em>'.
	 * @see UMLsecenc.encryptedenc#getBase_CommunicationPath()
	 * @see #getencryptedenc()
	 * @generated
	 */
	EReference getencryptedenc_Base_CommunicationPath();

	/**
	 * Returns the meta object for class '{@link UMLsecenc.encryptedpersistence <em>encryptedpersistence</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>encryptedpersistence</em>'.
	 * @see UMLsecenc.encryptedpersistence
	 * @generated
	 */
	EClass getencryptedpersistence();

	/**
	 * Returns the meta object for the reference '{@link UMLsecenc.encryptedpersistence#getBase_Class <em>Base Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Class</em>'.
	 * @see UMLsecenc.encryptedpersistence#getBase_Class()
	 * @see #getencryptedpersistence()
	 * @generated
	 */
	EReference getencryptedpersistence_Base_Class();

	/**
	 * Returns the meta object for the attribute '{@link UMLsecenc.encryptedpersistence#getAlg <em>Alg</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Alg</em>'.
	 * @see UMLsecenc.encryptedpersistence#getAlg()
	 * @see #getencryptedpersistence()
	 * @generated
	 */
	EAttribute getencryptedpersistence_Alg();

	/**
	 * Returns the meta object for the attribute '{@link UMLsecenc.encryptedpersistence#getKeylength <em>Keylength</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Keylength</em>'.
	 * @see UMLsecenc.encryptedpersistence#getKeylength()
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
	UMLsecencFactory getUMLsecencFactory();

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
		 * The meta object literal for the '{@link UMLsecenc.impl.securelinksencImpl <em>securelinksenc</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsecenc.impl.securelinksencImpl
		 * @see UMLsecenc.impl.UMLsecencPackageImpl#getsecurelinksenc()
		 * @generated
		 */
		EClass SECURELINKSENC = eINSTANCE.getsecurelinksenc();

		/**
		 * The meta object literal for the '<em><b>Adversary</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute SECURELINKSENC__ADVERSARY = eINSTANCE.getsecurelinksenc_Adversary();

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
		 * The meta object literal for the '<em><b>Base Package</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SECURELINKSENC__BASE_PACKAGE = eINSTANCE.getsecurelinksenc_Base_Package();

		/**
		 * The meta object literal for the '{@link UMLsecenc.impl.secrecyencImpl <em>secrecyenc</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsecenc.impl.secrecyencImpl
		 * @see UMLsecenc.impl.UMLsecencPackageImpl#getsecrecyenc()
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
		 * The meta object literal for the '<em><b>Base Dependency</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference SECRECYENC__BASE_DEPENDENCY = eINSTANCE.getsecrecyenc_Base_Dependency();

		/**
		 * The meta object literal for the '{@link UMLsecenc.impl.encryptedencImpl <em>encryptedenc</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsecenc.impl.encryptedencImpl
		 * @see UMLsecenc.impl.UMLsecencPackageImpl#getencryptedenc()
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
		 * The meta object literal for the '<em><b>Base Communication Path</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ENCRYPTEDENC__BASE_COMMUNICATION_PATH = eINSTANCE.getencryptedenc_Base_CommunicationPath();

		/**
		 * The meta object literal for the '{@link UMLsecenc.impl.encryptedpersistenceImpl <em>encryptedpersistence</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see UMLsecenc.impl.encryptedpersistenceImpl
		 * @see UMLsecenc.impl.UMLsecencPackageImpl#getencryptedpersistence()
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

} //UMLsecencPackage
