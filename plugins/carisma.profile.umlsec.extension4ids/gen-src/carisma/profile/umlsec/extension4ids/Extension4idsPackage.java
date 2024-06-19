/**
 */
package carisma.profile.umlsec.extension4ids;

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
 * @see carisma.profile.umlsec.extension4ids.Extension4idsFactory
 * @model kind="package"
 * @generated
 */
public interface Extension4idsPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "extension4ids";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.umlsec.de/profiles/UMLsec/extension4ids";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "extension4ids";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	Extension4idsPackage eINSTANCE = carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl.init();

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.extension4ids.impl.IDSconnectorImpl <em>ID Sconnector</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.extension4ids.impl.IDSconnectorImpl
	 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getIDSconnector()
	 * @generated
	 */
	int ID_SCONNECTOR = 0;

	/**
	 * The feature id for the '<em><b>Base Artifact</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ID_SCONNECTOR__BASE_ARTIFACT = 0;

	/**
	 * The number of structural features of the '<em>ID Sconnector</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ID_SCONNECTOR_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>ID Sconnector</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int ID_SCONNECTOR_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.extension4ids.impl.usagecontrolImpl <em>usagecontrol</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.extension4ids.impl.usagecontrolImpl
	 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getusagecontrol()
	 * @generated
	 */
	int USAGECONTROL = 1;

	/**
	 * The feature id for the '<em><b>Base Dependency</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USAGECONTROL__BASE_DEPENDENCY = 0;

	/**
	 * The number of structural features of the '<em>usagecontrol</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USAGECONTROL_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>usagecontrol</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USAGECONTROL_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.extension4ids.impl.datatransferImpl <em>datatransfer</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.extension4ids.impl.datatransferImpl
	 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getdatatransfer()
	 * @generated
	 */
	int DATATRANSFER = 2;

	/**
	 * The feature id for the '<em><b>Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATRANSFER__TYPE = 0;

	/**
	 * The feature id for the '<em><b>Transfer req step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATRANSFER__TRANSFER_REQ_STEP = 1;

	/**
	 * The feature id for the '<em><b>Transfer start step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATRANSFER__TRANSFER_START_STEP = 2;

	/**
	 * The feature id for the '<em><b>Push pull step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATRANSFER__PUSH_PULL_STEP = 3;

	/**
	 * The feature id for the '<em><b>Transfer complete step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATRANSFER__TRANSFER_COMPLETE_STEP = 4;

	/**
	 * The feature id for the '<em><b>Transfer suspend step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATRANSFER__TRANSFER_SUSPEND_STEP = 5;

	/**
	 * The feature id for the '<em><b>Transfer terminate step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATRANSFER__TRANSFER_TERMINATE_STEP = 6;

	/**
	 * The feature id for the '<em><b>Base Interaction</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATRANSFER__BASE_INTERACTION = 7;

	/**
	 * The number of structural features of the '<em>datatransfer</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATRANSFER_FEATURE_COUNT = 8;

	/**
	 * The number of operations of the '<em>datatransfer</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int DATATRANSFER_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.extension4ids.impl.providerImpl <em>provider</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.extension4ids.impl.providerImpl
	 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getprovider()
	 * @generated
	 */
	int PROVIDER = 3;

	/**
	 * The feature id for the '<em><b>Base Lifeline</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVIDER__BASE_LIFELINE = 0;

	/**
	 * The number of structural features of the '<em>provider</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVIDER_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>provider</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVIDER_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.extension4ids.impl.consumerImpl <em>consumer</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.extension4ids.impl.consumerImpl
	 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getconsumer()
	 * @generated
	 */
	int CONSUMER = 4;

	/**
	 * The feature id for the '<em><b>Base Lifeline</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSUMER__BASE_LIFELINE = 0;

	/**
	 * The number of structural features of the '<em>consumer</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSUMER_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>consumer</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSUMER_OPERATION_COUNT = 0;


	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.extension4ids.IDSconnector <em>ID Sconnector</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>ID Sconnector</em>'.
	 * @see carisma.profile.umlsec.extension4ids.IDSconnector
	 * @generated
	 */
	EClass getIDSconnector();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.IDSconnector#getBase_Artifact <em>Base Artifact</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Artifact</em>'.
	 * @see carisma.profile.umlsec.extension4ids.IDSconnector#getBase_Artifact()
	 * @see #getIDSconnector()
	 * @generated
	 */
	EReference getIDSconnector_Base_Artifact();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.extension4ids.usagecontrol <em>usagecontrol</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>usagecontrol</em>'.
	 * @see carisma.profile.umlsec.extension4ids.usagecontrol
	 * @generated
	 */
	EClass getusagecontrol();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.usagecontrol#getBase_Dependency <em>Base Dependency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Dependency</em>'.
	 * @see carisma.profile.umlsec.extension4ids.usagecontrol#getBase_Dependency()
	 * @see #getusagecontrol()
	 * @generated
	 */
	EReference getusagecontrol_Base_Dependency();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.extension4ids.datatransfer <em>datatransfer</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>datatransfer</em>'.
	 * @see carisma.profile.umlsec.extension4ids.datatransfer
	 * @generated
	 */
	EClass getdatatransfer();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.extension4ids.datatransfer#getType <em>Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Type</em>'.
	 * @see carisma.profile.umlsec.extension4ids.datatransfer#getType()
	 * @see #getdatatransfer()
	 * @generated
	 */
	EAttribute getdatatransfer_Type();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_req_step <em>Transfer req step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Transfer req step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_req_step()
	 * @see #getdatatransfer()
	 * @generated
	 */
	EReference getdatatransfer_Transfer_req_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_start_step <em>Transfer start step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Transfer start step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_start_step()
	 * @see #getdatatransfer()
	 * @generated
	 */
	EReference getdatatransfer_Transfer_start_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.datatransfer#getPush_pull_step <em>Push pull step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Push pull step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.datatransfer#getPush_pull_step()
	 * @see #getdatatransfer()
	 * @generated
	 */
	EReference getdatatransfer_Push_pull_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_complete_step <em>Transfer complete step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Transfer complete step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_complete_step()
	 * @see #getdatatransfer()
	 * @generated
	 */
	EReference getdatatransfer_Transfer_complete_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_suspend_step <em>Transfer suspend step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Transfer suspend step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_suspend_step()
	 * @see #getdatatransfer()
	 * @generated
	 */
	EReference getdatatransfer_Transfer_suspend_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_terminate_step <em>Transfer terminate step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Transfer terminate step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.datatransfer#getTransfer_terminate_step()
	 * @see #getdatatransfer()
	 * @generated
	 */
	EReference getdatatransfer_Transfer_terminate_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.datatransfer#getBase_Interaction <em>Base Interaction</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Interaction</em>'.
	 * @see carisma.profile.umlsec.extension4ids.datatransfer#getBase_Interaction()
	 * @see #getdatatransfer()
	 * @generated
	 */
	EReference getdatatransfer_Base_Interaction();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.extension4ids.provider <em>provider</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>provider</em>'.
	 * @see carisma.profile.umlsec.extension4ids.provider
	 * @generated
	 */
	EClass getprovider();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.provider#getBase_Lifeline <em>Base Lifeline</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Lifeline</em>'.
	 * @see carisma.profile.umlsec.extension4ids.provider#getBase_Lifeline()
	 * @see #getprovider()
	 * @generated
	 */
	EReference getprovider_Base_Lifeline();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.extension4ids.consumer <em>consumer</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>consumer</em>'.
	 * @see carisma.profile.umlsec.extension4ids.consumer
	 * @generated
	 */
	EClass getconsumer();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.consumer#getBase_Lifeline <em>Base Lifeline</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Lifeline</em>'.
	 * @see carisma.profile.umlsec.extension4ids.consumer#getBase_Lifeline()
	 * @see #getconsumer()
	 * @generated
	 */
	EReference getconsumer_Base_Lifeline();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	Extension4idsFactory getExtension4idsFactory();

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
		 * The meta object literal for the '{@link carisma.profile.umlsec.extension4ids.impl.IDSconnectorImpl <em>ID Sconnector</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.extension4ids.impl.IDSconnectorImpl
		 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getIDSconnector()
		 * @generated
		 */
		EClass ID_SCONNECTOR = eINSTANCE.getIDSconnector();

		/**
		 * The meta object literal for the '<em><b>Base Artifact</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference ID_SCONNECTOR__BASE_ARTIFACT = eINSTANCE.getIDSconnector_Base_Artifact();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.extension4ids.impl.usagecontrolImpl <em>usagecontrol</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.extension4ids.impl.usagecontrolImpl
		 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getusagecontrol()
		 * @generated
		 */
		EClass USAGECONTROL = eINSTANCE.getusagecontrol();

		/**
		 * The meta object literal for the '<em><b>Base Dependency</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference USAGECONTROL__BASE_DEPENDENCY = eINSTANCE.getusagecontrol_Base_Dependency();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.extension4ids.impl.datatransferImpl <em>datatransfer</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.extension4ids.impl.datatransferImpl
		 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getdatatransfer()
		 * @generated
		 */
		EClass DATATRANSFER = eINSTANCE.getdatatransfer();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute DATATRANSFER__TYPE = eINSTANCE.getdatatransfer_Type();

		/**
		 * The meta object literal for the '<em><b>Transfer req step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATATRANSFER__TRANSFER_REQ_STEP = eINSTANCE.getdatatransfer_Transfer_req_step();

		/**
		 * The meta object literal for the '<em><b>Transfer start step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATATRANSFER__TRANSFER_START_STEP = eINSTANCE.getdatatransfer_Transfer_start_step();

		/**
		 * The meta object literal for the '<em><b>Push pull step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATATRANSFER__PUSH_PULL_STEP = eINSTANCE.getdatatransfer_Push_pull_step();

		/**
		 * The meta object literal for the '<em><b>Transfer complete step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATATRANSFER__TRANSFER_COMPLETE_STEP = eINSTANCE.getdatatransfer_Transfer_complete_step();

		/**
		 * The meta object literal for the '<em><b>Transfer suspend step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATATRANSFER__TRANSFER_SUSPEND_STEP = eINSTANCE.getdatatransfer_Transfer_suspend_step();

		/**
		 * The meta object literal for the '<em><b>Transfer terminate step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATATRANSFER__TRANSFER_TERMINATE_STEP = eINSTANCE.getdatatransfer_Transfer_terminate_step();

		/**
		 * The meta object literal for the '<em><b>Base Interaction</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference DATATRANSFER__BASE_INTERACTION = eINSTANCE.getdatatransfer_Base_Interaction();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.extension4ids.impl.providerImpl <em>provider</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.extension4ids.impl.providerImpl
		 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getprovider()
		 * @generated
		 */
		EClass PROVIDER = eINSTANCE.getprovider();

		/**
		 * The meta object literal for the '<em><b>Base Lifeline</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PROVIDER__BASE_LIFELINE = eINSTANCE.getprovider_Base_Lifeline();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.extension4ids.impl.consumerImpl <em>consumer</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.extension4ids.impl.consumerImpl
		 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getconsumer()
		 * @generated
		 */
		EClass CONSUMER = eINSTANCE.getconsumer();

		/**
		 * The meta object literal for the '<em><b>Base Lifeline</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONSUMER__BASE_LIFELINE = eINSTANCE.getconsumer_Base_Lifeline();

	}

} //Extension4idsPackage
