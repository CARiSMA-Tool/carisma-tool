/**
 */
package carisma.profile.umlsec.extension4ids;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
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
	String eNS_URI = "http:///extension4ids.ecore";

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
	 * The meta object id for the '{@link carisma.profile.umlsec.extension4ids.impl.UsageControlImpl <em>Usage Control</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.extension4ids.impl.UsageControlImpl
	 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getUsageControl()
	 * @generated
	 */
	int USAGE_CONTROL = 1;

	/**
	 * The feature id for the '<em><b>Base Dependency</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USAGE_CONTROL__BASE_DEPENDENCY = 0;

	/**
	 * The number of structural features of the '<em>Usage Control</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USAGE_CONTROL_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Usage Control</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int USAGE_CONTROL_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.extension4ids.impl.TransferProcessProtocolImpl <em>Transfer Process Protocol</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.extension4ids.impl.TransferProcessProtocolImpl
	 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getTransferProcessProtocol()
	 * @generated
	 */
	int TRANSFER_PROCESS_PROTOCOL = 2;

	/**
	 * The feature id for the '<em><b>Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSFER_PROCESS_PROTOCOL__TYPE = 0;

	/**
	 * The feature id for the '<em><b>Transfer req step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSFER_PROCESS_PROTOCOL__TRANSFER_REQ_STEP = 1;

	/**
	 * The feature id for the '<em><b>Transfer start step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSFER_PROCESS_PROTOCOL__TRANSFER_START_STEP = 2;

	/**
	 * The feature id for the '<em><b>Push pull step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSFER_PROCESS_PROTOCOL__PUSH_PULL_STEP = 3;

	/**
	 * The feature id for the '<em><b>Transfer complete step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSFER_PROCESS_PROTOCOL__TRANSFER_COMPLETE_STEP = 4;

	/**
	 * The feature id for the '<em><b>Transfer suspend step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSFER_PROCESS_PROTOCOL__TRANSFER_SUSPEND_STEP = 5;

	/**
	 * The feature id for the '<em><b>Transfer terminate step</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSFER_PROCESS_PROTOCOL__TRANSFER_TERMINATE_STEP = 6;

	/**
	 * The feature id for the '<em><b>Base Interaction</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSFER_PROCESS_PROTOCOL__BASE_INTERACTION = 7;

	/**
	 * The number of structural features of the '<em>Transfer Process Protocol</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSFER_PROCESS_PROTOCOL_FEATURE_COUNT = 8;

	/**
	 * The number of operations of the '<em>Transfer Process Protocol</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TRANSFER_PROCESS_PROTOCOL_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.extension4ids.impl.ProviderConnectorImpl <em>Provider Connector</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.extension4ids.impl.ProviderConnectorImpl
	 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getProviderConnector()
	 * @generated
	 */
	int PROVIDER_CONNECTOR = 3;

	/**
	 * The feature id for the '<em><b>Base Lifeline</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVIDER_CONNECTOR__BASE_LIFELINE = 0;

	/**
	 * The number of structural features of the '<em>Provider Connector</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVIDER_CONNECTOR_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Provider Connector</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int PROVIDER_CONNECTOR_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.extension4ids.impl.ConsumerConnectorImpl <em>Consumer Connector</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.extension4ids.impl.ConsumerConnectorImpl
	 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getConsumerConnector()
	 * @generated
	 */
	int CONSUMER_CONNECTOR = 4;

	/**
	 * The feature id for the '<em><b>Base Lifeline</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSUMER_CONNECTOR__BASE_LIFELINE = 0;

	/**
	 * The number of structural features of the '<em>Consumer Connector</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSUMER_CONNECTOR_FEATURE_COUNT = 1;

	/**
	 * The number of operations of the '<em>Consumer Connector</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CONSUMER_CONNECTOR_OPERATION_COUNT = 0;

	/**
	 * The meta object id for the '{@link carisma.profile.umlsec.extension4ids.TransferType <em>Transfer Type</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.profile.umlsec.extension4ids.TransferType
	 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getTransferType()
	 * @generated
	 */
	int TRANSFER_TYPE = 5;


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
	 * Returns the meta object for class '{@link carisma.profile.umlsec.extension4ids.UsageControl <em>Usage Control</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Usage Control</em>'.
	 * @see carisma.profile.umlsec.extension4ids.UsageControl
	 * @generated
	 */
	EClass getUsageControl();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.UsageControl#getBase_Dependency <em>Base Dependency</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Dependency</em>'.
	 * @see carisma.profile.umlsec.extension4ids.UsageControl#getBase_Dependency()
	 * @see #getUsageControl()
	 * @generated
	 */
	EReference getUsageControl_Base_Dependency();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.extension4ids.TransferProcessProtocol <em>Transfer Process Protocol</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Transfer Process Protocol</em>'.
	 * @see carisma.profile.umlsec.extension4ids.TransferProcessProtocol
	 * @generated
	 */
	EClass getTransferProcessProtocol();

	/**
	 * Returns the meta object for the attribute '{@link carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getType <em>Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Type</em>'.
	 * @see carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getType()
	 * @see #getTransferProcessProtocol()
	 * @generated
	 */
	EAttribute getTransferProcessProtocol_Type();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getTransfer_req_step <em>Transfer req step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Transfer req step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getTransfer_req_step()
	 * @see #getTransferProcessProtocol()
	 * @generated
	 */
	EReference getTransferProcessProtocol_Transfer_req_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getTransfer_start_step <em>Transfer start step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Transfer start step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getTransfer_start_step()
	 * @see #getTransferProcessProtocol()
	 * @generated
	 */
	EReference getTransferProcessProtocol_Transfer_start_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getPush_pull_step <em>Push pull step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Push pull step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getPush_pull_step()
	 * @see #getTransferProcessProtocol()
	 * @generated
	 */
	EReference getTransferProcessProtocol_Push_pull_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getTransfer_complete_step <em>Transfer complete step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Transfer complete step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getTransfer_complete_step()
	 * @see #getTransferProcessProtocol()
	 * @generated
	 */
	EReference getTransferProcessProtocol_Transfer_complete_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getTransfer_suspend_step <em>Transfer suspend step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Transfer suspend step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getTransfer_suspend_step()
	 * @see #getTransferProcessProtocol()
	 * @generated
	 */
	EReference getTransferProcessProtocol_Transfer_suspend_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getTransfer_terminate_step <em>Transfer terminate step</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Transfer terminate step</em>'.
	 * @see carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getTransfer_terminate_step()
	 * @see #getTransferProcessProtocol()
	 * @generated
	 */
	EReference getTransferProcessProtocol_Transfer_terminate_step();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getBase_Interaction <em>Base Interaction</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Interaction</em>'.
	 * @see carisma.profile.umlsec.extension4ids.TransferProcessProtocol#getBase_Interaction()
	 * @see #getTransferProcessProtocol()
	 * @generated
	 */
	EReference getTransferProcessProtocol_Base_Interaction();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.extension4ids.ProviderConnector <em>Provider Connector</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Provider Connector</em>'.
	 * @see carisma.profile.umlsec.extension4ids.ProviderConnector
	 * @generated
	 */
	EClass getProviderConnector();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.ProviderConnector#getBase_Lifeline <em>Base Lifeline</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Lifeline</em>'.
	 * @see carisma.profile.umlsec.extension4ids.ProviderConnector#getBase_Lifeline()
	 * @see #getProviderConnector()
	 * @generated
	 */
	EReference getProviderConnector_Base_Lifeline();

	/**
	 * Returns the meta object for class '{@link carisma.profile.umlsec.extension4ids.ConsumerConnector <em>Consumer Connector</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Consumer Connector</em>'.
	 * @see carisma.profile.umlsec.extension4ids.ConsumerConnector
	 * @generated
	 */
	EClass getConsumerConnector();

	/**
	 * Returns the meta object for the reference '{@link carisma.profile.umlsec.extension4ids.ConsumerConnector#getBase_Lifeline <em>Base Lifeline</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Base Lifeline</em>'.
	 * @see carisma.profile.umlsec.extension4ids.ConsumerConnector#getBase_Lifeline()
	 * @see #getConsumerConnector()
	 * @generated
	 */
	EReference getConsumerConnector_Base_Lifeline();

	/**
	 * Returns the meta object for enum '{@link carisma.profile.umlsec.extension4ids.TransferType <em>Transfer Type</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Transfer Type</em>'.
	 * @see carisma.profile.umlsec.extension4ids.TransferType
	 * @generated
	 */
	EEnum getTransferType();

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
		 * The meta object literal for the '{@link carisma.profile.umlsec.extension4ids.impl.UsageControlImpl <em>Usage Control</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.extension4ids.impl.UsageControlImpl
		 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getUsageControl()
		 * @generated
		 */
		EClass USAGE_CONTROL = eINSTANCE.getUsageControl();

		/**
		 * The meta object literal for the '<em><b>Base Dependency</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference USAGE_CONTROL__BASE_DEPENDENCY = eINSTANCE.getUsageControl_Base_Dependency();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.extension4ids.impl.TransferProcessProtocolImpl <em>Transfer Process Protocol</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.extension4ids.impl.TransferProcessProtocolImpl
		 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getTransferProcessProtocol()
		 * @generated
		 */
		EClass TRANSFER_PROCESS_PROTOCOL = eINSTANCE.getTransferProcessProtocol();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TRANSFER_PROCESS_PROTOCOL__TYPE = eINSTANCE.getTransferProcessProtocol_Type();

		/**
		 * The meta object literal for the '<em><b>Transfer req step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TRANSFER_PROCESS_PROTOCOL__TRANSFER_REQ_STEP = eINSTANCE.getTransferProcessProtocol_Transfer_req_step();

		/**
		 * The meta object literal for the '<em><b>Transfer start step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TRANSFER_PROCESS_PROTOCOL__TRANSFER_START_STEP = eINSTANCE.getTransferProcessProtocol_Transfer_start_step();

		/**
		 * The meta object literal for the '<em><b>Push pull step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TRANSFER_PROCESS_PROTOCOL__PUSH_PULL_STEP = eINSTANCE.getTransferProcessProtocol_Push_pull_step();

		/**
		 * The meta object literal for the '<em><b>Transfer complete step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TRANSFER_PROCESS_PROTOCOL__TRANSFER_COMPLETE_STEP = eINSTANCE.getTransferProcessProtocol_Transfer_complete_step();

		/**
		 * The meta object literal for the '<em><b>Transfer suspend step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TRANSFER_PROCESS_PROTOCOL__TRANSFER_SUSPEND_STEP = eINSTANCE.getTransferProcessProtocol_Transfer_suspend_step();

		/**
		 * The meta object literal for the '<em><b>Transfer terminate step</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TRANSFER_PROCESS_PROTOCOL__TRANSFER_TERMINATE_STEP = eINSTANCE.getTransferProcessProtocol_Transfer_terminate_step();

		/**
		 * The meta object literal for the '<em><b>Base Interaction</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TRANSFER_PROCESS_PROTOCOL__BASE_INTERACTION = eINSTANCE.getTransferProcessProtocol_Base_Interaction();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.extension4ids.impl.ProviderConnectorImpl <em>Provider Connector</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.extension4ids.impl.ProviderConnectorImpl
		 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getProviderConnector()
		 * @generated
		 */
		EClass PROVIDER_CONNECTOR = eINSTANCE.getProviderConnector();

		/**
		 * The meta object literal for the '<em><b>Base Lifeline</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference PROVIDER_CONNECTOR__BASE_LIFELINE = eINSTANCE.getProviderConnector_Base_Lifeline();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.extension4ids.impl.ConsumerConnectorImpl <em>Consumer Connector</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.extension4ids.impl.ConsumerConnectorImpl
		 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getConsumerConnector()
		 * @generated
		 */
		EClass CONSUMER_CONNECTOR = eINSTANCE.getConsumerConnector();

		/**
		 * The meta object literal for the '<em><b>Base Lifeline</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CONSUMER_CONNECTOR__BASE_LIFELINE = eINSTANCE.getConsumerConnector_Base_Lifeline();

		/**
		 * The meta object literal for the '{@link carisma.profile.umlsec.extension4ids.TransferType <em>Transfer Type</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.profile.umlsec.extension4ids.TransferType
		 * @see carisma.profile.umlsec.extension4ids.impl.Extension4idsPackageImpl#getTransferType()
		 * @generated
		 */
		EEnum TRANSFER_TYPE = eINSTANCE.getTransferType();

	}

} //Extension4idsPackage
